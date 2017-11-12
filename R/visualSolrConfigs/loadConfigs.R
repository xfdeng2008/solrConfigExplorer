library(xml2)
library(psych)
library(tidyverse)
library(purrr)
library(stringr)
library(dplyr)


# load configuration xml files
# support a url from a soler server to be Added
load_xml <- function(base_conf_dir, xml_file_name){
  file_path <- file.path(base_conf_dir, xml_file_name)
  return(read_xml(file_path))
}

# ##########################################################################
# code snipet for debuging only: 
# global xml variables: 
# load solrconfig.xml and schema.xml from the solr conf directory
# config_xml <- load_xml(xml_file_name="solrconfig.xml")
# schema_xml <- load_xml(xml_file_name="schema.xml")

# config <- config_xml
# schema <- schema_xml
# ##########################################################################


# get a char vector of important reuqestHandler names
get_requestHandlers <- function(config){
  
  # find important requestHnadlers (SearchHandler or StandardRequestHandler):
  handler_names <- config %>% 
                    xml_find_all(xpath=".//requestHandler[@class='solr.SearchHandler' or @class='solr.StandardRequestHandler']/@name") %>%
                    str_match(pattern="name=\\\"\\/([a-zA-Z]*)\\\"")
  
  return(handler_names[,2] %>% na.omit())
}

# get fields by settings (qf, pf, fl).
# The function returns a tibble of {requestHandlers, settings, fields}
get_fields <- function(handler_name, setting, config){
  my_fields <- xml_find_all(config, paste0(".//requestHandler[@name='/", handler_name, "']/lst[@name='defaults']/str[@name='", setting, "']/text()", collapse=""))
  
  # regular expression for extracting pf anf qf fields:
  regex_pattern <- "[\\^0-9.]+\\s*|\\s"
  
  if(length(xml_type(my_fields)) == 0){
    return(tibble(requestHandlers=handler_name, settings=setting, fields=NA))
  } 
  
  # xml_type: text
  str_fields <- xml_text(my_fields)[1]
  if(setting=="fl"){
    regex_pattern <- ",\\s*|\\s+"
  }
  fields <- str_split(str_fields, regex_pattern)[[1]]
  fields <- fields[fields!=""]
  
  fields_tbl <- tibble(requestHandlers=handler_name, settings=setting, fields=fields)
  return(fields_tbl)
}


# construct the requestHandler-settings-fields table (tibble)
# return a combined tibble of {requestHandlers, settings, fields}
# for all three settings: qf, pf, fl
get_requestHandler_fields_table <- function(config, request_handler_names){
    
  # A list of qf fields for each requestHandler:
  qf_fields<- map(request_handler_names, get_fields, "qf", config)
  
  # A list of pf fields for each requestHandler:
  pf_fields<- map(request_handler_names, get_fields, "pf", config)
  
  # A list of fl fields for each requestHandler:
  fl_fields<- map(request_handler_names, get_fields, "fl", config)
  
  # combine the three lists into a table and sort by requestHandler names asc
  return(bind_rows(qf_fields, pf_fields, fl_fields) %>% arrange(requestHandlers))
}

# get the fieldType of a string field name
# assumes that each field name only has one fieldType
# return a tibble of {fields, fieldTypes}
get_fieldTypes <- function(field_name, schema){
  fieldType_node <- xml_find_first(schema, paste0(".//field[@name='", field_name, "']/@type", collapse=""))
  fieldType_value <- xml_text(fieldType_node)
  return(tibble(fields=field_name, fieldTypes=fieldType_value))
}

# return a table of {fields, fieldTypes}
# field_names: a set (tibble) of field names, duplicate and NAs are allowed
get_field_fieldTypes_table <- function(schema, field_names){  
  
  # ensure the uniqueness of field_names and remove NAs:
  my_field_names <- field_names %>% unique() %>% na.omit()
  
  # A table of fieldTypes for each unique field:
  fieldtypes_tbl <- map(my_field_names, get_fieldTypes, schema) %>% bind_rows() %>% arrange(fields)

  return(fieldtypes_tbl)
}


# This function obtains all analyzer filers/charFilters/tokenizers' with any attribute 
# that contains a .txt file. It returns a tibble:
# {analyzer_types, tokenizer_filters, filter_source, external_files}, or NA if there is no external .txt
#
# if the analyzer_type is set (as a string, didn't check the type), then xpath matches under index/query analyzers
# else xpath matches under a general analyzer node (regardless of its type attribute) 
get_filter_txt_table <- function(schema, fieldtype_name, analyzer_type=NA){

  # find all filter/tokenizer/charFilter class names that contains a ".txt" in any attribute
  # assume one filter/tokenizer/charFilter can only refer to one external .txt file
  if(is.na(analyzer_type)){
    xpath_txt <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer//@*[contains(., '.txt')]", collapse="")
  } else {
    xpath_txt <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer[@type='", analyzer_type,"']//@*[contains(., '.txt')]", collapse="")
  }
  
  external_txts <- schema %>% xml_find_all(xpath_txt)
  
  # verify there exists a filter that contains an external txt file:
  exists_external_txt<- !(length(xml_type(external_txts)) == 0)
  
  if(exists_external_txt){
    parent_nodes <- xml_parent(external_txts)
    source <- as.character(parent_nodes)
    txts <- external_txts %>% xml_text()
    filters_classes_with_txt <- parent_nodes %>% xml_attr(attr="class") 
    filters_with_txt <- str_match(filters_classes_with_txt, pattern = "solr.([a-zA-Z]+)")[,2]
    return(tibble(tokenizer_filters=filters_with_txt, filter_source=source, external_files=txts))
  } else{
    return(NA)
  }
}

# return a tibble of {fieldTypes, class, analyzer_types, tokenizer_filters, filter_source, external_files} for 
# each fieldType
# tokenizers, filters, external file names
get_filters_externalfiles <- function(fieldtype_name, schema){
  # first of all, obtain the Solr class of the fieldType:
  class_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/@class")
  class <- schema %>%
    xml_find_all(class_xpath) %>%
    xml_text()
  
  # needs to verify whether or not the fieldtype has a index/query analyzer:
  # (1) fieldType does not have any analyzer: 
  test_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer") 
  test_node <- xml_type(xml_find_all(schema, test_xpath))
  if(length(test_node) == 0){
    return(tibble(fieldTypes = fieldtype_name, class=class, analyzer_types = NA, tokenizer_filters = NA, external_files = NA))
  }
  
  # (2) fieldType only have one analyzer (regardless of index or query) without any type
  test_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer/@type") 
  test_node <- xml_type(xml_find_all(schema, test_xpath))
  if(length(test_node) == 0){
    xpath_analyzer_filters <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer//@class", collapse="")    
    nodes <- schema %>% xml_find_all(xpath_analyzer_filters)
    source <- as.character(xml_parent(nodes))
    analyzer_filter_classes <- nodes %>% xml_text()
    analyzer_filters <- str_match(analyzer_filter_classes, pattern = "solr.([a-zA-Z]+)")[,2]  
    analyzer_filters_tbl <- tibble(fieldTypes = fieldtype_name, class=class, analyzer_types = "all", tokenizer_filters = analyzer_filters, filter_source=source)
    
    # get the external lists that a filter uses:
    # find all filter/tokenizer/charFilter class names that contains a ".txt" in any attribute
    # assume one filter/tokenizer/charFilter can only refer to one external .txt file
    filters_txt_tbl <- get_filter_txt_table(schema, fieldtype_name, NA)
    # when there is no external .txt files:
    if(1==length(filters_txt_tbl) && is.na(filters_txt_tbl)){
      return(add_column(analyzer_filters_tbl, external_files = NA))
    } else {
      return(analyzer_filters_tbl %>% left_join(filters_txt_tbl, by = c("tokenizer_filters", "filter_source")))
    }
  }

  # (3) if thre is any index analyzer:    
  test_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer[@type='index']", collapse="")
  test_node <- xml_type(xml_find_all(schema, test_xpath))
  has_index_analyzer <- !(length(test_node) == 0)
  
  # For index analyzers': get tokennizer and filters' names:
  if(has_index_analyzer){
    index_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer[@type='index']//@class", collapse="")
    nodes <- schema %>% xml_find_all(index_xpath)
    source <- as.character(xml_parent(nodes))    
    index_filter_classes <- nodes %>% xml_text()
  
    # extract the tokenizer and filter class names for index analyzers:
    index_filters <- str_match(index_filter_classes, pattern = "solr.([a-zA-Z]+)")[,2] #%>%
    index_filters_tbl <- tibble(fieldTypes = fieldtype_name, class=class, analyzer_types = "index", tokenizer_filters = index_filters, filter_source=source)
    # otain filters with external .txt files:
    filters_txt_tbl <- get_filter_txt_table(schema, fieldtype_name, "index")
    # when there is no external .txt files:
    if(1==length(filters_txt_tbl) && is.na(filters_txt_tbl)){
      index_filters_txt_tbl <- add_column(index_filters_tbl, external_files = NA)
    } else {
    index_filters_txt_tbl <- index_filters_tbl %>% left_join(filters_txt_tbl, by = c("tokenizer_filters", "filter_source"))
    }
  } else{
    index_filters_txt_tbl <- tibble()
  }
  
  # (4) if there is a query analyzer:
  test_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer[@type='query']", collapse="")
  test_node <- xml_type(xml_find_all(schema, test_xpath))
  has_query_analyzer <- !(length(test_node) == 0)
  
  # For query analyzers:
  if(has_query_analyzer){
    query_xpath <- paste0(".//fieldType[@name='", fieldtype_name, "']/analyzer[@type='query']//@class", collapse="")
    nodes <- schema %>% xml_find_all(query_xpath)
    source <- as.character(xml_parent(nodes))  
    query_filter_classes <- nodes %>% xml_text()
    query_filters <- str_match(query_filter_classes, pattern = "solr.([a-zA-Z]+)")[,2]
    query_filters_tbl <- tibble(fieldTypes = fieldtype_name, class=class, analyzer_types = "query", tokenizer_filters = query_filters, filter_source=source)
    # otain filters with external .txt files:
    filters_txt_tbl <- get_filter_txt_table(schema, fieldtype_name, "query")
    # when there is no external .txt files:
    if(1==length(filters_txt_tbl) &&is.na(filters_txt_tbl)){
      query_filters_txt_tbl <- add_column(query_filters_tbl, external_files = NA)
    } else {
      query_filters_txt_tbl <- query_filters_tbl %>% left_join(filters_txt_tbl, by = c("tokenizer_filters", "filter_source"))
    }
  } else{
    query_filters_txt_tbl <- tibble()
  }
  
  # cobine index and query analyzers' tokenizers and filters:
  # an empty tibble() does not affect the result of row binding
  return(bind_rows(index_filters_txt_tbl, query_filters_txt_tbl))
}

# return a table of {fieldTypes, class, analyzer_types, tokenizer_filters, external_files}
# fieldTypes: a set of fieldType names: NAs and duplicated values are allowed
# note: FlattenGraphFilterFactory is filtered out
get_fieldType_analyzerType_filters_externallists <- function(schema, fieldtype_names){
  
  # remove duplicated fieldType names and NA values:
  my_fieldtypes_names <- fieldtype_names %>% unique() %>% na.omit()
  
  tbl <- map(my_fieldtypes_names, get_filters_externalfiles, schema) %>% 
    bind_rows() %>% filter(tokenizer_filters!="FlattenGraphFilterFactory") %>% arrange(fieldTypes)
  
  return(tbl)
}


# this function helps testing functions of constructing configuration tables:
test_load <- function(conf_directory="C:/Solrhome/solr-6.6.1/server/solr/conf"){
  
  # obtain the xml files:
  # load solrconfig.xml and schema.xml from the solr conf directory
  config <- load_xml(conf_directory, xml_file_name="solrconfig.xml")
  schema <- load_xml(conf_directory, xml_file_name="schema.xml")
  
  # a set of requestHandler names
  rhs <- get_requestHandlers(config)
  
  # a table of requestHandler and fields relationships (qf, pf and fl)
  rh_f <- get_requestHandler_fields_table(config, rhs)
  
  # a table of fields and fieldTypes relationships
  f_ft <- get_field_fieldTypes_table(schema, rh_f$fields)

  # a table of details on fieldTypes (analyzers, filters and external txt files)
  ft_at_f_txt <- get_fieldType_analyzerType_filters_externallists(schema, f_ft$fieldTypes)
  
  # a combined big table of configurations from the two xml files:
  SolrConfigs <- rh_f %>%
    left_join(f_ft, by = "fields") %>%
    left_join(ft_at_f_txt, by="fieldTypes")
    
  
  
  # SolrConfigs <- list(rhs=rhs, rh_f=rh_f, f_ft=f_ft, ft_at_f_txt=ft_at_f_txt)
  # to be decided
  
  return(SolrConfigs)
}

