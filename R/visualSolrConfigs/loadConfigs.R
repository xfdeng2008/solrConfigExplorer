library(xml2)
library(psych)
library(tidyverse)
library(purrr)
library(stringr)
library(dplyr)


# load configuration xml files
load_xml <- function(base_conf_dir = "C:/Solrhome/solr-6.6.1/server/solr/conf", xml_file_name){
  file_path <- file.path(base_conf_dir, xml_file_name)
  return(read_xml(file_path))
}


# global xml variables: 
# load solrconfig.xml and schema.xml from the solr conf directory
config_xml <- load_xml(xml_file_name="solrconfig.xml")
schema_xml <- load_xml(xml_file_name="schema.xml")

# debug:
config <- config_xml
schema <- schema_xml

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


