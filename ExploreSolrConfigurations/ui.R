library(shiny)
library(collapsibleTree)
library(DT)

source("loadConfigs.R")

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Solr ReuqestHandler Explorer"),
  
  # Sidebar with a slider input for number of bins 
  # "requestHandlers", "settings", "fields", "fieldTypes", "class", "analyzer_types", "tokenizer_filters", "filter_source", "external_files"
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        
        # ############start of panel1############
        tabPanel("Config files", 
                 
                 # remote urls: 
                 # textInput("solrconfig_xml_url", "Paste your solrconfig.xml url", ""),      
                 # textInput("schema_xml_url", "Paste your solrconfig.xml url", ""),
                 # actionButton("btn_get", "Get remote xmls"), 
                 
                 # Horizontal line ----
                 # tags$p("Optional: load local configurations."),
                 
                 # local conf folders: 
                 fileInput("solrconfig_xml_local", "Choose solrconfig.xml",
                           multiple = FALSE,
                           accept = ".xml"), 
                 
                 fileInput("schema_xml_local", "Choose schema.xml",
                           multiple = FALSE,
                           accept = ".xml"), 
                 
                 actionButton("btn_get_local", "Get local xml files")
        ), 
        # ############end of panel1 ############
        
        # ############start of panel2 ############
        tabPanel("Explorer", 
                 actionButton("go", "Display"),
                 selectizeInput("hierarchy","Configuration tree hierarchy",
                                # or use names(SolrConfigs) for the choices
                                choices = c("requestHandlers (select)"="requestHandlers", 
                                            "settings (qf,pf,fl,match_query_parser)"="settings", 
                                            "fields"="fields", 
                                            "fieldTypes"="fieldTypes", 
                                            "Java class (solr.TextField)"="class", 
                                            "analyzer_types (index,query)"="analyzer_types", 
                                            "tokenizer_filters"="tokenizer_filters", 
                                            "filter_source (xml source)"="filter_source", 
                                            "external_files (synonyms.txt)"="external_files"),
                                selected = c("requestHandlers",
                                             "settings", 
                                             "fields", 
                                             "fieldTypes", 
                                             "analyzer_types", 
                                             "tokenizer_filters", 
                                             "external_files"),
                                multiple=TRUE,
                                options=list(plugins=list('drag_drop','remove_button'))),
                 
                 tags$p("Correct hierarchy ordering:"),
                 # for the following, one can use: paste(names(SolrConfigs), collapse = "->")
                 tags$p(paste(c("requestHandlers", 
                                "settings", 
                                "fields", 
                                "fieldTypes", 
                                "class", 
                                "analyzer_types", 
                                "tokenizer_filters", 
                                "filter_source", 
                                "external_files"), collapse = "->")),
                 
                 checkboxGroupInput("grp_settings", "Explore fields in",
                                    c("Query fields (qf)" = "qf",
                                      "Phrase fields (pf)" = "pf",
                                      "Display fields (fl)" = "fl",
                                      "Taxonomy field (match query parser)" = "match_query_parser"),
                                    selected = "qf"),
                 
                 textInput("handler", "Search a requestHandler", ""),
                 
                 textInput("field", "Search a field", ""),
                 
                 textInput("fieldtype", "Search a fieldTypes", ""),
                 
                 textInput("externaltxt", "Search external txt files", ""),
                 
                 selectInput(
                   "root_node", "Root Node",
                   choices = c("requestHandlers", "fields", "fieldTypes"),
                   selected = "requestHandlers"
                 ),
                 tags$p("The node you most recently clicked:"),
                 verbatimTextOutput("str")
        )
        # ############end of panel2############
      ) 
      # end of tabsetPanel
    ),
    # end of side bar
    
    # start of main panel
    # display configuration dataset into tree and table in different tabs
    mainPanel(
      # start of tabset panel
      tabsetPanel(
        tabPanel("Tree",collapsibleTreeOutput("conftree", width=750, height= 800)), 
        tabPanel("Tabular",DT::dataTableOutput("conftable"))
      )
      # end of tabset panel
    )
    # end of main panel
  )
  
  )
)
