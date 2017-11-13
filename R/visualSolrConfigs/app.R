library(shiny)
library(collapsibleTree)
library("DT")


src_file_name <- "loadConfigs.R"
source(src_file_name)

# ######      debug only:    ######
# if(!exists("SolrConfigs")){
#  SolrConfigs <- test_load()
# }
# http://localhost:8983/solr/collection1/admin/file?file=solrconfig.xml&contentType=text/xml;charset=utf-8
# http://localhost:8983/solr/collection1/admin/file?file=schema.xml&contentType=text/xml;charset=utf-8
# ###### end of debug snipet ######


if (interactive()) {

  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
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
                   textInput("solrconfig_xml_url", "Paste your solrconfig.xml", ""),      
                   textInput("schema_xml_url", "Paste your solrconfig.xml", ""),
                   actionButton("btn_get", "Get remote xmls"), 

                   # Horizontal line ----
                   tags$p("Optional: load local configurations."),
                   
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
                                              "settings (qf,pf,fl)"="settings", 
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
                                        "Display fields (fl)" = "fl"),
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
  # end of UI design
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    # remote solr configurations:
    SolrConfigsInput <- eventReactive(input$btn_get, {
      data <- get_solr_configurations(input$solrconfig_xml_url, 
                              input$schema_xml_url, 
                              remote = TRUE)
      
      data
    })
    
    # local solr configurations:
    SolrConfigsInput <- eventReactive(input$btn_get_local, {
      data <- get_solr_configurations(input$solrconfig_xml_local$datapath, 
                              input$schema_xml_local$datapath, 
                              remote = FALSE)
      
      data
      
    })
    
    # Add code for Configuration loaded message in the future
    
    # visualizations: 
    eventReactive(input$go, {
      observe({
        x <- input$hierarchy
        
        # Can use character(0) to remove all choices
        if (is.null(x))
          x <- character(0)
        
        # Can also set the label and select items
        updateSelectInput(session, "root_node",
                          choices = x,
                          selected = head(x, 1)
        )
        
        validate(
          need(input$grp_settings, "Check at least one from {qf, pf, fl}!")
        )
      })
    })  
    
    datasetInput <- eventReactive(input$go, {
      
      # prepare the dataset for the collapsible tree and DT table
      
      configs <- SolrConfigsInput()
      
      # search the requestHandler (node) that you are interested in:
      if(!is.na(input$handler) && input$handler!=""){
        configs <- configs %>% 
          filter(str_detect(requestHandlers, regex(input$handler, ignore_case = TRUE)))
      }
      
      # search the field (node) that you are interested in: 
      if(!is.na(input$field) && input$field!=""){
        configs <- configs %>% 
          filter(str_detect(fields, regex(input$field, ignore_case = TRUE)))
      }
      
      # search the fieldtype (node) that you are interested in: 
      if(!is.na(input$fieldtype) && input$fieldtype!=""){
        configs <- configs %>% 
          filter(str_detect(fieldTypes, regex(input$fieldtype, ignore_case = TRUE)))
      }
      
      # search the externaltxt (node) that you are interested in: 
      if(!is.na(input$externaltxt) && input$externaltxt!=""){
        configs <- configs %>% 
          filter(str_detect(external_files, regex(input$externaltxt, ignore_case = TRUE)))
      }
      
      # decide fields in {qf, pf, fl}
      if(length(input$grp_settings) < 3){
        configs <- configs %>% filter(settings %in% input$grp_settings)
      } 
      
      # return the processed dataset    
      configs %>% select(input$hierarchy) %>% unique()
      
    })
    
    # input$hierarchy
    hierarchyInput <- eventReactive(input$go, {
      input$hierarchy
    })
    
    # input$root_node
    rootNodeInput <- eventReactive(input$go, {
      input$root_node
    })
    
    collapsedInput <- eventReactive(input$go, {
      collapsed <- TRUE;
      if(length(input$hierarchy)<=2){
        collapsed <- FALSE
      }
      collapsed
    })
    
    # conftree
    output$conftree <- renderCollapsibleTree({
      collapsibleTreeSummary(
        df = datasetInput(),
        hierarchy = hierarchyInput(),
        inputId = "node",
        root = rootNodeInput(),
        collapsed = collapsedInput()
      )
    })
    
    # tracking recently clicked node on conftree
    output$str <- renderPrint(str(input$node))
    
    # conftable
    output$conftable <- DT::renderDataTable(
      configs <- datasetInput()
    )
   
  } # end of server func
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}