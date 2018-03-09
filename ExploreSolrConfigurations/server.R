library(shiny)

# Define server logic:
shinyServer(function(input, output) {
   
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
        need(input$grp_settings, "Check at least one from {qf, pf, fl, match_query_parser}!")
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
    
    # decide fields in {qf, pf, fl, match_query_parser}
    if(length(input$grp_settings) < 4){
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
  
})
