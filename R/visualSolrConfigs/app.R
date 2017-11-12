library(shiny)
library(collapsibleTree)

src_file_name <- "loadConfigs.R"
source(src_file_name)
if(!exists("SolrConfigs")){
  SolrConfigs <- test_load()
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Solr Configuration Explorer"),
   
   # Sidebar with a slider input for number of bins 
   # "requestHandlers", "settings", "fields", "fieldTypes", "class", "analyzer_types", "tokenizer_filters", "filter_source", "external_files"
   sidebarLayout(
      sidebarPanel(
        selectizeInput("hierarchy","Configuration tree hierarchy",
                       choices = names(SolrConfigs),
                       selected = c("requestHandlers","settings", "fields", "fieldTypes", "analyzer_types", "tokenizer_filters", "external_files"),
                       multiple=TRUE,
                       options=list(plugins=list('drag_drop','remove_button'))),
        
        tags$p("Correct hierarchy ordering:"),
        tags$p(paste(names(SolrConfigs), collapse = "->")),
       
        checkboxGroupInput("grp_settings", "Explore fields in",
                           c("Query fields (qf)" = "qf",
                             "Phrase fields (pf)" = "pf",
                             "Display fields (fl)" = "fl"),
                           selected = "qf"),
         
        textInput("handler", "Search RequestHandler", ""),
       
        selectInput(
          "root_node", "Root Node",
          choices = c("requestHandlers", "fields", "fieldTypes"),
          selected = "requestHandlers"
        ),
        tags$p("The node you most recently clicked:"),
        verbatimTextOutput("str")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        collapsibleTreeOutput("conftree")
        
        #p(paste(names(SolrConfigs), collapse = "->"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
  })
  
   output$conftree <- renderCollapsibleTree({
     
     validate(
       need(input$grp_settings, "Check at least one from {qf, pf, fl}!")
     )
     
     # prepare the df dataset for collapsible tree display
    configs <- SolrConfigs
     
     # filter the requestHandler that you are interested in:
     if(!is.na(input$handler) && input$handler!=""){
       configs <- SolrConfigs %>% 
         filter(str_detect(requestHandlers, regex(input$handler, ignore_case = TRUE)))
     }
    
   
    if(length(input$grp_settings) < 3){
      configs <- configs %>% filter(settings %in% input$grp_settings)
    }
    
     
     # projection on data sets (select appropriate columns) when hierarchy tree is selected: 
     #if(1!=length(input$hierarchy) || !is.na(input$hierarchy)){
       # needs to sort the selected hierarchy accoriding to its position in the original dataset
       # To be added
       
       # filtering:
     #  configs <- configs %>% select(input$hierarchy)
     #}  
       
     #configs <- SolrConfigs %>% select(input$hierarchy)
     
     #if(input$root_node %in% input$hierarchy){
     #   my_root_node <- input$root_node
     # } else{
     #   my_root_node <- input$hierarchy[1]
     # }
     
     collapsibleTreeSummary(
       configs,
       hierarchy = input$hierarchy, #,
       inputId = "node",
       root = input$root_node#,
       #attribute = "settings"
       #height = "1000px"
     )
   })
   
   output$str <- renderPrint(str(input$node))
   
   
   #output$request_handler_name <-  renderText({ input$handler })
}

# Run the application 
shinyApp(ui = ui, server = server)

