options(shiny.sanitize.errors = FALSE) 
options(shiny.maxRequestSize = 50*1024^2)
library(shiny)
library(readxl)
library(xlsx)
library(DT)
library(shinybusy)

ui <- fluidPage(title = "Simulation Results", # title in browser window tab
                
                
                # App title
                titlePanel("Simulation Shiny App"), 
                
                add_busy_spinner(spin = "fading-circle"),
                sidebarLayout(
                  
                  
                  sidebarPanel(
                    
                    checkboxInput("exampleData", "Use example dataset"),
                    
                    
                    conditionalPanel("input.exampleData == 0",
                                     
                                     radioButtons("sep", "Csv-Separator", choices = c(",", ";", "\t")),
                                     fileInput("file", "Choose file to upload") 
                                     
                                     
                    ),
                    
                    selectInput("inputend", "State last input variable", choices = NULL)
                    
                    
                  ),
                  
                  
                  mainPanel(
                    tabsetPanel(
                      
                      tabPanel("Pre-filter data",
                               
                               DT::dataTableOutput("dataDT")), 
                      
                      
                      tabPanel("Choose default values",
                               
                               fluidRow(
                                 column(12,
                                        
                                        actionButton("goDT", "Apply filters to dataset"),
                                        #actionButton("goDefault", "Save default values"),
                                        DT::dataTableOutput("filteredDT"))),
                               
                               fluidRow(
                                 column(6,
                                        HTML("test output to see if list with default values is filled correctly"),
                                        verbatimTextOutput("lDefault")),
                                 
                                 column(6,
                                        actionButton("updateDefaultList", "Save default values"),
                                        checkboxGroupInput("checkboxDefault",
                                                           "Which variables should have saved default values?",
                                                           choices = NULL)
                                 )
                                 
                                 
                               ) 
                      )
                      
                    )
                  )
                )
                
)







server <- function(session, input, output){
  
  # read in Example data and convert some variables for correct display
  exampleData <- read.csv("example_data.csv", 
                          header = TRUE,
                          sep = ";",
                          stringsAsFactors = TRUE)
  
  # exampleData <- as.data.frame(lapply(exampleData, function(y) gsub(",", ".", y)))
  # for(i in c(1:2, 5:6, 8:13)){
  #   exampleData[,i] <- as.numeric(exampleData[,i])
  # }
  # for(j in c(3,4,7)){
  #   exampleData[,j] <- as.factor(exampleData[,j])
  # }
  
  
  # widget for user data upload
  upload <- reactive({
    validate(need(input$file, "no file")) # if no file is uploaded yet "no file" appears everywhere upload() is called
    inFile <-input$file 
    
    mydata <- read.csv(inFile$datapath,
                       header = TRUE,
                       sep = input$sep, 
                       stringsAsFactors = TRUE)
    # updateCheckboxGroupInput(session, "inputend", choices = colnames(mydata))
    
    
    
    # for(i in 1:length(input$session)){
    #   mydata[,input$session[i]] <- as.numeric(mydata[,input$session[i]])
    # }    
    
    return(mydata) 
    
  })
  
  
  
  
  # Use example data if checkbox is checked, otherwise use uploaded dataset
  data_full <- reactive({
    if(input$exampleData){
      updateSelectInput(session, "inputend", choices = colnames(exampleData))
      return(exampleData)
      
      # additional condition to be implemented if user changes variable classes
      
      # } else if(input$change){
      #   upload_numerics <- upload()
      #   #observeEvent(input$go, {
      #   
      #   for(i in input$numerics){
      #     levels(upload_numerics[,i]) <- as.numeric(levels(upload_numerics[,i]))}
      #   #})
      #   return(upload_numerics)
      
    } else{
      updateSelectInput(session, "inputend", choices = colnames(upload()))
      return(upload())
    }
  })
  
  
  
  
  
  
  # display dataset as DT
  output$dataDT <- DT::renderDT(
    data_full(),
    filter = "top",
    options = list(#lengthChange = FALSE, 
      autoWidth = TRUE, 
      scrollX = TRUE)
  )
  
  #filter directly in DT
  data <- reactive({
    req(input$dataDT_rows_all)
    d <- data_full()
    d[input$dataDT_rows_all,] # extracts rows that fit the filter choices
  })
  
  
  
  # Creates DT to choose default values
  # execute filters to create new DT upon clicking action button
  output$filteredDT <- DT::renderDT({
    input$goDT
    
    # display only input columns
    ind_inputend <<- isolate(which(colnames(data()) == input$inputend)) # which column is the last input column?
    data_filtered <<- isolate(data()[,1:ind_inputend])
    names_columns <- colnames(data()[1:ind_inputend])
    
    updateCheckboxGroupInput(session, "checkboxDefault", choices = names_columns, selected = names_columns)
    
    # display only columns with more than 1 unique entry
    uniques <- lapply(data_filtered, unique)
    bUniques <- sapply(uniques, function(x) length(x) == 1)
    data_filtered <<- data_filtered[,which(!bUniques)]
    
    for(i in colnames(data_filtered)){
      # transforms variables to factors to be able to choose 1 factor level as default value
      data_filtered[,i] <<- factor(as.factor(data_filtered[,i]))  #factor(...) drops unused factor levels from prefiltering
    }
    data_filtered
    
  },
  
  filter = "top",
  options = list(#lengthChange = FALSE, 
    autoWidth = TRUE, 
    scrollX = TRUE, 
    pageLength = 5)
  )
  
  
  
  # save remaining values as defaults and put them in a list
  data_default <- reactive({
    req(input$filteredDT_rows_all)
    # ind_inputend <- isolate(which(colnames(data()) == input$inputend))
    # data_filtered <<- isolate(data()[,1:ind_inputend])
    d <- data()
    d[input$filteredDT_rows_all, 1:ind_inputend]
  })
  
  #lDefault <- eventReactive(input$goDefault,{as.list(data_default())})
  
  lDefault <- eventReactive(input$updateDefaultList, {as.list(data_default()[input$checkboxDefault])})
  
  # observeEvent(input$goDefault,{
  #   lDefault <<- as.list(data_default())
  # })
  
  output$default <- renderTable({
    input$goDefault
    #lDefault <- as.list(data_default)
    isolate(data_default())
  })
  
  output$lDefault <- renderPrint({
    req(lDefault)
    print(lDefault())
  })
  
  
  
  
  
}  

shinyApp(ui = ui, server = server)





