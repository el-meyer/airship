options(shiny.sanitize.errors = FALSE) 
options(shiny.maxRequestSize = 50*1024^2)
library(shiny)
library(readxl)
library(xlsx)
library(DT)

ui <- fluidPage(title = "Simulation Results", # title in browser window tab
                
                
                # App title
                titlePanel("Simulation Shiny App"), 
                
                
                sidebarLayout(
                  
                  
                  sidebarPanel(
                    
                    checkboxInput("exampleData", "Use example dataset"),
                    
                    
                    conditionalPanel("input.exampleData == 0",
                                     radioButtons("datatype", 
                                                  "Which datatype do you upload", 
                                                  choices = c("Excel", "Csv")),
                                     conditionalPanel("input.datatype == 'Csv'",
                                                      radioButtons("sep", "Csv-Separator", choices = c(",", ";", "\t"))),
                                     fileInput("file", "Choose file to upload"),
                                     checkboxInput("change", "Do you want to specify numeric columns?"),
                                     conditionalPanel("input.change",
                                                      checkboxGroupInput("numerics", "Choose numeric Variables", choices = NULL),
                                                      actionButton("go", "Process changes to numeric"))
                    )
                    
                    
                    
                    
                  ),
                  
                  
                  mainPanel(
                    tabsetPanel(
                      
                      tabPanel("Data Overview",
                               
                               DT::dataTableOutput("dataDT")), 
                      
                      tabPanel("Filter variables",
                               
                               #adds every checked variable from input$varname as an input element 
                               # with the corresponding values as choices
                               h3("Choose values to filter for"),
                               uiOutput("vars_prefilter"),
                               textOutput("prefilters")),
                      
                      tabPanel("New Data Overview",
                               
                               actionButton("goDT", "Apply filters to dataset"),
                               DT::dataTableOutput("filteredDT"))
                      
                      
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
  
  exampleData <- as.data.frame(lapply(exampleData, function(y) gsub(",", ".", y)))
  for(i in c(1:2, 5:6, 8:13)){
    exampleData[,i] <- as.numeric(exampleData[,i])
  }
  for(j in c(3,4,7)){
    exampleData[,j] <- as.factor(exampleData[,j])
  }
  
  
  # widget for user data upload
  upload <- reactive({
    validate(need(input$file, "no file")) # if run "no file" appears everywhere upload() is called
    inFile <-input$file 
    
    if(input$datatype == "Excel"){
      mydata <- read_excel(inFile$datapath,  1)
    } else {
      mydata <- read.csv(inFile$datapath, 
                         sep = input$sep, 
                         stringsAsFactors = TRUE)
      updateCheckboxGroupInput(session, "numerics", choices = colnames(mydata))
    }
    
    
    return(mydata) 
    
  })
  
  
  # Use example data if checkbox is checked, otherwise use uploaded dataset
  data_full <- reactive({
    if(input$exampleData){
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
      
      return(upload())
    }
  })
  
  
  
  
  
  
  # display dataset as DT
  output$dataDT <- DT::renderDT(
    data_full(),
    filter = "top",
    options = list(lengthChange = FALSE, autoWidth = TRUE)
  )
  
  #filter directly in DT
  data <- reactive({
    req(input$dataDT_rows_all)
    d <- data_full()
    d[input$dataDT_rows_all,]
  })
  
  output$filteredDT <- DT::renderDT({
    data()
  })

  # output$filteredDT <- renderDT(
  #   d
  # )
  
  
  
  
}  

shinyApp(ui = ui, server = server)



