options(shiny.sanitize.errors = FALSE) 
options(shiny.maxRequestSize = 50*1024^2)
library(shiny)
library(readxl)
library(xlsx)
library(DT)
library(shinybusy)
library(plotly)
library(tidyverse)
library(shinyBS)

ui <- 
  fluidPage(
    title = "Simulation Results", # title in browser window tab
    
    
    # App title
    titlePanel("Simulation Shiny App"), 
    
    add_busy_spinner(spin = "fading-circle"),
    sidebarLayout(
      
      sidebarPanel(
        
        checkboxInput(
          "exampleData", 
          "Use example dataset"
        ),
        
        
        conditionalPanel(
          "input.exampleData == 0",
          
          radioButtons(
            "sep", 
            "Csv-Separator", 
            choices = c(",", ";", "\t")
          ),
          
          fileInput(
            "file", 
            "Choose file to upload"
          ),
          
          selectInput(
            "inputend", 
            "State last input variable", 
            choices = NULL
          )
          
          
          
        )
        
      ),
      
      
      mainPanel(
        tabsetPanel(
          
          tabPanel(
            "Pre-filter data",
            DT::dataTableOutput("dataDT")
          ), 
          
          
          tabPanel(
            "Choose default values",
            
            fluidRow(
              column(
                12,
                actionButton(
                  "goDT", 
                  "Apply filters to dataset"
                ),
                DT::dataTableOutput("filteredDT")
              )
            ),
            
            fluidRow(
              column(
                width = 6,
                HTML("test output to see if list with default values is filled correctly"),
                verbatimTextOutput("lDefault")
              ),
              
              column(
                width = 6,
                actionButton(
                  "updateDefaultList", 
                  "Save default values"
                ),
                
                checkboxInput("all_default", 
                              "All/None", 
                              value = TRUE
                              ),
                
                checkboxGroupInput(
                  "checkboxDefault",
                  "Which variables should have saved default values?",
                  choices = NULL
                )
              )
              
              
            ) 
          ),
          
          tabPanel(
            "Plot",
            
            fluidRow(
              plotOutput("lineplot")
            ),
            
            
            fluidRow(
              column(
                6,
                #verbatimTextOutput("test"),
                
                selectInput(
                  "x", 
                  "Choose x-Variable", 
                  choices = NULL
                ),
                
                checkboxGroupInput(
                  "OC", 
                  "Choose OC to plot", 
                  choices = NULL
                )
              ),
              
              column(
                6,
                
                checkboxInput(
                  "checkboxFacet",
                  "Do you want to add a facet grid dimension"
                ),
                
                conditionalPanel(
                  "input.checkboxFacet != 0",
                  
                  selectInput(
                    "facet_rows", 
                    "Choose row variable", 
                    choices = NULL
                  ),
                  
                  selectInput(
                    "facet_cols", 
                    "Choose col variable", 
                    choices = NULL
                  )
                ),
                
                
                
                
                checkboxInput(
                  "checkboxShape", 
                  "Do you want to add a shape dimension"
                ),
                conditionalPanel(
                  "input.checkboxShape != 0",
                  
                  selectInput(
                    "shape", 
                    "Choose shape variable",
                    choices = NULL
                  )
                )
              )
              
              
            ),
            
            fluidRow(
              DT::dataTableOutput("df_plot")
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
                          sep = ",",
                          stringsAsFactors = FALSE)
  
  
  
  # widget for user data upload
  upload <- reactive({
    validate(need(
      input$file, 
      "no file")
    ) # if no file is uploaded yet "no file" appears everywhere upload() is called
    
    inFile <-input$file 
    
    mydata <- read.csv(inFile$datapath,
                       header = TRUE,
                       sep = input$sep, 
                       stringsAsFactors = FALSE)
    # updateCheckboxGroupInput(session, "inputend", choices = colnames(mydata))
    
    
    
    # for(i in 1:length(input$session)){
    #   mydata[,input$session[i]] <- as.numeric(mydata[,input$session[i]])
    # }    
    
    return(mydata) 
    
  })
  
  
  
  
  # Use example data if checkbox is checked, otherwise use uploaded dataset
  data_full <- reactive({
    if(input$exampleData){
      updateSelectInput(session, 
                        "inputend", 
                        choices = colnames(exampleData),
                        selected = "setting"
      )
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
      updateSelectInput(session, 
                        "inputend", 
                        choices = colnames(upload())
      )
      return(upload())
    }
  })
  
  
  
  
  
  
  # display dataset as DT
  # Table in tab 'Pre-filter data'
  output$dataDT <- DT::renderDT(
    data_full(),
    filter = "top",
    options = list(lengthChange = FALSE, 
                   autoWidth = TRUE
                   #, scrollX = TRUE
    )
  )
  
  
  # Table with all chosen filters in 'Pre-filter data'
  data <- reactive({
    req(input$dataDT_rows_all)
    d <- data_full()
    ind_outputstart <<- isolate(which(colnames(d) == input$inputend)+1) # which column is the last input column?
    
    # Convert output parameters to numeric values
    for(i in ind_outputstart:ncol(d)){
      d[,i] <- as.numeric(d[ ,i])
    }
    
    
    d[input$dataDT_rows_all,] # extracts rows that fit the filter choices
    
  })
  
  
  
  # Table displayed at start in tab 'Choose default values'
  # execute filters to create new DT upon clicking action button
  output$filteredDT <- DT::renderDT({
    input$goDT
    
    # which column is the last input column?
    ind_inputend <<- isolate(which(colnames(data()) == input$inputend)) 
    
    # display only input columns
    data_filtered <<- isolate(data()[,1:ind_inputend])
    
    # inputvariables
    names_inputs <<- colnames(data()[,1:ind_inputend]) 
    
    #outputvariables
    names_outputs <<- colnames(data()[,(ind_inputend + 1):ncol(data())])
    
    
    
    # --------------Some choice-updates for inputs---------
    updateCheckboxGroupInput(session, 
                             "checkboxDefault", 
                             choices = names_inputs, 
                             selected = if(input$all_default) names_inputs
    )
    
    updateCheckboxGroupInput(session,
                             "OC",
                             choices = names_outputs)
    
    updateSelectInput(session,
                      "x",
                      choices = names_inputs
    )
    
    updateSelectInput(session,
                      "facet_rows",
                      choices = names_inputs
    )
    
    
    updateSelectInput(session,
                      "facet_cols",
                      choices = names_inputs
    )
    
    updateSelectInput(session,
                      "shape",
                      choices = names_inputs
    )
    
    #---------------------------------------------------------------
    
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
  options = list(lengthChange = FALSE, 
                 autoWidth = TRUE, 
                 #scrollX = TRUE, 
                 pageLength = 5)
  )
  
  
  
  # specify default value for every variable and save in data frame
  # if df is not reduced to 1 line (i. e. if not for every variable a default value is specified), the first observation is taken as default
  
  data_default <- reactive({
    req(input$filteredDT_rows_all)
    # ind_inputend <- isolate(which(colnames(data()) == input$inputend))
    # data_filtered <<- isolate(data()[,1:ind_inputend])
    d <- data()
    d <- d[input$filteredDT_rows_all, 1:ind_inputend]
    d[1,]
  })
  
  
  
  
  
  

  # save default values in a list upon clicking action button
  lDefault <- eventReactive(input$updateDefaultList, {as.list(data_default()[1, input$checkboxDefault])})
  
  
  # Output of list with default values
  output$lDefault <- renderPrint({
    req(lDefault)
    print(lDefault())
  })
  

  
  
  
  
  # PLOT -------------------------------------------------------------------

  # Data frame used for plot
  # Filters every variable for the specified default value except the chosen simulation parameters, which can have more distinguishable values
  
  df_plot <- reactive({
    
    # 1 line df with default values for variables that are checked
    default_df <- data_default()[1, input$checkboxDefault]
    
    # vector of names of simulation parameters
    sim_par <- input$x
    if(input$checkboxShape){
      sim_par <- c(sim_par, input$shape)
    }
    
    if(input$checkboxFacet){
      sim_par <- c(sim_par, input$facet_rows, input$facet_cols)
    }
    
    # exclude simulation parameters from df with default values
    default_filter <- default_df[!(names(default_df) %in% sim_par)]
    
    bedingung <- paste0(names(default_filter),
                        " == ",
                        paste0("'", default_filter[1,], "'"),
                        collapse = " & ")
    
    
    df_plot <- subset(data(), eval(parse(text = bedingung)))
    df_plot # return data frame
  })
  
  
  
  output$df_plot <- DT::renderDT({
    
    df_plot()
  })
  
  
  
  # Transform dataset to long format on chosen output variables for easy plotting
  data_longer <- reactive({
    
    
    d <- df_plot()
    # d <- d[input$filteredDT_rows_all,]
    
    d <- 
      d %>%
      pivot_longer(cols = input$OC,
                   names_to = "OC",
                   values_to = "value")
    d
  })
  
  
  
  # Plot based on which dimensions are chosen
  output$lineplot<- renderPlot({
    p1 <- ggplot(
      data_longer(), 
      aes_string(x = input$x)
    )

    p1 <- 
      p1 + 
      geom_line(aes(y = value, color = OC)) +
      geom_point(aes(y = value, color = OC))
    
    
    
    if(input$checkboxShape){
      p1 <- 
        p1 + 
        aes(linetype = factor(get(input$shape)),
            shape = factor(get(input$shape)))
    }
    

    if(input$checkboxFacet){
      p1 <- 
        p1 + 
        facet_grid(vars(get(input$facet_rows)),
                   vars(get(input$facet_cols))
                   )
    }
    
    p1
    
  })
  
  
 
}  

shinyApp(ui = ui, server = server)

#shinyBS::bsModal()







# data_test <- read.csv("example_data.csv",
#                       header = TRUE,
#                       sep = ",",
#                       stringsAsFactors = FALSE)
# 
# default_list <- as.list(data_test[1,1:which(names(data_test) == "setting")])
# 
# 
# # default_df <- as.data.frame(do.call(cbind, default_list))
# 
# default_df <- as.data.frame(default_list)
# 
# 
# 
# sim_par <- c("n_int", "sharing_type", "cohorts_max", "sensitivity_biomarker")
# default_filter <- default_df[!(names(default_df) %in% sim_par)]
# 
# bedingung <- paste0(names(default_filter),
#                     " == ",
#                     paste0("'", default_filter[1,], "'"),
#                     collapse = " & ")
# 
# 
# 
# df_plot <- subset(data_test, eval(parse(text = bedingung)))
# dim(df_plot)
# 
# remove.factors(df_plot)
# 
# p <- ggplot(df_plot, aes(x = n_int))
# p <- p + geom_line(aes(y = FWER, color = factor(sensitivity_biomarker), linetype = factor(sensitivity_biomarker)))
# p <- p + facet_grid(rows = vars(sharing_type), cols = vars(cohorts_max))
# p
# 
# 


