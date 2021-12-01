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
library(colourpicker)
library(shinyWidgets)
library(bslib)
library(shinydashboard)

ui <- 
  dashboardPage(
    
    dashboardHeader(
      title = "Simulation Results"
    ), # title in browser window tab
    
    
    
    
    dashboardSidebar(
      sidebarMenu(
        
        menuItem(
          "Data Settings", 
          tabName = "data_settings", 
          icon = icon("gear"),
          checkboxInput(
            "checkboxExampleData", 
            "Use example dataset"
          ),
          
          
          conditionalPanel(
            "input.checkboxExampleData == 0",
            
            radioButtons(
              "sep", 
              "Csv-Separator", 
              choiceValues = c(",", ";", ""),
              choiceNames = c(",", ";", "whitespace")
            ),
            
            fileInput(
              "file", 
              "Choose file to upload"
            ),
            
            selectInput(
              "inputend", 
              "State last input variable", 
              choices = NULL
            ),
            
            checkboxInput(
              "checkboxRepvar",
              "Does your dataset contain a variable indicating the replication run?"
            ),
            
            conditionalPanel(
              "input.checkboxRepvar != 0",
              
              selectInput(
                "repvar",
                "Select the replication run variable",
                choices = NULL
              ),
              
              selectInput(
                "repvarMethod",
                "Select the summary method you want to apply to your data",
                choices = c("mean", "median")
              )
            )
          )
        ),
        
        menuItem(
          "Data",
          tabName = "data",
          icon = icon("file")
        ),
        
        menuItem(
          "Default values", 
          tabName = "default", 
          icon = icon("motorcycle")
        ),
        
        menuItem(
          "Distribution", 
          tabName = "distribution", 
          icon = icon("area-chart")
        ),
        
        menuItem(
          "Plot", 
          tabName = "plot", 
          icon = icon("line-chart")
        ),
        
        br(),
        h3("Default value overview"),
        tableOutput("defaults_df")
      )
    ),
    
    dashboardBody(
      add_busy_spinner(spin = "fading-circle"),
      tabItems(
        tabItem(
          tabName = "data",
          verbatimTextOutput("test"),
          DT::dataTableOutput("dataDT"),
          # conditionalPanel("input.checkboxRepvar != 0",
          #                  h3("Summarized Data"),
          #                  DT::dataTableOutput("repDataDT")
          # )
          uiOutput("dataDT_summarized")
        ),
        tabItem(
          tabName = "default",
          DT::dataTableOutput("chooseDT"),
          br(),
          # conditionalPanel(
          # "input.checkboxExampleData",
          actionButton("buttonDefault", "Take first row as default values")
          # )
          
        ),
        tabItem(
          tabName = "distribution",
          
          fluidRow(
            column(
              4,
              
              selectInput(
                "boxplotGroupVar",
                "Select grouping variable for distribution plot",
                choices = NULL
              ),
              
              selectInput(
                "boxplotColorVar",
                "Select variable for (additional) color differentiation",
                choices = NULL
              ),
              
              sliderInput(
                "alpha",
                "Select transparency (alpha)",
                min = 0,
                max = 1,
                value = 0.1,
                step = 0.1
              ),
              
              selectInput(
                "boxplotOutputVar",
                "Select distribution variable",
                choices = NULL
              )
            ),
            
            column(
              4,
              radioButtons(
                "radioFacetDistribution",
                "Do you want to add a facet dimension?",
                choices = c("no", "grid", "wrap")
              ),
              
              conditionalPanel(
                "input.radioFacetDistribution == 'grid'",
                
                selectInput(
                  "facet_distribution_rows", 
                  "Choose row variable", 
                  choices = NULL,
                  multiple = TRUE
                ),
                
                selectInput(
                  "facet_distribution_cols", 
                  "Choose col variable", 
                  choices = NULL,
                  multiple = TRUE
                )
              ),
              
              
              conditionalPanel(
                "input.radioFacetDistribution == 'wrap'",
                
                selectizeInput(
                  "facet_distribution_wrap", 
                  "Choose variables to facet wrap",
                  choices = NULL,
                  multiple = TRUE
                )
              )),
            
            column(
              4,
              
              HTML("<b>Choose plottype</b>"),
              
              radioButtons(
                "boxplottype",
                "Box-/Violin- or Density-plot",
                choices =c("Boxplot", "Violinplot", "Densityplot"),
                selected = "Boxplot",
              )
            )
          ),
          
          plotOutput("pBoxplot")
        ),
        
        tabItem(
          tabName = "plot",
          
          fluidRow(
            # plotlyOutput("lineplot")
            uiOutput("lineplot_ui")
            # plotOutput("lineplot")
          ),
          
          hr(),
          
          
          fluidRow(
            column(
              3,
              #verbatimTextOutput("test"),
              
              selectInput(
                "x", 
                "Choose x-Variable", 
                choices = NULL
              ),
              
              # colourPicker(3),
              
              
              selectizeInput(
                "OC", 
                "Choose OC to plot", 
                choices = NULL,
                multiple = TRUE
              )
            ),
            
            column(
              3,
              
              radioButtons(
                "radioFacet",
                "Do you want to add a facet dimension?",
                choices = c("no", "grid", "wrap")
              ),
              
              conditionalPanel(
                "input.radioFacet == 'grid'",
                
                selectInput(
                  "facet_rows", 
                  "Choose row variable", 
                  choices = NULL,
                  multiple = TRUE
                ),
                
                selectInput(
                  "facet_cols", 
                  "Choose col variable", 
                  choices = NULL,
                  multiple = TRUE
                )
              ),
              
              
              conditionalPanel(
                "input.radioFacet == 'wrap'",
                
                selectizeInput(
                  "facet_wrap", 
                  "Choose variables to facet wrap",
                  choices = NULL,
                  multiple = TRUE
                )
              ),
              
              
              
              
              checkboxInput(
                "checkboxShape", 
                "Do you want to add a shape dimension?"
              ),
              conditionalPanel(
                "input.checkboxShape != 0",
                
                selectInput(
                  "shape", 
                  "Choose shape variable",
                  choices = NULL
                )
              )
            ),
            
            column(
              3,
              
              switchInput("plottype",
                          "Interactive Plot?",
                          value = FALSE,
                          size = "small"),
              
              actionButton("change_colors", label = "color choices for OC"),
              
              bsModal("modal_colors",
                      "Change colors of plot",
                      trigger = "change_colors",
                      size = "large",
                      uiOutput("colors_ui")
              )
              ,
              
              actionButton("change_style", label = "style options"),
              
              
              
              bsModal("modal_style", 
                      "Change style and size of plot", 
                      trigger = "change_style", 
                      size = "large",
                      
                      
                      
                      checkboxInput(
                        "checkboxLine",
                        "Add lines?",
                        value = TRUE
                      ),
                      
                      checkboxInput(
                        "checkboxPoint",
                        "Add points?",
                        value = TRUE
                      ),
                      
                      
                      # checkboxInput(
                      #   "checkboxLegend",
                      #   "Specify legend coordinates?"
                      # ),
                      
                      conditionalPanel(
                        "input.plottype",
                        
                        sliderInput("xLegend",
                                    "x-coord legend",
                                    min = -0.5,
                                    max = 1.2,
                                    value = 1.05, 
                                    step = 0.05),
                        
                        
                        
                        sliderInput("yLegend",
                                    "y-coord legend",
                                    min = -0.5,
                                    max = 1.2,
                                    value = 0.5,
                                    step = 0.05)
                      ),
                      # 
                      #                 sliderInput("res",
                      #                             "Change resolution",
                      #                             value = 72,
                      #                             min = 50, 
                      #                             max = 200),
                      
                      checkboxInput(
                        "checkboxSize", 
                        "Change plot size"
                      ),
                      conditionalPanel(
                        "input.checkboxSize != 0",
                        
                        
                        
                        sliderInput(
                          "plotwidth",
                          "Plot width (px)",
                          value = 1000,
                          min = 600,
                          max = 1500
                        ),
                        
                        sliderInput(
                          "plotheight",
                          "Plot height (px)",
                          value = 600,
                          min = 300,
                          max = 1000
                        ),
                        
                        sliderInput(
                          "linesize",
                          "Line and point size",
                          value = 0.6,
                          min = 0.1,
                          max = 3,
                          step = 0.1
                        ),
                        
                        numericInput(
                          "plotfontsize",
                          "Font size",
                          value = 11,
                          min = 1,
                          max = 30,
                          step = 0.5
                        ),
                        
                        selectInput(
                          "plotfont",
                          "Font",
                          choices = c("sans", "Times", "Courier")
                        )
                      ),
                      
                      
                      checkboxInput(
                        "checkboxTheme",
                        "Change the theme?"
                      ),
                      
                      conditionalPanel(
                        "input.checkboxTheme !=0",
                        
                        radioButtons(
                          "plottheme",
                          "Select the theme",
                          choices = c(
                            "Grey", "White", "Linedraw",
                            "Light", "Minimal", "Classic"
                          )
                        )
                        
                      )
              ),
              
              actionButton("save_plot", label = "Download plot"),
              
              
            ),
            
            column(
              3,
              
              checkboxInput(
                "checkboxTitle",
                "Add title"
              ),
              
              conditionalPanel(
                "input.checkboxTitle != 0",
                
                textInput(
                  "plot_title",
                  "Enter the plot title"
                ),
                
                radioButtons(
                  "plot_title_place",
                  "Title alignment",
                  choices = c("left" = 0, "center" = 0.5, "right" = 1)
                ),
                
                numericInput(
                  "plot_title_size",
                  "Size",
                  value = 30,
                  min = 1,
                  max = 50,
                  step = 1
                ),
                
                colourInput(
                  inputId = "plot_title_colour", label = "Title colour:",
                  showColour = "both",
                  value = "black",
                  allowTransparent = FALSE)
                
              ),
              
              hr(),
              
              checkboxInput(
                "checkboxAxis",
                "Change axis labels"
              ),
              
              conditionalPanel(
                "input.checkboxAxis != 0",
                
                textInput(
                  "xLab",
                  "X-axis label:"
                ),
                
                textInput(
                  "yLab",
                  "Y-axis label:"
                )
                
              ),
              
              hr(),
              
              
              bsModal("modal", "Download plot", trigger = "save_plot", size = "medium",
                      
                      selectInput("download_type", "Choose file type",choices = c("png", "jpeg", "tiff")),
                      
                      
                      sliderInput(
                        "resolution",
                        "Resolution",
                        value = 72,
                        min = 36, 
                        max = 288
                      ),
                      
                      
                      textInput("download_name", "Specify file name"),
                      
                      downloadButton("download_plot", "Download")
              )
              
              
            )
          ),
          
          fluidRow(
            # verbatimTextOutput("df_plot")
            DT::dataTableOutput("df_plot")
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
                          stringsAsFactors = TRUE)
  
  
  
  # widget for user data upload
  upload <- reactive({
    validate(
      need(input$file, "no file")
    ) # if no file is uploaded yet "no file" appears everywhere upload() is called
    
    inFile <-input$file 
    
    mydata <- read.csv(inFile$datapath,
                       header = TRUE,
                       sep = input$sep,
                       stringsAsFactors = TRUE)
    # 
    # mydata <- read_csv(inFile$datapath,
    #                    col_names = TRUE)
    
    # names(mydata) <- gsub("\\.", " ", names(mydata))
    
    
    # for(i in 1:length(input$session)){
    #   mydata[,input$session[i]] <- as.numeric(mydata[,input$session[i]])
    # }
    
    
    
    
    
    
    
    return(mydata) 
    
  })
  
  
  
  
  # Use example data if checkbox is checked, otherwise use uploaded dataset
  data_full <- reactive({
    if(input$checkboxExampleData){
      updateSelectInput(session, 
                        "inputend", 
                        choices = colnames(exampleData),
                        selected = "setting"
      )
      
      return(exampleData)
      
      
      
    } else {
      updateSelectInput(session,
                        "inputend",
                        choices = colnames(upload())
      )
      
      updateSelectInput(session,
                        "repvar",
                        choices = colnames(upload())
      )
      
      return(upload())
    }
  })
  
  
  
  # update choices for replication variable
  observe({
    # if(input$checkboxRepvar){
    # updateSelectInput(
    #   session,
    #   "repvar",
    #   choices = colnames(upload())
    # )
    # } else {
    #   updateSelectInput(
    #     session, 
    #     "repvar",
    #     choices = NULL,
    #     selected = NULL
    #   )
    # }
  })
  
  
  
  
  observe({
    
    if(input$checkboxRepvar){
      showTab("tabs", target = "Summarized Data")
      showTab("tabs", target = "Distribution")
    } else {
      hideTab("tabs", target = "Summarized Data")
      hideTab("tabs", target = "Distribution")
    }
    
  })
  
  
  # if there is replication variable 'data_full_norep' has one column less than 'data_full', otherwise they are identical
  data_full_norep <- reactive({
    validate(
      need(input$repvar != input$inputend, "replication variable can't be input variable")
    )
    if(input$checkboxRepvar){
      data_full() %>% select(-input$repvar)
    } else {
      data_full()
    }
  })
  
  # Pre-filter data Tab ---------------------------------------------------
  
  # display dataset as DT
  # Table in tab 'Pre-filter data'
  output$dataDT <- DT::renderDT(
    as.tibble(data_full()),
    filter = "top",
    options = list(lengthChange = FALSE, 
                   autoWidth = TRUE,
                   scrollX = TRUE
    )
  )
  
  
  reacVals <- reactiveValues()
  
  # observe({
  #   reacVals$ind_inputend <- which(colnames(data_full_norep()) == input$inputend)
  #   # reacVals$ind_outputstart <- which(colnames(data_full_norep()== input$inputend) +1)
  # })
  
  ind_inputendR <- reactive({
    which(colnames(data_full_norep()) == input$inputend)
  })
  
  ind_outputstartR <- reactive({
    ind_inputendR() + 1
  })
  
  
  # Table with all chosen filters in 'Pre-filter data' (for no-repvar scenario)
  data_prefiltered <- reactive({
    validate(
      need(input$repvar != input$inputend, "replication variable can't be input variable")
    )
    req(input$dataDT_rows_all)
    d <- data_full()
    #ind_outputstart <<- isolate(which(colnames(d) == input$inputend)+1) # which column is the last input column?
    
    # Convert output parameters to numeric values
    for(i in ind_outputstartR():ncol(d)){
      d[[i]] <- as.numeric(d[[i]])
    }
    
    # if(input$checkboxRepvar){
    # d[input$repDataDT_rows_all,]
    # } else {
    d[input$dataDT_rows_all,] # extracts rows that fit the filter choices
    # }
    
  })
  
  
  # inputsR <- reactive({
  #   colnames(data_full_norep())[1:ind_inputendR()]
  # })
  # inputs <- inputs[!(inputs %in% input$repvar)]
  
  # outputsR <- reactive({
  #   colnames(data_full_norep())[ind_outputstartR():ncol(data_full_norep())]
  # })
  # outputs <- outputs[!(outputs %in% input$repvar)]
  
  
  # names_inputsR <- reactive({
  #   nm <- colnames(data_full_norep())[1:ind_inputendR()]
  #   if(input$checkboxRepvar){
  #     nm <- nm[!(nm %in% input$repvar)]
  #   }
  #   nm
  # })
  # 
  # 
  # #outputvariables
  # names_outputsR <- reactive({
  #   colnames(data_full_norep())[ind_outputstartR():ncol(data_full_norep())]
  # })
  
  
  
  # -------------------------------------
  
  data_full_mean <- reactive({
    validate(
      need(input$repvar != input$inputend, "Sbeve")
    )
    
    # summarize DT with replication runs by averaging outputs for every setting
    
    req(input$dataDT_rows_all)
    d <- data_full()[input$dataDT_rows_all,]
    
    
    
    
    
    
    if(input$checkboxRepvar){
      
      
      
      inputs <<- colnames(data_full_norep())[1:ind_inputendR()]
      
      outputs <<- colnames(data_full_norep())[ind_outputstartR():ncol(data_full_norep())]
      
      
      
      d <- group_by_at(d, vars(inputs)) %>%
        summarise(
          
          across(everything(), get(input$repvarMethod))
        )
      
      colnames(d) <- c(inputs, input$repvar, outputs)
      
      d <- d %>% select(-input$repvar)
      
    }
    d#[input$dataDT_rows_all,]
    
    as.data.frame(d)
  })
  
  
  # overview of
  output$repDataDT <- DT::renderDT({
    
    d <- data_full_mean()
    
    if(input$checkboxRepvar)
      colnames(d) <- c(inputs, paste(input$repvarMethod, "of", outputs))
    d
  },
  filter = "top",
  options = list(lengthChange = FALSE, 
                 autoWidth = TRUE,
                 scrollX = TRUE
  ))
  
  output$dataDT_summarized <- 
    renderUI({
      if(input$checkboxRepvar){
        tagList(
          h3("Summarized Data"),
          DT::dataTableOutput("repDataDT")
        )
      }
    })
  
  
  
  
  
  
  data_filteredR <- reactive({
    if(input$checkboxRepvar){
      #req(data_full_mean)
      data_full_mean()
    } else {
      #req(data_prefiltered)
      data_prefiltered()
    }
  })
  
  
  
  # inputvariables
  names_inputsR <- reactive({
    #req(data_filteredR, ind_inputendR)
    nm <- names(data_filteredR())[1:ind_inputendR()]
    # if(input$checkboxRepvar){
    #   nm <- nm[!(nm %in% input$repvar)]
    # }
    nm
  })
  
  
  # outputvariables
  names_outputsR <- reactive({
    colnames(data_filteredR())[ind_outputstartR():ncol(data_filteredR())]
  })
  
  data_choose_defaultR <- reactive({
    data_filteredR()[names_inputsR()]
  })
  
  
  # output$test <- renderPrint({list(
  # "input$file" = input$file,
  # "length(input$file)" = length(input$file),
  # "input$checkboxExampleData" = input$checkboxExampleData
  #   "names_inputsR" = names_inputsR()
  #   ,
  #   "names_outputsR" = names_outputsR(),
  #   # "outputs" = outputs,
  #   # "inputs" = inputs,
  #   "ind_inputendR" = ind_inputendR(),
  #   "ind_outputstartR" = ind_outputstartR()
  #   ,
  #   "ncol(data_filteredR())" = ncol(data_filteredR()),
  #   "colnames(data_filteredR())" = colnames(data_filteredR()),
  #   "head(data_filteredR)" = head(data_filteredR()),
  #   "head(data_full_mean())" = head(data_full_mean()),
  #   "head(data_prefiltered())" = head(data_prefiltered()),
  #   "head(data_choose_defaultR()" = head(data_choose_defaultR())
  # )
  # })
  
  
  
  # Choose default values Tab ------------------------------------------
  output$chooseDT <- DT::renderDT({
    
    
    #input$goDT
    validate(
      need(data_full(), "no file")
    )
    
    validate(
      need(input$inputend != input$repvar, "Replication run variable can't be an input variable")
    )
    
    
    # Table displayed at start in tab 'Choose default values'
    # execute filters to create new DT upon clicking action button
    
    # if(input$checkboxRepvar){
    #   data_filteredR <- data_full_mean()
    # } else {
    #   data_filteredR <- data_prefiltered()
    # }  
    
    # which column is the last input column?
    # ind_inputend <- isolate(which(colnames(data_filteredR == input$inputend)) 
    
    # display only input columns
    
    # 
    # data_choose_defaultR <- reactive({
    #   data_filteredR()[,1:ind_inputendR()]
    # })
    
    
    
    # --------------Some choice-updates for inputs---------
    
    
    
    updateSelectizeInput(session,
                         "OC",
                         choices = names_outputsR(),
                         selected = names_outputsR()[1]
    )
    
    
    
    updateSelectInput(session,
                      "boxplotOutputVar",
                      # choices = names_outputsR()
                      choices = names_outputsR()
    )
    
    
    
    
    
    #---------------------------------------------------------------
    
    # display only columns with more than 1 unique entry
    uniques <- lapply(data_choose_defaultR(), unique)
    bUniques <- sapply(uniques, function(x) {length(x) == 1})
    data_filtered <<- data_choose_defaultR()[,which(!bUniques)]
    
    validate(
      need(ncol(data_filtered) > 1, "Data has to have more than 1 input variables with non-unique characteristics.")
    )
    
    
    for(i in colnames(data_filtered)){
      # transforms variables to factors to be able to choose 1 factor level as default value
      data_filtered[,i] <<- factor(as.factor(data_filtered[,i]))  #factor(...) drops unused factor levels from prefiltering
    }
    
    if(input$buttonDefault){
      # Let first row be standard default value combination
      data_filtered_helper <- data.frame(lapply(data_filtered, as.character), stringsAsFactor = FALSE)
      
      first_row_filters <<- paste0("'[\"", data_filtered_helper[1,], "\"]'")
      first_row_filters_string <<- paste0(
        "list(NULL, ",
        paste0("list(search = ", first_row_filters, ")", collapse = ", "),
        ")"
      )
    } else {
      first_row_filters_string <<- "NULL"
    }
    
    data_filtered
    
  },
  
  filter = "top",
  
  options = list(lengthChange = FALSE, 
                 autoWidth = TRUE, 
                 scrollX = TRUE, 
                 pageLength = 5,
                 searchCols = eval(parse(text = first_row_filters_string))
  )
  #, columns = list(search = "applied")
  
  )
  
  #-----------------------------------------------------------------
  
  
  
  # Vector column filter choices ------------------------------------------
  
  search_vector <- reactive({
    req(input$chooseDT_search_columns)
    
    
    vNamedSearch <- input$chooseDT_search_columns
    names(vNamedSearch) <- colnames(data_filtered)
    vNamedSearch
    
  })
  
  
  # named vector with names of input variables
  # filled successively after default values are chosen from DT
  output$search <- renderPrint({
    
    search_vector()
  })
  
  
  # subsetting above vector only with variables that have been assigned default value
  defaults_input <- reactive({
    # req(input$chooseDT_search_columns)
    
    defaults_input <- search_vector()[search_vector() != ""]
    
    
    
    defaults_input
    
    # paste0(names(defaults_input),
    #        " == ",
    #        defaults_input,
    #        collapse = " & ")
    
    
    #names(defaults_input)
    
    
    
    # paste0(names(defaults_input),
    #        " == ",
    #        defaults_input,
    #        collapse = " & ")
    
    
  })
  
  # print subsetted data frame with filled default values
  
  output$defaultsInput <- renderPrint({
    
    as.data.frame(t(defaults_input()))
    #dim(as.data.frame(defaults_input()))
    
    
  })
  
  # table output in sidebar displaying chosen default values
  output$defaults_df <- renderTable({
    
    Values <- data.frame("Variable" = names(defaults_input()),
                         "Default value" = defaults_input(),
                         check.names = FALSE) # makes whitespace in header names possible
    Values
  })
  
  
  
  
  
  
  
  # Color vector specification ------------------------------------
  
  
  
  # Adding reactive Values for number of OCs and names of OCs
  
  # observe({
  #   
  #   reacVals$nOC <- ncol(data_prefiltered()) - reacVals$ind_inputend
  #   
  # })
  # 
  # 
  # observe({
  # 
  #   reacVals$names_outputs <- colnames(data_prefiltered()[, reacVals$ind_outputstart:ncol(data_prefiltered())])
  # })
  
  
  
  
  # --------------------------------------------------------------------------
  # dynamic number of color selectors (one for every OC) ---------------------
  
  
  nOCR <- reactive({
    ncol(data_filteredR()) - ind_inputendR()
  })
  
  
  
  output$colors_ui <- renderUI({
    
    # nWidgets <- as.integer(reacVals$nOC)
    
    nWidgetsR <- reactive({
      nOCR()
    })
    
    lapply(1:nOCR(), function(i) {
      colourInput(inputId = paste0("col", i), 
                  # label = reacVals$names_outputs[i],
                  label = names_outputsR()[i],
                  showColour = "both",
                  # value = "black"
                  value = colors()[sample(1:length(colors()),
                                          size = 1,
                                          replace = FALSE)]
      )
    })
    
    
    
    
  })
  
  lUiColors <- reactive({
    
    #nWidgets <- as.integer(reacVals$nOC)
    #names_outputs <- colnames(data_prefiltered()[,reacVals$ind_outputstart:ncol(data_prefiltered())])
    
    df_colors <- data.frame(lapply(1:nOCR(), function(i) {
      input[[paste0("col", i)]]
    }))
    
    vColors <- as.vector(t(df_colors))
    
    # names(df_colors) <- names_outputsR()
    names(vColors) <- names_outputsR()
    
    # df_colors
    vColors
    
  })
  
  
  
  
  
  
  
  
  
  
  
  # --------------------------------------------------------------------------
  # only make variables with default values available for simulation parameter choice
  
  observe({
    
    updateSelectInput(session,
                      "boxplotGroupVar",
                      choices = names_inputsR()
    )
    
    
  })
  
  observe({
    
    updateSelectInput(session,
                      "boxplotColorVar",
                      choices = names_inputsR()
    )
  })
  
  observe({
    
    updateSelectInput(session,
                      "facet_distribution_rows",
                      choices = names_inputsR()
    )
  })
  
  observe({
    
    updateSelectInput(session,
                      "facet_distribution_cols",
                      choices = names_inputsR()
    )
  })
  
  observe({
    
    updateSelectizeInput(session,
                         "facet_distribution_wrap",
                         choices = names_inputsR()
    )
  })
  
  
  
  
  observe({
    
    updateSelectInput(session,
                      "x",
                      choices = names(defaults_input())
    )
    
  })
  
  observe({
    
    updateSelectInput(session,
                      "facet_rows",
                      choices = names(defaults_input())
    )
    
  })
  
  observe({
    
    updateSelectInput(session,
                      "facet_cols",
                      choices = names(defaults_input())
    )
    
  })
  
  observe({
    
    updateSelectizeInput(session,
                         "facet_wrap",
                         choices = names(defaults_input())
    )
  })
  
  observe({
    
    updateSelectInput(session,
                      "shape",
                      choices = names(defaults_input())
    )
  })
  
  
  
  
  # 
  # updateSelectInput(session,
  #                   "facet_rows",
  #                   choices = names_inputs
  # )
  # 
  # 
  # updateSelectInput(session,
  #                   "facet_cols",
  #                   choices = names_inputs
  # )
  # 
  # updateSelectInput(session,
  #                   "shape",
  #                   choices = names_inputs
  # )
  # 
  
  
  
  
  # save default values in a list upon clicking action button
  
  # deactivated in ui -------------------------------------------
  
  # # variant with checkboxes for all variables
  # lDefault <- eventReactive(input$updateDefaultList, {as.list(data_default()[1, input$checkboxDefault])})
  # 
  
  
  
  
  # variant with automatic input of chosen filters as default values
  lDefault <- eventReactive(input$updateDefaultList, 
                            {as.list(defaults_input())})
  
  
  
  
  
  
  
  # Output of list with default values
  output$lDefault <- renderPrint({
    req(lDefault)
    print(lDefault())
  })
  
  
  # Boxplot -------------------------------------------------------------
  
  output$pBoxplot <- renderPlot({
    validate(
      need(input$repvar != input$inputend, "replication variable can't be an input variable")
    )
    # validate(
    #   need(length(defaults_input()) != 0, "Please choose default values first")
    # )
    # req(names_outputsR, data_full(), names_inputsR)
    d <- data_prefiltered()
    for(i in names_inputsR()){
      d[,i] <- as.factor(d[,i])
    }
    
    boxplot <- ggplot(d)
    
    
    
    facets_distribution <- input$facet_distribution_wrap %>% 
      str_replace_all(",", "+") %>% 
      rlang::parse_exprs()
    
    frows_distribution <- input$facet_distribution_rows %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    fcols_distribution <- input$facet_distribution_cols %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    
    # if(input$radioFacet == "grid"){
    #   p1 <- 
    #     p1 + 
    #     facet_grid(vars(get(input$facet_rows)),
    #                vars(get(input$facet_cols))
    #     )
    # }
    
    if(input$radioFacetDistribution == "grid"){
      boxplot <- 
        boxplot + 
        facet_grid(vars(!!!frows_distribution),
                   vars(!!!fcols_distribution),
                   labeller = "label_both"
        )
    }
    
    if(input$radioFacetDistribution == "wrap"){
      
      boxplot <-
        boxplot +
        facet_wrap(vars(!!!facets_distribution)
                   # facet_wrap(vars(get(!!!(input$facet_wrap)))
                   , labeller = "label_both"
        )
    }
    
    
    if(input$boxplottype == "Densityplot"){
      
      boxplot + geom_density(aes_string(fill = input$boxplotGroupVar, 
                                        col = input$boxplotColorVar, 
                                        x = input$boxplotOutputVar), 
                             alpha = input$alpha)
      
      
    } else {
      
      if(input$boxplottype == "Violinplot"){
        boxplot + geom_violin(aes_string(x = input$boxplotGroupVar,
                                         y = input$boxplotOutputVar,
                                         color = input$boxplotColorVar,
                                         fill = input$boxplotColorVar),
                              alpha = input$alpha)
      } else {
        boxplot + geom_boxplot(aes_string(x = input$boxplotGroupVar,
                                          y = input$boxplotOutputVar,
                                          color = input$boxplotColorVar,
                                          fill = input$boxplotColorVar),
                               alpha = input$alpha)
      }
      
    }
  })
  
  
  
  # PLOT -------------------------------------------------------------------
  #-----------------------------------------------------------------------------------
  
  
  # Plot Df ------------------------------------------
  
  # Data frame used for plot
  # Filters every variable for the specified default value except the chosen simulation parameters, which can have more distinguishable values
  
  df_plot <- reactive({
    
    # 1 line df with default values for variables that are checked
    
    
    # default_df <- defaults_input()
    # default_df <- as.data.frame(defaults_input())
    default_df <- defaults_input()
    
    
    
    
    # vector of names of simulation parameters
    sim_par <- input$x
    if(input$checkboxShape){
      sim_par <- c(sim_par, input$shape)
    }
    
    if(input$radioFacet == "grid"){
      sim_par <- c(sim_par, input$facet_rows, input$facet_cols)
    }
    
    if(input$radioFacet == "wrap"){
      sim_par <- c(sim_par, input$facet_wrap)
    }
    
    # exclude simulation parameters from df with default values
    default_filter <- default_df[!(names(default_df) %in% sim_par)]
    
    # default_filter <- gsub('\\[', "", default_filter)
    # default_filter <- gsub('\\]', "", default_filter)
    
    default_filter <- gsub('\\[\\"', "", default_filter)
    default_filter <- gsub('\\"\\]', "", default_filter)
    
    bedingung <- paste0(paste0("`", names(default_filter), "`"),
                        " == ",
                        paste0("'", default_filter, "'"),
                        # default_filter,
                        collapse = " & ")
    
    
    if(length(default_filter) != 0){
      df_plot <- subset(data_filteredR(), eval(parse(text = bedingung)))
    } else {
      df_plot <- data_filteredR()
    }
    
    df_plot # return data frame
    
    
    
  })
  
  
  
  output$df_plot <- DT::renderDT({
    
    df_plot()
  })
  
  
  
  # Transform dataset to long format on chosen output variables for easy plotting
  data_longer <- reactive({
    req(input$OC)
    
    d <- df_plot()
    # d <- d[input$chooseDT_rows_all,]
    
    d <- 
      d %>%
      pivot_longer(cols = input$OC,
                   names_to = "OC",
                   values_to = "value")
    d
  })
  
  # qplot -------------------------------------------------
  qplot_object <- reactive({
    
    pQplot <- qplot(1:10, 1:10)
    
    pQplot <- pQplot + geom_line(size = input$qplot_size) + theme_minimal(base_size = input$qplot_base)
    
    pQplot
  })
  
  output$Qplot <- renderPlot({
    qplot_object()
  })
  
  output$pQplot <- renderUI({
    
    plotOutput("Qplot",
               height = input$qplotheight,
               width = input$qplotwidth)
  })
  
  qplot_type <- reactive({input$qplot_type})
  
  output$download_qplot <- downloadHandler(
    
    filename = function(){paste0(input$qplot_name,
                                 ".", 
                                 input$qplot_type)},
    
    content = function(file){
      fun <- match.fun( qplot_type() )
      fun(file,
          height = input$qplotheight, 
          width = input$qplotwidth,
          res = input$qresolution)
      print(qplot_object())
      dev.off()
    }
  )
  
  
  # Plot ---------------------------------------
  
  # Plot based on which dimensions are chosen
  lineplot_object <- reactive({
    
    # output$lineplot <-renderPlot({
    
    
    
    colScale <- scale_colour_manual(values = lUiColors())
    
    p1 <- ggplot(
      data_longer(), 
      aes_string(x = input$x) 
    ) + colScale
    
    
    
    
    if(input$checkboxLine){
      p1 <- 
        p1 + 
        geom_line(aes(y = value, color  = OC),
                  size = input$linesize)
    }
    
    if(input$checkboxPoint){
      
      p1 <-
        p1  +
        geom_point(aes(y = value, color  = OC),
                   size = 3*input$linesize)
    }
    
    
    
    if(input$checkboxShape){
      p1 <-
        p1 +
        aes(
          linetype =
            factor(get(
              input$shape
            ))
          
          ,
          shape =
            factor(get(
              input$shape
            ))
          ,
          group =
            interaction(
              factor(get(
                input$shape
              )),
              OC
            )
        )
      
    } else {
      p1 <-  p1 + aes(group = OC)
    }
    
    facets <- input$facet_wrap %>% 
      str_replace_all(",", "+") %>% 
      rlang::parse_exprs()
    
    frows <- input$facet_rows %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    fcols <- input$facet_cols %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    
    # if(input$radioFacet == "grid"){
    #   p1 <- 
    #     p1 + 
    #     facet_grid(vars(get(input$facet_rows)),
    #                vars(get(input$facet_cols))
    #     )
    # }
    
    if(input$radioFacet == "grid"){
      p1 <- 
        p1 + 
        facet_grid(vars(!!!frows),
                   vars(!!!fcols),
                   labeller = "label_both"
        )
    }
    
    if(input$radioFacet == "wrap"){
      
      p1 <-
        p1 +
        facet_wrap(vars(!!!facets)
                   # facet_wrap(vars(get(!!!(input$facet_wrap)))
                   , labeller = "label_both"
        )
    }
    
    
    # THEME ---------------------------------------------
    
    plot_theme <- input$plottheme
    plot_fontsize <- input$plotfontsize
    plot_font <- input$plotfont
    
    if (plot_theme == "Grey") {
      p1 <- p1 + theme_gray(plot_fontsize, plot_font)
    }
    if (plot_theme == "White") {
      p1 <- p1 + theme_bw(plot_fontsize, plot_font)
    }
    if (plot_theme == "Linedraw") {
      p1 <- p1 + theme_linedraw(plot_fontsize, plot_font)
    }
    if (plot_theme == "Light") {
      p1 <- p1 + theme_light(plot_fontsize, plot_font)
    }
    if (plot_theme == "Minimal") {
      p1 <- p1 + theme_minimal(plot_fontsize, plot_font)
    }
    if (plot_theme == "Classic") {
      p1 <- p1 + theme_classic(plot_fontsize, plot_font)
    }
    
    
    if (plot_fontsize == 12 & plot_font == "sans" & plot_theme == "Grey") {
    }
    
    # TITLE ----------------------------------------
    
    if (input$checkboxTitle){
      p1 <- p1 + 
        labs(title = input$plot_title)  + 
        theme(plot.title = element_text(colour = input$plot_title_colour,
                                        size = input$plot_title_size, 
                                        vjust = 1.5,
                                        hjust = input$plot_title_place))
      
      
      
    }
    
    # LABS ----------------------------------------
    
    if(input$checkboxAxis){
      
      p1 <- p1 +
        labs(x = input$xLab,
             y = input$yLab)
    }
    
    p1 <- p1 + theme(legend.title = element_blank())
    
    # if(input$plottype){
    #   
    # p2 <- ggplotly(p1)
    # p2
    # 
    # } else {
    
    # p2 <- ggplotly(p1)
    # p2 %>% layout(legend = list(x = input$xLegend, y = input$yLegend))
    
    # }
    
    # if(input$plottype){
    #   ggplotly(p1)
    # } else {
    #   p1
    # }
    
    p1
    
  },
  
  # res = exprToFunction(input$resolution)
  # res = input$resolution
  
  
  )
  
  
  
  
  observe({
    
    if(input$plottype){
      
      output$lineplotly <- renderPlotly({
        ggplotly(lineplot_object())
      })
      
    } else {
      
      output$lineplot <- renderPlot({
        lineplot_object()
      })
      
    }
    
  })
  
  
  
  
  output$lineplot_ui <- renderUI({
    
    validate(
      need(input$OC, "no OCs chosen")
    )
    validate(
      need(input$x, "please specify default values first")
    )
    
    # plotOutput("lineplot",
    #            height = input$plotheight,
    #            width = input$plotwidth
    # )
    
    if(input$plottype){
      plotlyOutput("lineplotly",
                   height = input$plotheight,
                   width = input$plotwidth
      )
    } 
    #if(!input$plottype){
    else{
      plotOutput("lineplot",
                 height = input$plotheight,
                 width = input$plotwidth
      )
    }
    
  })
  
  
  download_type <- reactive({input$download_type})
  
  output$download_plot <- downloadHandler(
    
    filename = function(){paste0(input$download_name,
                                 ".", 
                                 input$download_type)},
    
    content = function(file){
      fun <- match.fun(download_type() )
      fun(file,
          height = input$plotheight, 
          width = input$plotwidth,
          res = input$resolution)
      print(lineplot_object())
      dev.off()
    }
    
    
    
    
  )
  
}


shinyApp(ui = ui, server = server
         # , options = list(launch.browser = TRUE)
)

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

