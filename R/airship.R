#' Runs the app "AIRSHIP".
#'
#' @param dfData Dataset that should be plotted by Airship; can be NULL if 
#'               upload should be done within the app.
#'               
#' @param cLastInputVar   Optional and only useful in combination with dfData. 
#'                        Character name of last input variable.
#'                      
#' @param cReplicationVar Optional and only useful in combination with dfData. 
#'                        Character name of simulation replication variable.
#'                        
#' @param bIsFacts Boolean variable; is the supplied dfData a FACTS aggregated simulation file.
#' 
#' @return No return value
#'
#' @examples
#'
#' \dontrun{
#' airship()
#' airship(data)
#' }
#' 
#' # See Vignette.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "%+%"
#' @importFrom rlang "!!"
#' @importFrom stats "sd"
#' @importFrom stats "median"
#' 
#' @export
airship <- function(
 dfData = NULL,
 cLastInputVar = NULL,
 cReplicationVar = NULL,
 bIsFacts = FALSE
) {
  
  # Error messages ----
  
  if (
      !is.null(dfData) && 
      (
       !is.data.frame(dfData) | 
       length(dfData) < 2
      )
    ) {
    stop(
      "airship(): ", 
      "Please provide a data frame with at least two columns.", 
      call. = FALSE
    )
  }
  
  if (
    !is.null(dfData) &&
    !is.null(cReplicationVar) &&
    length(dfData) < 3
  ) {
    stop(
      "airship(): ", 
      "Please provide a data frame with at least three columns if one column represents simulation replications.", 
      call. = FALSE
    )
    
  }
  
  if (!is.null(cLastInputVar)) {
    if (!cLastInputVar %in% colnames(dfData))
    {
      stop(
        "airship(): ", 
        cLastInputVar,
        " is not a column in the supplied dataset.",
        call. = FALSE
      )
    }
  }
  
  if (!is.null(cReplicationVar)) {
    if (!cReplicationVar %in% colnames(dfData))
    {
      stop(
        "airship(): ", 
        cReplicationVar,
        " is not a column in the supplied dataset.",
        call. = FALSE
      )
    }
  }
  
  if (
    is.null(dfData) &&
    any(c(!is.null(cReplicationVar), !is.null(cLastInputVar)))
  ){
    stop(
      "airship(): ", 
      "cLastInputVar or cReplicationVar were provided without dfData.",
      call. = FALSE
    )
  }
  
  # Install dependencies --------
  dependencies <- c(
    "shiny",
    "DT",
    "shinybusy",
    "plotly",
    "dplyr",
    "tidyselect",
    "tidyr",
    "stringr",
    "shinyBS",
    "colourpicker",
    "shinyWidgets",
    "shinydashboard",
    "scales",
    "Cairo",
    "ggplot2",
    "rlang",
    "magrittr",
    "shinyjs"
  )
  
  "%>%" <- dplyr::"%>%"
  
  ind_missing_package <- !dependencies %in% utils::installed.packages()[ ,"Package"]
  
  if (any(ind_missing_package)) {
    n_missing_packages <- sum(ind_missing_package)
    stop(
      "Some dependencies are missing. Please install ",
      ifelse(
        n_missing_packages == 1, 
        "the", 
        "these"
      ),
      " missing package",
      ifelse(
        n_missing_packages > 1, 
        "s", 
        ""
      ),
      " by running :\n ",
      "install.packages(",
      ifelse(
        n_missing_packages > 1, 
        "c('", 
        "'"
      ),
      paste0(
        "", 
        dependencies[ind_missing_package], 
        collapse = "','"
      ),
      ifelse(
        n_missing_packages > 1, 
        "'))", 
        "')"
      )
    )
  }
  
  # Global Options ----
  options(shiny.sanitize.errors = FALSE) 
  options(shiny.maxRequestSize = 100*1024^2)
  options(shiny.usecairo = TRUE)
  options(shiny.reactlog = TRUE) 
  
  
  # CSS ----
  # css needed for scrollbar placement in DataTables to appear on top
  css <- 
    shiny::HTML(
      "#dataDT > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
    transform:rotateX(180deg);
    }
    #dataDT > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
    transform:rotateX(180deg);
    }
    #repDataDT > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
    transform:rotateX(180deg);
    }
    #repDataDT > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
    transform:rotateX(180deg);
    }
    #chooseDT > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
    transform:rotateX(180deg);
    }
    #chooseDT > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
    transform:rotateX(180deg);
    }
    #df_plot > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
    transform:rotateX(180deg);
    }
    #df_plot > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
    transform:rotateX(180deg);
    }"
    )
  
  
  # UI ----
  ui <- 
    
    shinydashboard::dashboardPage(
      # title in browser window tab
      shinydashboard::dashboardHeader(
        title = "AIRSHIP", 
        titleWidth = "210px"
      ), 
      
      ## Sidebar -----
      shinydashboard::dashboardSidebar(
        shinyjs::useShinyjs(),
        width = "210px",
        
        shinydashboard::sidebarMenu(
          id = "sidebarMenu",
          
          ### Data ----               
          shinydashboard::menuItem(
            text = "Data",
            tabName = "data",
            icon = shiny::icon("gear")
          ),
          
          ### Default Values ----
          shinydashboard::menuItem(
            text = "Focus Variables", 
            tabName = "default", 
            icon = shiny::icon("layer-group")
          ),
          
          ### Boxplot ----
          shiny::conditionalPanel(
            condition = "input.checkboxRepvar != 0",
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "Boxplot", 
                tabName = "boxplot", 
                icon = shiny::icon("chart-area")
              )
            )
          ),
          
          ### Scatterplot ----
          shiny::conditionalPanel(
            condition = "input.checkboxRepvar != 0",
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "Scatterplot", 
                tabName = "scatterplot", 
                icon = shiny::icon("braille")
              )
            )
          ),
          
          ### Line/Dotplot ----
          shinydashboard::menuItem(
            text = "Line/Dotplot", 
            tabName = "ldplot", 
            icon = shiny::icon("chart-line")
          ),
          
          ### Help ----
          shinydashboard::menuItem(
            selected = TRUE,
            text = "Help",
            tabName = "help",
            icon = shiny::icon("question")
          ),
          
          shiny::hr()
        )
        
      ),
      
      ## Body ----
      shinydashboard::dashboardBody(
        
        ### Busy Spinner ----
        shiny::tags$head(shiny::tags$style(css)),
        shinybusy::add_busy_spinner(spin = "fading-circle"),
        shinybusy::add_busy_bar(color = "red", height = "8px"),
        
        shinydashboard::tabItems(
          
          ### Data ----
          shinydashboard::tabItem(
            tabName = "data",
            
            shiny::fluidRow(
              shiny::column(
                width = 4,
                
                shiny::checkboxInput(
                  inputId = "checkboxExampleData", 
                  label = "Use example dataset"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxExampleData == 1",
                  
                  shiny::selectInput(
                    inputId = "selectExampleData", 
                    label = "Which example dataset?",
                    choices = 
                      c(
                        "NASH platform trial design",
                        "Toy simulation study"
                      )
                  )
                ), 
                
                shiny::conditionalPanel(
                  condition = "input.checkboxExampleData == 0",
                  
                  shiny::checkboxInput(
                    inputId = "checkboxFactsData", 
                    label = "Use FACTS aggregated simulations"
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxFactsData == 1",
                  
                    shiny::checkboxInput(
                      inputId = "checkboxFactsConvertNA", 
                      label = "Convert -9999 values to NA"
                    ),
                  
                  )
                  
                ), 
                
              ),
              
              shiny::column(
                width = 4,
                
                shiny::conditionalPanel(
                  condition = "input.checkboxExampleData == 0",
                  
                  shiny::fileInput(
                    inputId = "file", 
                    label = "Choose file to upload (max 100MB)"
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxFactsData == 0",
                    
                    shiny::radioButtons(
                      inputId = "sep", 
                      label = "Csv-Separator", 
                      choiceValues = c(",", ";", ""),
                      choiceNames = c(",", ";", "whitespace")
                    ),
                    
                    shiny::numericInput(
                      inputId = "rowSkip",
                      label = "Initial rows to skip",
                      value = 0,
                      min = 0, 
                      step = 1
                    ),
                    
                    shiny::selectInput(
                      inputId = "inputend", 
                      label = "Select last input variable", 
                      choices = NULL
                    ),
                    
                  ),
                  
                ),
                
              ),
              
              shiny::column(
                width = 4,
                
                shiny::conditionalPanel(
                  condition = "input.checkboxExampleData == 0 && input.checkboxFactsData == 0",
                  
                  shiny::checkboxInput(
                    inputId = "checkboxRepvar",
                    label = "Summarize over individual simulations?"
                  )
                  
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxRepvar != 0",
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxExampleData == 0 && input.checkboxFactsData == 0",
                    
                    shiny::selectInput(
                      inputId = "repvar",
                      label = "Select the simulation run variable",
                      choices = NULL
                    ),
                    
                  ),
                  
                  shiny::selectInput(
                    inputId = "repvarMethod",
                    label = "Select the summary method you want to apply to your data",
                    choices = c("mean", "median")
                  ),
                  
                  shiny::selectInput(
                    inputId = "deviationMethod",
                    label = "Select the dispersion you want to calculate",
                    choices = c("sd", "sem")
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.deviationMethod == 'sem'",
                    
                    shiny::numericInput(
                      inputId = "sem_mult",
                      label = "multiply with",
                      value = 1.96,
                      step = 0.1,
                      min = 0.001
                    )
                    
                  )
                  
                )
                
              ),
              
            ),
            
            shiny::hr(),
            shiny::br(),
            
            shiny::h3("Original Data"),
            DT::dataTableOutput("dataDT"),
            shiny::uiOutput("dataDT_summarized")
          ),
          
          ### Default Values ----
          
          shinydashboard::tabItem(
            tabName = "default",
            shiny::br(),
            shiny::h4("Please choose a subset (at least one) of the input variables as focus variables."),
            shiny::HTML("Focus variables can be investigated further in the plot tabs. By specifying a default value for input variables, they are treated as focus variables."),
            shiny::br(),
            shiny::HTML("If focus variables are not chosen to be displayed in a plot, the displayed dataset is filtered according to the chosen default values."),
            shiny::br(),
            shiny::br(),
            
            shiny::actionButton(
              inputId = "buttonDefault", 
              label = "Take first row as default values"
            ),
            shiny::actionButton(
              inputId = "buttonDefaultHighlighted", 
              label = "Take highlighted row as default values"
            ),
            shiny::actionButton(
              inputId = "buttonResetDefault", 
              label = "Reset selections"
            ),
            DT::dataTableOutput("chooseDT")
          ),
          
          ### Boxplot ----
          shinydashboard::tabItem(
            tabName = "boxplot",
            
            shiny::fluidRow(
              shiny::column(
                width = 3,
                airship:::fnDefaultValueDropdownUI("boxplot"),
                shiny::hr(),
                airship:::fnTogglePlotlyUI("boxplot"),
                airship:::fnStyleOptionsUI("boxplot"),
                airship:::fnDownloadUI("boxplot"),
                shiny::hr()
              ),
              
              shiny::column(
                width = 3,
                airship:::fnXYUI("boxplot"),
                airship:::fnFacetGridUI("boxplot"),
                
              ),
              
              shiny::column(
                width = 3,
                airship:::fnBoxplotOutlierUI("boxplot"),
                airship:::fnColorUI("boxplot")
              ),
              
              shiny::column(
                width = 3,
                airship:::fnBoxplotUI("boxplot")
              ),
              
            ),
            
            airship:::fnStandardOutputUI("boxplot"),
            
          ),
          
          ### Scatterplot ----
          shinydashboard::tabItem(
            tabName = "scatterplot",
            
            shiny::fluidRow(
              shiny::column(
                width = 3,
                airship:::fnDefaultValueDropdownUI("scatterplot"),
                shiny::hr(),
                airship:::fnTogglePlotlyUI("scatterplot"),
                airship:::fnStyleOptionsUI("scatterplot"),
                airship:::fnDownloadUI("scatterplot"),
                shiny::hr()
              ),
              
              shiny::column(
                width = 3,
                airship:::fnXYUI("scatterplot"),
                airship:::fnFacetGridUI("scatterplot"),
                
              ),
              
              shiny::column(
                width = 3,
                airship:::fnColorUI("scatterplot")
              ),
              
              shiny::column(
                width = 3,
                airship:::fnScatterplotUI("scatterplot")
              ),
              
            ),
            
            airship:::fnStandardOutputUI("scatterplot"),
            
          ),
          
          ### Line/Dotplot 2 ----
          shinydashboard::tabItem(
            tabName = "ldplot",
            
            shiny::fluidRow(
              shiny::column(
                width = 3,
                airship:::fnDefaultValueDropdownUI("ldplot"),
                shiny::hr(),
                airship:::fnTogglePlotlyUI("ldplot"),
                airship:::fnStyleOptionsUI("ldplot"),
                airship:::fnDownloadUI("ldplot"),
                shiny::hr()
              ),
              
              shiny::column(
                width = 3,
                airship:::fnXYUI("ldplot"),
                airship:::fnFacetGridUI("ldplot"),
                
              ),
              
              shiny::column(
                width = 3,
                airship:::fnLDplotColorUI("ldplot")
              ),
              
              shiny::column(
                width = 3,
                airship:::fnLDplotUI("ldplot")
              ),
              
            ),
            
            airship:::fnStandardOutputUI("ldplot"),
            
          ),
          
          ### HELP ----
          shinydashboard::tabItem(
            tabName = "help",
            
            shiny::h2("Thank you for using AIRSHIP!"),
            shiny::HTML("Get started by clicking on the 'Data' tab and then choose/upload a dataset."),
            shiny::h3("Resources"),
            shiny::HTML(
              "For more information on the app, as well as instructions on how to use it, please refer to either the 
              <a target='_blank' rel='noopener' href='https://el-meyer.github.io/airship/articles/AIRSHIP-vignette.html'> Vignette</a>,
              <a target='_blank' rel='noopener' href='https://github.com/el-meyer/airship'> Github repository</a> or
              <a target='_blank' rel='noopener' href='https://www.softxjournal.com/article/S2352-7110(23)00043-2/fulltext'> SoftwareX publication</a>."
            )
          )
        )
      )
    )   
  
  
  
  # Server ----
  server <- function(
    session, 
    input, 
    output
  ){
    
    ## dfData check ----
    ## Check if dataset was provided and if yes, hide upload
    if (!is.null(dfData)) {
      
      shinyjs::hide(id = "checkboxExampleData")
      shinyjs::hide(id = "checkboxFactsData")
      shinyjs::hide(id = "file")
      shinyjs::hide(id = "sep")
      shinyjs::hide(id = "rowSkip")
      
      shiny::updateSelectInput(
        session = session, 
        inputId = "inputend", 
        choices = colnames(dfData)
      )
      
      shiny::updateSelectInput(
        session = session, 
        inputId = "repvar", 
        choices = colnames(dfData)
      )
      
      if (!is.null(cLastInputVar)) {
        
        shinyjs::hide(id = "inputend")
        
        shiny::updateSelectInput(
          session = session, 
          inputId = "inputend", 
          choices = colnames(dfData),
          selected = cLastInputVar
        )
        
      }
      
      if (!is.null(cReplicationVar)) {
        
        shiny::updateCheckboxInput(
          session = session, 
          inputId = "checkboxRepvar", 
          value = TRUE
        )
        
        shinyjs::hide(id = "checkboxRepvar")
        
        shiny::updateSelectInput(
          session = session, 
          inputId = "repvar", 
          choices = colnames(dfData),
          selected = cReplicationVar
        )
        
        shinyjs::hide(id = "repvar")
        
      }
      
      if (bIsFacts) {
        
        shiny::updateCheckboxInput(
          session = session, 
          inputId = "checkboxFactsData", 
          value = TRUE
        )
        
      }
      
    }

    ## Upload Data Input ----
    # widget for user data upload
    upload <- shiny::reactive({
      
      shiny::validate(
        # if no file is uploaded yet "no file" appears everywhere upload() is called
        shiny::need(
          input$file, 
          "No file uploaded."
        )
      )
      
      # file = user uploaded file in tab Data Settings
      inFile <- input$file
      
      if (input$checkboxFactsData == 0) {
        
        dfCandidate <-
          try(
            utils::read.csv(
              inFile$datapath,
              header = TRUE,
              sep = input$sep,
              skip = input$rowSkip,
              stringsAsFactors = TRUE
            )
          )
        
      } else {
        
        headrows <- readLines(
          inFile$datapath,
          n = 5
        )
        
        deleterows <- length(
          grep(
            '^ *#', 
            headrows
          )
        ) - 1
        
        dfCandidate <-
          try(
            utils::read.csv(
              inFile$datapath, 
              header = TRUE, 
              sep = input$sep,
              skip = deleterows, 
              stringsAsFactors = TRUE
            )
          )
        
          colnames(dfCandidate) <- sub(
            'X.', 
            '', 
            colnames(dfCandidate)
          )
          
          if (!'Sim' %in% colnames(dfCandidate)) {
            if ('Number' %in% colnames(dfCandidate)) {
              colnames(dfCandidate)[colnames(dfCandidate) == 'Number'] <- 'Sim'
            } else {
              stop('The dataset contains neither a `Sim` nor a `Number` column')
            }
          }

          if (input$checkboxFactsConvertNA) {
            dfCandidate[dfCandidate == -9999] <- NA
          }
        
      }

      # Get rid of columns without names
      if ("X" %in% colnames(dfCandidate)) {
        dfCandidate <-
          dfCandidate[, -which(colnames(dfCandidate) == "X")]
      }
      
      # Get rid of empty columns
      dfCandidate <- dfCandidate[,colSums(is.na(dfCandidate)) < nrow(dfCandidate)]
      
      # If Facts data, get rid of any "Flags" or "Random.Number.Seed" columns
      if (input$checkboxFactsData == 1) {
        
        if ("Flags" %in% colnames(dfCandidate)) {
          dfCandidate <- subset(dfCandidate, select = -c(`Flags`))
        }
        
        if ("Random.Number.Seed" %in% colnames(dfCandidate)) {
          dfCandidate <- subset(dfCandidate, select = -c(`Random.Number.Seed`))
        }
        
      }
      
      # Return dfCandidate
      dfCandidate
      
    })
    
    
    ## Create Dataset ----
    # Dataset to be used (before possible aggregation)
    # Use example data if checkbox is checked, otherwise use either supplied
    # or uploaded dataset
    # Update Input choices
    data_full <- shiny::reactive({
      
      reacVals$first_row_filters_string <- "NULL"
      
      # Force users to go back to data tab after modifying the data
      shinydashboard::updateTabItems(
        session = session,
        inputId = "sidebarMenu",
        selected = "data"
      )
      
      # Check if Dataset was provided via console or not
      if (!is.null(dfData)) {
        
        # If Facts data, get rid of any "Flags" and "Random.Number.Seed" columns
        if (bIsFacts) {
          
          if ("Flags" %in% colnames(dfData)) {
            dfData <- subset(dfData, select = -c(`Flags`))
          }

          if ("Random.Number.Seed" %in% colnames(dfData)) {
            dfData <- subset(dfData, select = -c(`Random.Number.Seed`))
          }
          
          if (input$checkboxFactsConvertNA) {
            dfData[dfData == -9999] <- NA
          }
          
        }
        
        # Get rid of columns without names
        if ("X" %in% colnames(dfData)) {
          dfData <-
            dfData[, -which(colnames(dfData) == "X")]
        }
        
        # Get rid of empty columns
        dfData <- dfData[,colSums(is.na(dfData)) < nrow(dfData)]
        
        return(dfData)
        
      } else {
        
        # If example data is chosen, update other input options
        # as they are hidden automatically in GUI
        
        if(input$checkboxExampleData){
          
          if (input$selectExampleData == "NASH platform trial design") {
            
            # ExampleData2 exists in package airship
            exampleData <- airship::ExampleData2
            
            # Get column names
            col_names_example_dat <- colnames(exampleData)
            
            # Update inputs
            shiny::updateCheckboxInput(
              session = session,
              inputId = "checkboxFactsData",
              value = FALSE
            )
            
            shiny::updateCheckboxInput(
              session = session,
              inputId = "checkboxRepvar",
              value = FALSE
            )
            
            shiny::updateSelectInput(
              session = session, 
              inputId = "inputend", 
              choices = col_names_example_dat,
              selected = "TreatmentEfficacySetting"
            )
            
            shiny::updateSelectInput(
              session = session,
              inputId = "repvar",
              choices = col_names_example_dat
            )
            
          } else  if (input$selectExampleData == "Toy simulation study") {
            
            # ExampleData1 exists in package airship
            exampleData <- airship::ExampleData1
            
            # Get column names
            col_names_example_dat <- colnames(exampleData)
            
            # Update inputs
            shiny::updateCheckboxInput(
              session = session,
              inputId = "checkboxFactsData",
              value = FALSE
            )
            
            shiny::updateCheckboxInput(
              session = session,
              inputId = "checkboxRepvar",
              value = TRUE
            )
            
            shiny::updateSelectInput(
              session = session, 
              inputId = "inputend", 
              choices = col_names_example_dat,
              selected = "input4"
            )
            
            shiny::updateSelectInput(
              session = session,
              inputId = "repvar",
              choices = col_names_example_dat,
              selected = "replications"
            )
            
          }
          
          return(exampleData)
          
          
        } else {
          # If not using default data, differentiate between behavior when 
          # FACTS data is used vs. custom uploaded data
          
          # Get column names
          col_names_upload <- colnames(upload())
          
          # FACTS Data
          if (input$checkboxFactsData == 1) {
            
            shiny::updateCheckboxInput(
              session = session,
              inputId = "checkboxRepvar",
              value = TRUE
            )
            
            shiny::updateSelectInput(
              session = session,
              inputId = "inputend",
              choices = col_names_upload,
              selected = "Agg.Timestamp"
            )
            
            shiny::updateSelectInput(
              session = session,
              inputId = "repvar",
              choices = col_names_upload,
              selected = "Sim"
            )
            
            
          } else {
            # Custom Data
            
            # Update inputs
            
            shiny::updateCheckboxInput(
              session = session,
              inputId = "checkboxRepvar",
              value = FALSE
            )
            
            shiny::updateSelectInput(
              session = session,
              inputId = "inputend",
              choices = col_names_upload
            )
            
            shiny::updateSelectInput(
              session = session,
              inputId = "repvar",
              choices = col_names_upload,
              selected = col_names_upload[1]
            )
            
          }
          
          return(upload())
        }
        
      }
      
    })
    
    ## Aggregation ----
    
    # Show Boxplot and Scatterplot tab only if replication is chosen above
    shiny::observe({
      if(input$checkboxRepvar){
        
        shiny::showTab(
          inputId = "tabs", 
          target = "boxplot", 
          select = FALSE, 
          session = session
        )
        shiny::showTab(
          inputId = "tabs", 
          target = "scatterplot", 
          select = FALSE, 
          session = session
        )
        
      } else {
        
        shiny::hideTab(
          inputId = "tabs", 
          target = "boxplot",  
          session = session
        )
        shiny::hideTab(
          inputId = "tabs", 
          target = "scatterplot", 
          session = session
        )
      }
    })
    
    ## Final unmodified Dataset -----
    # if there is replication variable 'data_full_norep' has one column less than 'data_full', otherwise they are identical
    data_full_norep <- shiny::reactive({
      
      shiny::validate(
        shiny::need(
          input$repvar != input$inputend, 
          "Replication variable can't be input variable (Please alter last input variable or replication variable)"
        )
      )
      
      tryCatch({
        if(input$checkboxRepvar){
          data_full() %>% dplyr::select(-input$repvar)
        } else {
          data_full()
        }
      }, error = function(e) {
        err_ <- ""
        shiny::validate(
          shiny::need(
            err_ != "", 
            "Something went wrong. If changing some settings does not solve the problem, re-visit the data tab, re-upload the data and wait for computations and then re-define focus variables. If this error persists, contact the package maintainer."
          )
        )
      })
      
    })
    
    
    ## Pre-DV data Tab ----------------------------------------------
    
    # display dataset as DT
    # Table in tab 'Pre-filter data'
    output$dataDT <- DT::renderDataTable({
      shiny::req(data_full())
      data_full()},
      colnames = paste0(
        colnames(data_full()), 
        "\n", 
        "(", sapply(data_full(), function(x) class(x)[1]), ")"
      ),
      filter = "top",
      extensions = "ColReorder", 
      options = list(
        columnDefs = list(
          list(
            className = 'dt-center', 
            targets = 0:ncol(data_full())
          )
        ),
        lengthChange = FALSE, 
        autoWidth = TRUE,
        scrollX = TRUE,
        colReorder = TRUE
      )
    )
    
    
    # reacVals 
    # add first_row_filters_string
    reacVals <- shiny::reactiveValues(
      first_row_filters_string = "NULL"
    )
    
    # index of last input variable
    ind_inputendR  <- shiny::reactive({
      which(
        colnames(
          data_full_norep()
        ) == input$inputend
      )
    })
    
    # index of first output variable
    # Make sure that such a variable exists
    ind_outputstartR <- shiny::reactive({
      shiny::validate(
        shiny::need(
          ncol(
            data_full_norep()
          ) != ind_inputendR(),
          "You have selected the last input variable to be the last column in the dataset. This is impossible, because it does not leave any outcome variables."
        )
      )
      ind_inputendR() + 1
    })
    
    
    # Table with all chosen filters in 'Pre-filter data'
    # I.e. Data pre DV tab
    data_prefiltered <- shiny::reactive({
      shiny::req(ind_outputstartR())
      shiny::req(data_full())
      shiny::validate(
        shiny::need(
          input$repvar != input$inputend, 
          "Replication variable can't be input variable (Please alter last input variable or replication variable)"
        )
      )
      shiny::req(input$dataDT_rows_all)
      d <- data_full()
      # ind_outputstart <<- isolate(which(colnames(d) == input$inputend)+1) # which column is the last input column?
      
      # Convert output parameters to numeric values
      for(i in ind_outputstartR():ncol(d)){
        d[[i]] <- as.numeric(d[[i]])
      }
      
      # if(input$checkboxRepvar){
      # d[input$repDataDT_rows_all,]
      # } else {
      
      # extracts rows that fit the filter choices
      d[input$dataDT_rows_all,] 
      # }
      
    })
    
    
    # Define sem function to calculate sem used in deviation method when using replication variable
    sem <- function(x) {
      sd(x) / sqrt(length(x))
    }
    
    # data_agg ----
    # aggregate data (if not aggregated yet)
    data_agg <- shiny::reactive({
      
      shiny::validate(
        shiny::need(
          !is.integer(input$repvar), 
          "Replication variable can't be input variable (Please alter last input variable or replication run variable)"
        )
      )
      
      shiny::validate(
        shiny::need(
          input$repvar != input$inputend, 
          "You have selected the last input variable to be the last column in the dataset. This is impossible, because it does not leave any outcome variables."
        )
      )
      
      # summarize DT with replication runs by averaging outputs for every setting
      shiny::req(input$dataDT_rows_all)
      shiny::req(data_full())
      # This makes sure that only the rows that the user selected in the DT
      # interface of the uploaded dataset get passed to the aggregated data
      d <- data_full()[input$dataDT_rows_all,]
      # d <- data_full()
      # print(input$dataDT_rows_all)
      
      if(input$checkboxRepvar){
        
        shiny::req(data_full_norep())
        shiny::req(ind_inputendR())
        shiny::req(ind_outputstartR())      
        
        inputs <<- colnames(
          data_full_norep()
        )[1:ind_inputendR()]
        
        outputs <<- colnames(
          data_full_norep()
        )[ind_outputstartR():ncol(data_full_norep())]
        
        output_class <- sapply(
          d[outputs], 
          class
        )
        
        if (any(output_class != "numeric" & output_class != "integer")) {
          
          ind_not_num <- which(
            output_class != "numeric" & output_class != "integer"
          )
          
          vars_not_num <- outputs[ind_not_num]
          n_vars <- length(vars_not_num)
          
          xx <- paste0(
            "Currently `", 
            paste0(vars_not_num, collapse = "`, "), 
            "`",
            ifelse(n_vars == 1, " variable is", " variables are"),
            " defined as output ",
            ifelse(n_vars == 1, "variable", "variables"),
            ifelse(n_vars == 1, " but is not of class numeric", " but are not of class numeric")
          ) 
          
          text_err <- paste0(
            "All defined outcome variables must be of class numeric. If this error persits even when the outcome variables are of class numeric, contact the package maintainer.", 
            xx
          )
          
          shiny::validate(
            shiny::need(
              all(output_class == "numeric"), 
              text_err
            )
          )
        }
        
        custom_list <- 
          list(
            agg1 = get(input$repvarMethod),
            agg2 = get(input$deviationMethod)
          )
        
        names(custom_list) <- 
          c(
            input$repvarMethod,
            input$deviationMethod
          )
        
        d <- 
          dplyr::group_by_at(
            d, 
            dplyr::vars(inputs)
          ) %>%
          dplyr::summarise(
            dplyr::across(
              tidyselect::everything(),
              custom_list
            )
          )
        
        # d <- tryCatch({
        #   suppressMessages(group_by_at(d, ggplot2::vars(inputs)) %>%
        #                      summarise(across(
        #                        everything(),
        #                        custom_list
        #                      )))
        # }, warning = function(w) {
        #   err_ <- ""
        #   shiny::validate(
        #     shiny::need(err_ != "", "Replication variable must be an integer variable")
        #   )}
        # )
        
        d <- 
          d %>% 
          dplyr::select(
            -paste0(
              input$repvar, 
              "_", 
              input$repvarMethod
            )
          )
        
        d <- 
          d %>% 
          dplyr::select(
            -paste0(
              input$repvar, 
              "_", 
              input$deviationMethod
            )
          )
        
        d <- 
          d %>% 
          dplyr::mutate(
            dplyr::across(
              .cols = tidyselect::contains(
                paste0(
                  "_", 
                  input$deviationMethod
                )
              ), 
              ~ .x * input$sem_mult
            )
          )
      }
      
      as.data.frame(d)
    })
    
    
    ## render Data Table ----
    output$repDataDT <- 
      DT::renderDataTable({
        d <- data_agg()
        d
      },
      
      filter = "top",
      options = list(
        lengthChange = FALSE, 
        autoWidth = TRUE,
        scrollX = TRUE
      )
      )
    
    
    ## Aggregated Datatable ----
    output$dataDT_summarized <- 
      shiny::renderUI({
        if(input$checkboxRepvar){
          shiny::tagList(
            shiny::h3("Summarized Data"),
            DT::dataTableOutput("repDataDT")
          )
        }
      })
    
    ## data_filteredR ----
    data_filteredR <- shiny::reactive({
      shiny::req(data_prefiltered())
      if(input$checkboxRepvar){
        #shiny::req(data_agg)
        data_agg()
      } else {
        #shiny::req(data_prefiltered)
        data_prefiltered()
      }
    })
    
    
    ## names_inputsR ----
    # inputvariables
    names_inputsR <- shiny::reactive({
      #shiny::req(data_filteredR, ind_inputendR)
      nm <- names(
        data_filteredR()
      )[1:ind_inputendR()]
      nm
    })
    
    
    ## names_outputsR ----
    # outputvariables
    names_outputsR <- shiny::reactive({
      colnames(
        data_filteredR()
      )[ind_outputstartR():ncol(data_filteredR())]
    })
    
    
    ## names_outputsR_unaggregated ----
    names_outputsR_unaggregated <- shiny::reactive({
      
      #shiny::req(data_filteredR, ind_inputendR)
      nm <- names(
        data_prefiltered() %>% 
          dplyr::select(-input$repvar)
      )
      nm <- nm[!(nm %in% names_inputsR())]
      nm
    })
    
    ## names of focus variables -----
    cNamesFocusVar <- shiny::reactive({
      names(defaults_input())
    })
    
    
    ## data_choose_defaultR ----
    # initialize default value object with all input variables
    data_choose_defaultR <- shiny::reactive({
      data_filteredR()[names_inputsR()]
    })
    
    
    ## Default values Tab ----
    
    ### chooseDT ----
    # Choose default values Tab 
    output$chooseDT <- DT::renderDataTable({
      
      shiny::validate(
        shiny::need(
          data_full(), 
          "No file"
        )
      )
      
      # choice-updates for inputs 
      shiny::updateSelectizeInput(
        session = session,
        inputId = "OC",
        choices = names_outputsR(),
        selected = names_outputsR()[1]
      )
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "OC_scatter",
        choices = names_outputsR_unaggregated(),
        selected = names_outputsR_unaggregated()[c(1,2)]
      )
      
      shiny::updateSelectInput(
        session = session,
        inputId = "boxplotOutputVar",
        choices = names_outputsR_unaggregated()
      )
      
      if (input$checkboxFactsData == 1) {
        
        shiny::updateSelectInput(
          session = session,
          inputId = "boxplotOutputVar",
          selected = "Duration"
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "OC_scatter",
          choices = names_outputsR_unaggregated(),
          selected = c("X.Participants", "Duration")
        )
        
      }
      
      
      # display only columns with more than 1 unique entry 
      uniques <- lapply(
        data_choose_defaultR(), 
        unique
      )
      
      bUniques <- sapply(
        uniques, 
        function(x) {length(x) == 1}
      )
      
      data_filtered <<- 
        data_choose_defaultR()[,which(!bUniques), drop = FALSE]
      
      input$buttonDefault
      input$buttonResetDefault # reset selection 
      
      for(i in colnames(data_filtered)){
        # transforms variables to factors to be able to choose 1 factor level as default value
        data_filtered[,i] <<- 
          factor(
            as.factor(data_filtered[,i])
          )  #factor(...) drops unused factor levels from prefiltering
      }
      
      data_filtered
      
    },
    
    filter = "top",
    selection = "single",
    
    options = list(
      lengthChange = FALSE, 
      autoWidth = TRUE, 
      scrollX = TRUE, 
      scrollY = TRUE,
      pageLength = 5,
      searchCols = eval(parse(text = reacVals$first_row_filters_string))
    )
    
    )
    
    
    
    ### Buttons ----
    # Update Default value filters in Tab based on pre-defined buttons
    
    #### Filter for first row ----
    shiny::observeEvent(input$buttonDefault, {
      
      # Let first row be standard default value combination
      data_filtered_helper <- 
        data.frame(
          lapply(
            data_filtered, 
            as.character
          ), 
          stringsAsFactor = FALSE
        )
      
      first_row_filters <- paste0(
        "'[\"", 
        data_filtered_helper[1,], 
        "\"]'"
      )
      
      reacVals$first_row_filters_string <- paste0( 
        "list(NULL, ",
        paste0(
          "list(search = ", 
          first_row_filters, 
          ")", 
          collapse = ", "
        ),
        ")"
      )
    })
    
    shiny::observeEvent(input$buttonDefaultHighlighted, {
      
      shiny::req(input$chooseDT_rows_selected)
      ith_row <- input$chooseDT_rows_selected
      
      data_filtered_helper <- 
        data.frame(
          lapply(
            data_filtered, 
            as.character
          ), 
          stringsAsFactor = FALSE
        )
      
      first_row_filters <- paste0(
        "'[\"", 
        data_filtered_helper[ith_row,], 
        "\"]'"
      )
      
      reacVals$first_row_filters_string <- paste0( 
        "list(NULL, ",
        paste0("list(search = ", first_row_filters, ")", collapse = ", "),
        ")"
      )
    })
    
    
    #### Reset filters----
    shiny::observeEvent(input$buttonResetDefault ,{
      reacVals$first_row_filters_string <- "NULL"
    })
    
    
    #### Vector column filter choices ----
    search_vector <- shiny::reactive({
      shiny::req(input$chooseDT_search_columns)
      
      vNamedSearch <- input$chooseDT_search_columns
      names(vNamedSearch) <- colnames(data_filtered)
      vNamedSearch
    })
    
    ### searchbar ----
    # named vector with names of input variables
    # filled successively after default values are chosen from DT
    output$search <- shiny::renderPrint({
      search_vector()
    })
    
    
    ## Default values Tab----
    
    ## defaults_input ----
    # subsetting above vector only with variables that have been assigned default value
    defaults_input <- shiny::reactive({
      defaults_input <- search_vector()[search_vector() != ""]
      defaults_input
    })
    
    
    ### output ----
    # print subsetted data frame with filled default values
    output$defaultsInput <- shiny::renderPrint({
      as.data.frame(
        t(
          defaults_input()
        )
      )
    })
    
    ## Plots -------------------------------------------------------------------
    
    ### Boxplot -----------
    
    #### Observe Input changes ----
    
    shiny::observe({
      
      airship:::fnXYUpdateInput(
        cID = "boxplot",
        cNamesX = names(defaults_input()),
        cNamesY = names_outputsR_unaggregated()
      )
      
      airship:::fnFacetGridUpdateInput(
        cID = "boxplot",
        cNamesInputs = names(defaults_input())
      )
      
      airship:::fnColorUpdateInput(
        cID = "boxplot",
        cNamesInputs = names(defaults_input())
      )
      
      airship:::fnDownloadUpdateInput(
        cID = "boxplot"
      )
      
    })
    
    #### Create Color Dropdown ----
    shiny::observe({
      airship:::fnColorCreateDropdown(
        cID = "boxplot",
        dfPrefilter = data_prefiltered()
      )
    })
    
    #### Boxplot Function ----
    
    boxplot_get <- shiny::reactive({
      
      ##### Return object 
      lPlot <- 
        list(
          lCode = list(start = "ggplot2::ggplot(data)"),
          cSimPars = c()
        )
      
      ##### Initial plot data 
      lPlot$lData <-
        airship:::fnDynFilterData(
          cID = "boxplot",
          dfFilter = defaults_input(),
          dfData = data_prefiltered(),
          cSimPars = lPlot$cSimPars,
          cInputNames = names_inputsR()
        )
      
      ##### Empty plot 
      lPlot$lggPlot <-
        ggplot2::ggplot(
          lPlot$lData
        )
      
      ##### Boxplot specific function 
      lPlot <-
        airship:::fnBoxplotServer(
          cID = "boxplot",
          lPlot = lPlot
        )
      
      ##### XY 
      lPlot <- 
        airship:::fnXYServer(
          cID = "boxplot",
          lPlot = lPlot
        )
      
      ##### Facet 
      lPlot <-
        airship:::fnFacetGridServer(
          cID = "boxplot",
          lPlot = lPlot
        )
      
      ##### Color 
      lPlot <- 
        airship:::fnColorServer(
          cID = "boxplot",
          lPlot = lPlot,
          dfPrefilter = data_prefiltered()
        )
      
      ##### Style Options 
      lPlot <- 
        airship:::fnStyleOptionsServer(
          cID = "boxplot",
          lPlot = lPlot
        )
      
      ##### Final Plot Data 
      lPlot$lData <-
        airship:::fnDynFilterData(
          cID = "boxplot",
          dfFilter = defaults_input(),
          dfData = data_prefiltered(),
          cSimPars = lPlot$cSimPars,
          cInputNames = names_inputsR()
        )
      
      # replace fully filtered dataset with dynamically filtered dataset
      lPlot$lggPlot <- 
        lPlot$lggPlot %+%
        lPlot$lData
      
      ##### Return object -----
      return(lPlot)
      
    })
    
    #### Display DV Dropdown ------
    airship:::fnDefaultValueDropdownServer(
      cID = "boxplot",
      dfDefaultValues = defaults_input
    )
    
    #### Create Standard Output -----
    shiny::observe({
      airship:::fnStandardOutputServer(
        cID = "boxplot",
        lPlot = boxplot_get
      )
    })
    
    #### Download Handler ------
    airship:::fnDownloadServer(
      cID = "boxplot",
      lPlot = boxplot_get
    )
    
    
    ### Scatterplot -----------
    
    #### Observe Input changes ----
    
    shiny::observe({
      
      airship:::fnXYUpdateInput(
        cID = "scatterplot",
        cNamesX = names_outputsR_unaggregated(),
        cNamesY = names_outputsR_unaggregated()
      )
      
      airship:::fnFacetGridUpdateInput(
        cID = "scatterplot",
        cNamesInputs = names(defaults_input())
      )
      
      airship:::fnColorUpdateInput(
        cID = "scatterplot",
        cNamesInputs = names(defaults_input())
      )
      
      airship:::fnDownloadUpdateInput(
        cID = "scatterplot"
      )
      
    })
    
    #### Create Color Dropdown ----
    shiny::observe({
      airship:::fnColorCreateDropdown(
        cID = "scatterplot",
        dfPrefilter = data_prefiltered()
      )
    })
    
    #### Scatterplot Function ----
    
    scatterplot_get <- shiny::reactive({
      
      ##### Return object 
      lPlot <- 
        list(
          lCode = list(start = "ggplot2::ggplot(data)"),
          cSimPars = c()
        )
      
      ##### Initial plot data 
      lPlot$lData <-
        airship:::fnDynFilterData(
          cID = "scatterplot",
          dfFilter = defaults_input(),
          dfData = data_prefiltered(),
          cSimPars = lPlot$cSimPars,
          cInputNames = names_inputsR()
        )
      
      ##### Empty plot 
      lPlot$lggPlot <-
        ggplot2::ggplot(
          lPlot$lData
        )
      
      ##### Boxplot specific function 
      lPlot <-
        airship:::fnScatterplotServer(
          cID = "scatterplot",
          lPlot = lPlot
        )
      
      ##### XY 
      lPlot <- 
        airship:::fnXYServer(
          cID = "scatterplot",
          lPlot = lPlot
        )
      
      ##### Facet 
      lPlot <-
        airship:::fnFacetGridServer(
          cID = "scatterplot",
          lPlot = lPlot
        )
      
      ##### Color 
      lPlot <- 
        airship:::fnColorServer(
          cID = "scatterplot",
          lPlot = lPlot,
          dfPrefilter = data_prefiltered()
        )
      
      ##### Style Options 
      lPlot <- 
        airship:::fnStyleOptionsServer(
          cID = "scatterplot",
          lPlot = lPlot
        )
      
      ##### Final Plot Data 
      lPlot$lData <-
        airship:::fnDynFilterData(
          cID = "scatterplot",
          dfFilter = defaults_input(),
          dfData = data_prefiltered(),
          cSimPars = lPlot$cSimPars,
          cInputNames = names_inputsR()
        )
      
      # replace fully filtered dataset with dynamically filtered dataset
      lPlot$lggPlot <- 
        lPlot$lggPlot %+%
        lPlot$lData
      
      ##### Return object -----
      return(lPlot)
      
    })
    
    #### Display DV Dropdown ------
    airship:::fnDefaultValueDropdownServer(
      cID = "scatterplot",
      dfDefaultValues = defaults_input
    )
    
    #### Create Standard Output -----
    shiny::observe({
      airship:::fnStandardOutputServer(
        cID = "scatterplot",
        lPlot = scatterplot_get
      )
    })
    
    #### Download Handler ------
    airship:::fnDownloadServer(
      cID = "scatterplot",
      lPlot = scatterplot_get
    )
    
    
    ### LDPlot -----------
    
    #### Observe Input changes ----
    
    shiny::observe({
      
      airship:::fnXYUpdateInput(
        cID = "ldplot",
        cNamesX = names(defaults_input()),
        cNamesY = names_outputsR()
      )
      
      airship:::fnFacetGridUpdateInput(
        cID = "ldplot",
        cNamesInputs = names(defaults_input())
      )
      
      airship:::fnLDPlotUpdateInput(
        cID = "ldplot",
        cNamesInputs = names(defaults_input())
      )
      
      airship:::fnDownloadUpdateInput(
        cID = "ldplot"
      )
      
    })
    
    #### Create Color Dropdown ----
    shiny::observe({
      airship:::fnLDPlotColorCreateDropdown(
        cID = "ldplot",
        dfPrefilter = data_prefiltered(),
        cNamesOutput = names_outputsR(),
        dfSummarized = data_filteredR()
      )
    })
    
    #### Create Error Bar Dropdown ----
    # shiny::observe({
    #   airship:::fnLDPlotErrorbarUpdateInput(
    #     cID = "ldplot",
    #     cNamesOutputs = names_outputsR()
    #   )
    # })
    
    #### LDPlot Function ----

    ldplot_get <- shiny::reactive({

      ##### Return object
      lPlot <-
        list(
          lCode = list(start = "ggplot2::ggplot(data)"),
          cSimPars = c()
        )

      ##### Initial plot data
      lPlot$lData <-
        airship:::fnDynFilterData(
          cID = "ldplot",
          dfFilter = defaults_input(),
          dfData = data_filteredR(),
          cSimPars = lPlot$cSimPars,
          cInputNames = names_inputsR()
        )

      ##### Empty plot
      lPlot$lggPlot <-
        ggplot2::ggplot(
          lPlot$lData
        )

      ##### Boxplot specific function
      lPlot <-
        airship:::fnLDplotServer(
          cID = "ldplot",
          lPlot = lPlot
        )

      ##### XY
      lPlot <-
        airship:::fnXYServer(
          cID = "ldplot",
          lPlot = lPlot
        )

      ##### Facet
      lPlot <-
        airship:::fnFacetGridServer(
          cID = "ldplot",
          lPlot = lPlot
        )

      ##### Color
      lPlot <-
        airship:::fnLDPlotColorServer(
          cID = "ldplot",
          lPlot = lPlot,
          dfPrefilter = data_prefiltered(),
          dfSummarized = data_filteredR()
        )

      ##### Style Options
      lPlot <-
        airship:::fnStyleOptionsServer(
          cID = "ldplot",
          lPlot = lPlot
        )

      ##### Final Plot Data
      lPlot$lData <-
        airship:::fnDynFilterData(
          cID = "ldplot",
          dfFilter = defaults_input(),
          dfData = data_filteredR(),
          cSimPars = lPlot$cSimPars,
          cInputNames = names_inputsR()
        )

      # replace fully filtered dataset with dynamically filtered dataset
      lPlot$lggPlot <-
        lPlot$lggPlot %+%
        lPlot$lData

      ##### Return object -----
      return(lPlot)

    })

    #### Display DV Dropdown ------
    airship:::fnDefaultValueDropdownServer(
      cID = "ldplot",
      dfDefaultValues = defaults_input
    )

    #### Create Standard Output -----
    shiny::observe({
      airship:::fnStandardOutputServer(
        cID = "ldplot",
        lPlot = ldplot_get
      )
    })

    #### Download Handler ------
    airship:::fnDownloadServer(
      cID = "ldplot",
      lPlot = ldplot_get
    )
    
    ## Handle Session closing ----
    session$onSessionEnded(function() {
      shiny::stopApp()
    })
    
  }
  
  # Run Shiny App ----
  shiny::shinyApp(
    ui = ui, 
    server = server, 
    options = list(launch.browser = TRUE)
  )
  
}
