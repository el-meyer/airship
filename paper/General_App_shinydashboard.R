# Libraries ----
options(shiny.sanitize.errors = FALSE) 
options(shiny.maxRequestSize = 50*1024^2)
library(shiny)
library(readxl)
#library(xlsx)
library(DT)
library(shinybusy)
library(plotly)
library(tidyverse)
library(shinyBS)
library(colourpicker)
library(shinyWidgets)

library(bslib)
library(shinydashboard)
library(scales)
library(Cairo)
library(shinyAce)
options(shiny.usecairo=T)

library(gganimate)
library(ggplot2)
library(gifski)
library(png)


# CSS ----
# css needed for scrollbar placement in DataTables to appear on top
css <- HTML(
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
  dashboardPage(
    
    # title in browser window tab
    dashboardHeader(
      title = "Simulation Results"
    ), 
    
    
    
    ## Sidebar -----
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       id = "sidebarMenu",
                       menuItem(
                         text = "Data Settings",
                         
                         
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
                           
                           
                         ),
                         
                         checkboxInput(
                           "checkboxRepvar",
                           "Do you want to aggregate over a replication run variable?"
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
                           ),
                           
                           selectInput(
                             "deviationMethod",
                             "Select the deviation you want to calculate",
                             choices = c("sd", "sem")
                           ),
                           
                           conditionalPanel(
                             "input.deviationMethod == 'sem'",
                             numericInput(
                               "sem_mult",
                               "multiply with",
                               value = 1.96,
                               step = 0.1,
                               min = 0.001
                             )

                           )
                           
                           
                         ),
                         tabName = "data_settings",icon = icon("gear")
                       ),
                       
                       menuItem(
                         "Data",
                         tabName = "data",
                         icon = icon("database")
                       ),
                       
                       menuItem(
                         "Default values", 
                         tabName = "default", 
                         icon = icon("pen")
                       ),
                       
                       conditionalPanel(
                         "input.checkboxRepvar != 0",
                         sidebarMenu(
                           menuItem(
                             "Distribution", 
                             tabName = "distribution", 
                             icon = icon("chart-area")
                           )
                         )
                       ),
                       
                       menuItem(
                         "Plot", 
                         tabName = "plot", 
                         icon = icon("chart-line")
                       ),
                       
                       # conditionalPanel(
                       #   "input.checkboxRepvar != 0",
                       #   sidebarMenu(
                       menuItem(
                         "Scatterplot", 
                         tabName = "scatterplot", 
                         icon = icon("braille")
                       )
                       #   )
                       # )
                       ,
                       
                       menuItem(
                         "Animation", 
                         tabName = "animationTab", 
                         icon = icon("braille")
                       ),
                       
                       
                       menuItem("Help",
                                tabName = "help",
                                icon = icon("question")
                       ),
                       
                       hr()
                       # h3("Default value overview"),
                       # uiOutput("defaults_df_ui")
                       
                     )
    ),
    
    
    ## Body ----
    dashboardBody(
      ### Busy Spinner ----
      tags$head(tags$style(css)),
      add_busy_spinner(spin = "fading-circle"),
      
      
      tabItems(
        
        ### DATA SETTINGS ----
        tabItem(
          tabName = "data_settings",
        ),
        
        ### DATA ----
        tabItem(
          tabName = "data",
          DT::dataTableOutput("dataDT"),
          # conditionalPanel("input.checkboxRepvar != 0",
          #                  h3("Summarized Data"),
          #                  DT::dataTableOutput("repDataDT")
          # )
          uiOutput("dataDT_summarized")
        ),
        
        ### DEFAULT VALUES ----
        tabItem(
          tabName = "default",
          br(),
          actionButton("buttonDefault", "Take first row as default values"),

          actionButton("buttonResetDefault", "Reset selections"),

          DT::dataTableOutput("chooseDT")
          # conditionalPanel(
          # "input.checkboxExampleData",
          # )
        ),
        
        ### DISTRIBUTION ----
        tabItem(
          tabName = "distribution",
          
          fluidRow(
            column(
              4,
              
              #### Grouping Var ----
              selectInput(
                "boxplotGroupVar",
                "Select grouping variable for distribution plot",
                choices = NULL
              ),
              
              #### Transparancy ----
              sliderInput(
                "alpha",
                "Select transparency (alpha)",
                min = 0,
                max = 1,
                value = 0.1,
                step = 0.1
              ),
              
              #### Distribution Var ----
              selectInput(
                "boxplotOutputVar",
                "Select distribution variable",
                choices = NULL
              )
            ),
            
            column(
              4,
              
              #### Facet Dimension ----
              radioButtons(
                "radioFacetDistribution",
                "Do you want to add a facet dimension?",
                choices = c("no", "grid", "wrap")
              ),
              
              ##### Grid ----
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
              
              ##### Wrap ----
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
              
              #### Plottype ----
              HTML("<b>Choose plottype</b>"),
              
              radioButtons(
                "boxplottype",
                "Box-/Violin- or Density-plot",
                choices =c("Boxplot", "Violinplot", "Densityplot"),
                selected = "Boxplot",
              ),
              
              ##### Densityplot ----
              conditionalPanel(
                "input.boxplottype == 'Densityplot'",
                checkboxGroupInput(
                  "densitytype",
                  "Densityplot or Histogram",
                  choices = c("Density", "Histogram")
                )
              ),
              
              ###### Histogram ----
              conditionalPanel(
                "input.densitytype.includes('Histogram')",
                numericInput(
                  "bins",
                  "How many bins?",
                  value = 10, min = 1, max = 100, step = 1
                ),
                radioButtons(
                  "hist_position",
                  "Histogram position style",
                  choices = c("stack", "dodge", "fill")
                )
              )
              
            )
          ),
          
          plotOutput("pBoxplot")
        ),
        
        
        ### PLOT ----
        tabItem(
          tabName = "plot",
          
          fluidRow(
            column(
              10,
              #### Plot Output ----
              # plotlyOutput("lineplot")
              uiOutput("lineplot_ui"),
              #plotOutput("scatterplot")
              # plotOutput("lineplot")
            ),
            
            column(
              2,
              #### Color choice ----
              conditionalPanel(
                "input.checkboxColor == 0",
                checkboxInput(
                  "checkboxPalette_OC", 
                  "Do you want to specify your own colors for every OC?"
                ),
                ##### Brush button ----
                absolutePanel(
                  shinyWidgets::dropdownButton(
                    label = "Color Choices",
                    status = "primary",
                    circle = TRUE,
                    right = TRUE,
                    icon = icon("paint-brush"),
                    tooltip = TRUE,
                    uiOutput("colors_ui"),
                    inputId = "dropdown_colors"
                  ),
                  draggable = TRUE
                )
              ),
              
              conditionalPanel(
                "input.checkboxColor != 0",
                checkboxInput(
                  "checkboxPalette_dim",
                  # TODO: Check if need Fix: This text wont change on checking checkboxColor
                  "Do you want to specify your own colors for color dimension?"
                ),
                absolutePanel(
                  shinyWidgets::dropdownButton(
                    label = "Color choices",
                    status = "primary",
                    circle = TRUE,
                    right = TRUE,
                    icon = icon("paint-brush"),
                    uiOutput("colordim_ui"),
                    inputId = "dropdown_colordim"
                  ),
                  draggable = TRUE
                )
              )
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              3,
              
              #### Default Value overview----
              shinyWidgets::dropdown(
                label = "Default value overview",
                HTML("Current default value settings"),
                
                uiOutput("defaults_df_ui")
              ),
              
              #### X Var ----
              selectInput(
                "x", 
                "Choose x-Variable", 
                choices = NULL
              ),
              
              #### Oc to Plot ----
              selectizeInput(
                "OC", 
                "Choose OC to plot", 
                choices = NULL,
                multiple = TRUE
              ),
              
              #### add Errorbars ----
              checkboxInput("checkboxErrorbar",
                            "Do you want to add Errorbars?"),
              
              conditionalPanel(
                "input.checkboxErrorbar != 0",
                
                radioButtons(
                  "radioErrorsymmetry",
                  "Are the errors symmetrical (1 error variable) or asymmetrical (2 error variables)",
                  choices = c("symmetrical", "asymmetrical")
                ),
                
                conditionalPanel(
                  "input.radioErrorsymmetry == 'asymmetrical'",
                  
                  radioButtons(
                    "radioErrorstructure",
                    "Do the variables represent the upper and lower deviation from the estimate or the upper and lower bounds?",
                    choices = c("deviation", "bounds")
                  )
                ),
                HTML("Select the corresponding error variable (Sd) for every OC chosen."),
                HTML("For a correct display, the error variables have to be in the same order as the OCs chosen above"),
                uiOutput("errorbar_var")
              ),
              
              
              # ,
              # verbatimTextOutput("OClength")
            ),
            
            column(
              3,
              
              #### Facet Dimension ----
              radioButtons(
                "radioFacet",
                "Do you want to add a facet dimension?",
                choices = c("no", "grid", "wrap")
              ),
              
              ##### Grid ----
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
              
              ##### Wrap ----
              conditionalPanel(
                "input.radioFacet == 'wrap'",
                
                selectizeInput(
                  "facet_wrap", 
                  "Choose variables to facet wrap",
                  choices = NULL,
                  multiple = TRUE
                )
              ),
              
              #### Linetype ----
              checkboxInput(
                "checkboxLinetype",
                "Do you want to add a Linetype dimension?"
              ),
              conditionalPanel(
                "input.checkboxLinetype != 0",
                
                selectInput(
                  "linetype",
                  "Choose linetype variable",
                  choices = NULL
                )
              ),
              
              #### Color dimension ----
              conditionalPanel(
                "input.OC.length == 1",
                
                checkboxInput(
                  "checkboxColor",
                  "Do you want to add a Color dimension?"
                ),
                conditionalPanel(
                  "input.checkboxColor != 0",
                  
                  selectInput(
                    "color",
                    "Choose color variable",
                    choices = NULL
                  )
                )
              )
            ),
            
            column(
              3,
              
              #### Interactive Plot ----
              switchInput("plottype",
                          "Interactive Plot?",
                          value = FALSE,
                          size = "small"),
              
              # switchInput("scatterplot",
              #             "Scatterplot",
              #             value = FALSE,
              #             size = "small"),
              
              #### Style options ----
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
                        
                        numericInput(
                          "resolution",
                          "Resolution",
                          value = 72
                        ),
                        
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
                        "input.checkboxTheme != 0",
                        
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
              
              #### Download Plot Button ----
              actionButton("save_plot", label = "Download plot"),
              
            ),
            
            column(
              3,
              
              #### Add Titel ----
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
              
              #### Change Axis Labels ----
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
              
              #### Download Plot Window ----
              bsModal("modal", "Download plot", trigger = "save_plot", size = "medium",
                      
                      selectInput("download_type", 
                                  "Choose file type",
                                  choices = c("png", "jpeg", "tiff", "pdf")
                      ),
                      
                      selectInput("download_unit", 
                                  "Choose unit",
                                  choices = c("px", "in", "cm", "mm")
                      ),
                      
                      numericInput(
                        "download_plotwidth",
                        "Plot width",
                        value = 1000,
                        min = 1,
                        max = 2000
                      ),
                      
                      numericInput(
                        "download_plotheight",
                        "Plot height",
                        value = 600,
                        min = 1,
                        max = 2000
                      ),
                      
                      numericInput(
                        "download_resolution",
                        "Resolution",
                        value = 72,
                        min = 1, 
                        max = 1000
                      ),
                      
                      textInput("download_name", "Specify file name"),
                      
                      downloadButton("download_plot", "Download")
              )
            )
          ),
          hr(),
          
          #### Plotted Data ----
          h2("Plotted Data"),
          br(),
          fluidRow(
            # verbatimTextOutput("df_plot")
            DT::dataTableOutput("df_plot")
          ),
          
          hr(),
          
          #### Code for reproduction ----
          h2("Code for reproduction"),
          br(),

          # aceEditor(outputId = "print_code", value = "", mode = "r", theme = "texmate", readOnly = FALSE),

          br(),
          br()
        ),
        
        ### SCATTERPLOT ----
        tabItem("scatterplot",
                
                column(10,
                       
                       fluidRow(
                         ##### Scatterplot Output ----
                         uiOutput("scatter_ui"),
                         
                         ##### Infotext ----
                         HTML("In this tab you can look at the variability and scatter of two OCs by letting 1 variable take on every possible value, whereas all other variables remain at their set default value. This could for example be a replication run variable, if you want to investigate the variability of your outcome for a certain set of design parameters.")
                       ),
                       
                       fluidRow(
                         column(3,
                                
                                ##### Variability Parameter ----
                                selectInput(
                                  "repvar_scatter",
                                  "Choose variability parameter",
                                  choices = NULL
                                ),
                                
                                ##### Color param. ----
                                selectInput(
                                  "colvar_scatter",
                                  "Choose color parameter",
                                  choices = NULL
                                ),
                                
                                ##### OC to plot ----
                                selectizeInput(
                                  "OC_scatter", 
                                  "Choose OC to plot", 
                                  choices = NULL,
                                  multiple = TRUE
                                )
                         ),
                         
                         column(3,
                                
                                ##### Facet dimension ----
                                radioButtons(
                                  "radioFacet_scatter",
                                  "Do you want to add a facet dimension?",
                                  choices = c("no", "grid", "wrap")
                                ),
                                
                                ###### Grid ----
                                conditionalPanel(
                                  "input.radioFacet_scatter == 'grid'",
                                  
                                  selectInput(
                                    "facet_rows_scatter", 
                                    "Choose row variable", 
                                    choices = NULL,
                                    multiple = TRUE
                                  ),
                                  
                                  selectInput(
                                    "facet_cols_scatter", 
                                    "Choose col variable", 
                                    choices = NULL,
                                    multiple = TRUE
                                  )
                                ),
                                
                                ###### Wrap ----
                                conditionalPanel(
                                  "input.radioFacet_scatter == 'wrap'",
                                  
                                  selectizeInput(
                                    "facet_wrap_scatter", 
                                    "Choose variables to facet wrap",
                                    choices = NULL,
                                    multiple = TRUE
                                  )
                                )
                         )
                       )

                       # checkboxInput(
                       #   "checkboxShape_scatter", 
                       #   "Do you want to add a shape dimension?"
                       # ),
                       # conditionalPanel(
                       #   "input.checkboxShape_scatter != 0",
                       #   
                       #   selectInput(
                       #     "shape_scatter", 
                       #     "Choose shape variable",
                       #     choices = NULL
                       #   )
                       # )
                ),
                
                #### Color Button ----
                column(2,
                       checkboxInput(
                         "checkboxPalette_scatter",
                         "Do you want to specify your own colors"
                       ),
                       absolutePanel(
                         shinyWidgets::dropdownButton(
                           label = "Color choices",
                           status = "primary",
                           circle = TRUE,
                           right = TRUE,
                           icon = icon("paint-brush"),
                           uiOutput("colors_scatter_ui"),
                           inputId = "dropdown_colors_scatter"
                         ),
                         draggable = TRUE
                       )
                )
        ),
        
        
        ### ANIMATION Tab ----
        
        tabItem("animationTab",
                
                fluidRow(
                  column(10,
                         ##### Animation Output ----
                         imageOutput("animationOutDynamic"),
                  ),
                ),
                
                hr(),
                
                fluidRow(
                  column(5,
                         ##### Infotext ----
                         HTML("Animation Test"),
                         
                         ##### animateIteratorSelect ----
                         selectInput(
                           "animateIteratorSelect", 
                           "Choose Variable to iterate over", 
                           choices = NULL
                         ),
                         actionButton(
                           "animationRenderButton",
                           "Render animation"
                          ),
                  ),
                ),
                
                hr(),
        ),
        
        ### HELP ----
        tabItem("help",

                h4("Info"),
                HTML("This app is designed to plot simulation results of clinical trials. It has been developed by Constantin Kumaus, Elias Meyer (both Medical University Vienna) and Michal Majka"),
                
                h2("User Manual"),
                HTML("Following you will find details on every part of the app and how they are to be used"),
                
                h4("Data Settings"),
                HTML("There are a few requirements to the data in order for the app to work. So far only .csv files can be uploaded. It is expected that the data is arranged in a way such that the input variables/design parameters precede the output variables/operating characteristics. Each row represents one simulation run with a different combination of input/design parameters. "),
                HTML("If your data is not aggregated yet i.e. if you have every single simulation outcome as one row in your dataset, and a 'replication run index variable' you can click the checkbox and choose which of your variables is the 'replication run index' The dataset is then averaging over the OCs either by mean or median. Additionally the 'Distribution' tab opens where you can investigate the behaviour of your variables and outcomes."),

                
                h3("Data"),
                HTML("In the Data tab you find an overview of your data. Already here you can set filters for your input parameters, if you are not interested in some observations."),
                
                h3("Default values"),
                HTML("The default value tab is a key tab in this App. Please choose one default value for every input variable that can take on more than one unique value. Later in the plot tab the dataset is filtered for these values, unless the respective variable is chosen to be one of the dimensions in the graph (See 'plot' tab)."),
                h3("Distribution"),
                HTML("This tab only appears when the checkbox regarding 'replication run index/variables' is checked. You can create boxplots or distribution plots for your output variables to get an overview of the distribution behaviour."),
                
                h3("Plot"),
                HTML("After uploading the data and establishing the settings, you can visualize your simulation results on up to 4 dimensions. An x-axis variable as well as at least one OC have to be specified in order for the plot to show up: You can opt to add further design parameters on the 'facet' dimensions (row and column), which splits the plot into a grid as well as the 'shape' dimension, which adds lines/points in different shapes according to the value of the respective input parameter."),
                HTML("Furthermore you can change the style of your plot when clicking the 'style options' button and download a plot in the exact size and quality you need when clicking the 'Download plot' button"),
                
                h3("Scatterplot"),
                HTML("If you are interested in the variability of certain operating characteristics in 1 specific scenario, you can look at the settings in this tab which generates a scatterplot of 2 output variables, with the possibility of adding a grid. This is especially suitable if you ran e.g. 10000 simulation runs with the same setting and have not aggregated your data yet. Then you can choose your 'replication index variable' and investigate the variability of the outcome.")
        )
      )
    )
  )   



# Server ----
server <- function(session, input, output){
  
  
  # Example Data read ----
  # read in Example data and convert some variables for correct display
  exampleData <- read.csv(
    #"example_data.csv",

    "ExampleData.csv",

    header = TRUE,
    sep = ",",
    stringsAsFactors = TRUE
  )
  
  
  # Upload Data ----
  # widget for user data upload
  upload <- reactive({
    validate(
      # if no file is uploaded yet "no file" appears everywhere upload() is called
      need(input$file, "no file")
    ) 
    
    # file = user uploaded file in tab Data Settings
    inFile <-input$file 
    
    mydata <- read.csv(inFile$datapath,
                       header = TRUE,
                       sep = input$sep,
                       stringsAsFactors = TRUE)
    
    # mydata <- read_csv(inFile$datapath,
    #                    col_names = TRUE)
    
    # names(mydata) <- gsub("\\.", " ", names(mydata))
    
    
    # for(i in 1:length(input$session)){
    #   mydata[,input$session[i]] <- as.numeric(mydata[,input$session[i]])
    # }

    return(mydata) 
  })
  
  
  # data_full ----
  # Use example data if checkbox is checked, otherwise use uploaded dataset
  # Update Input choices
  data_full <- reactive({
        
    if(input$checkboxExampleData){
      updateSelectInput(session, 
                        "inputend", 
                        choices = colnames(exampleData),
                        selected = "input4"
      )
      
      updateSelectInput(session,
                        "repvar",
                        choices = colnames(exampleData),
                        selected = "replications"
      )
      
      updateSelectInput(session,
                        "repvar_scatter",
                        choices = colnames(exampleData)
      )
      
      updateSelectInput(session,
                        "colvar_scatter",
                        choices = colnames(exampleData),
                        selected = colnames(exampleData)[2]
      )
      
      return(exampleData)
      
      
    } else {
      updateSelectInput(session,
                        "inputend",
                        choices = colnames(upload())
      )
      
      updateSelectInput(session,
                        "repvar",
                        choices = colnames(upload()),
                        selected = colnames(upload())[1]
      )
      
      updateSelectInput(session,
                        "repvar_scatter",
                        choices = colnames(upload())
      )
      
      updateSelectInput(session,
                        "colvar_scatter",
                        choices = colnames(upload())
      )
      
      return(upload())
    }
  })
  
  
  # hide/show Tabs (aggregation) ----
  # Show aggregated datatable and distribution tab only if replication is chosen above
  # TODO: Find out: what is "Summarized Data"?
  observe({
    if(input$checkboxRepvar){
      showTab("tabs", target = "Summarized Data")
      showTab("tabs", target = "Distribution")
    } else {
      hideTab("tabs", target = "Summarized Data")
      hideTab("tabs", target = "Distribution")
    }
  })
  
  
  # daata_full_norep ----
  # if there is replication variable 'data_full_norep' has one column less than 'data_full', otherwise they are identical
  data_full_norep <- reactive({
    validate(
      need(input$repvar != input$inputend, "replication variable can't be input variable (Please alter last input variable or replication variable)")
    )
    
    ## data adapting ----
    if(input$checkboxRepvar){
      data_full() %>% select(-input$repvar)
    } else {
      data_full()
    }
  })

  
  # Pre-filter data Tab ----------------------------------------------
  
  # display dataset as DT
  # Table in tab 'Pre-filter data'
  output$dataDT <- DT::renderDataTable({
    req(data_full())
    data_full()},
    filter = "top",
    options = list(lengthChange = FALSE, 
                   autoWidth = TRUE,
                   scrollX = TRUE
    )
  )
  
  
  # reacVals ----
  # add first_row_filters_string
  reacVals <- reactiveValues(first_row_filters_string = "NULL")
  
  # observe({
  #   reacVals$ind_inputend <- which(colnames(data_full_norep()) == input$inputend)
  #   # reacVals$ind_outputstart <- which(colnames(data_full_norep()== input$inputend) +1)
  # })
  
  
  # ind_inputendR ----
  # index of last input variable
  ind_inputendR  <- reactive({
    which(
      colnames(
        data_full_norep()
        ) == input$inputend
    )
  })
  
  
  # ind_outputstartR ----
  # index of first output variable
  ind_outputstartR <- reactive({
    validate(
      need(
        ncol(
          data_full_norep()
        ) != ind_inputendR(),
        "last input variable cannot be last variable in data frame")
    )
    ind_inputendR() + 1
  })
  
  
  # data_prefiltered ----
  # Table with all chosen filters in 'Pre-filter data' (for no-repvar scenario)
  data_prefiltered <- reactive({
    req(ind_outputstartR())
    req(data_full())
    validate(
      need(input$repvar != input$inputend, "replication variable can't be input variable (Please alter last input variable or replication variable)")
    )
    req(input$dataDT_rows_all)
    d <- data_full()
    #ind_outputstart <<- isolate(which(colnames(d) == input$inputend)+1) # which column is the last input column?
    
    # Convert output parameters to numeric values
    # TODO: findout: can this be more optimized?
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
  
  
  # Define sem function to calculate sem used in deviation method when using replication variable
  sem <- function(x) {sd(x)/sqrt(length(x))}
  
  # data_full_mean ----
  # aggregate data (if not aggregated yet)
  data_full_mean <- reactive({
    validate(
      need(input$repvar != input$inputend, 
           "Replication variable can't be input variable (Please alter last input variable or replication run variable)")
    )
    
    # summarize DT with replication runs by averaging outputs for every setting
    req(input$dataDT_rows_all)
    d <- data_full()[input$dataDT_rows_all,]
    
    
    if(input$checkboxRepvar){

      inputs <<- colnames(data_full_norep())[1:ind_inputendR()]
      
      outputs <<- colnames(data_full_norep())[ind_outputstartR():ncol(data_full_norep())]
      
      d <- group_by_at(d, vars(inputs)) %>%
        summarise(
          across(everything(), 
                 list(
                   estimate = get(input$repvarMethod), 
                   deviation = get(input$deviationMethod)
        )))
      
      
      #colnames(d) <- c(inputs, "rep_mean",  outputs)
      
      d <- d %>% select(-paste0(input$repvar, "_estimate"))
      d <- d %>% select(-paste0(input$repvar, "_deviation"))
      
      d <- d %>% mutate(
        across(.cols = contains("_deviation"), ~ .x * input$sem_mult)
      )
    }
    
    d#[input$dataDT_rows_all,]
    
    as.data.frame(d)
  })
  
  
  # render Data Table ----
  output$repDataDT <- 
    DT::renderDataTable({
    
    d <- data_full_mean()
    
    # if(input$checkboxRepvar)
    #   colnames(d) <- c(inputs, paste(input$repvarMethod, "of", outputs))
    d
    },
  
    filter = "top",
    options = list(lengthChange = FALSE, 
                 autoWidth = TRUE,
                 scrollX = TRUE
  ))
  
  
  # Aggregated Datatable in 'Data' Tab ----
  output$dataDT_summarized <- 
    renderUI({
      if(input$checkboxRepvar){
        tagList(
          h3("Aggregated Data"),
          DT::dataTableOutput("repDataDT")
        )
      }
    })
  
  # data_filteredR ----
  data_filteredR <- reactive({
    req(data_prefiltered())
    if(input$checkboxRepvar){
      #req(data_full_mean)
      data_full_mean()
    } else {
      #req(data_prefiltered)
      data_prefiltered()
    }
  })
  
  
  # names_inputsR ----
  # inputvariables
  names_inputsR <- reactive({
    #req(data_filteredR, ind_inputendR)
    nm <- names(data_filteredR())[1:ind_inputendR()]
    nm
  })
  
  
  # names_outputsR ----
  # outputvariables
  names_outputsR <- reactive({
    colnames(data_filteredR())[ind_outputstartR():ncol(data_filteredR())]
  })
  
  
  # names_outputsR_distribution ----
  names_outputsR_distribution <-  reactive({
    
    #req(data_filteredR, ind_inputendR)
    nm <- names(data_prefiltered())
    nm <- nm[!(nm %in% names_inputsR())]
    nm
  })
  
  
  # data_choose_defaultR ----
  # initialize default value object with all input variables
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
  
  
  # Default values Tab ----
  ## chooseDT ----
  # Choose default values Tab 
  output$chooseDT <- DT::renderDataTable({
    
    #input$goDT
    validate(
      need(data_full(), "no file")
    )
    
    # validate(
    #   need(input$inputend != input$repvar, "Replication run variable can't be an input variable")
    # )
    
    
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
    
    
    ### choice-updates for inputs ----
    updateSelectizeInput(session,
                         "OC",
                         choices = names_outputsR(),
                         selected = names_outputsR()[1]
    )
    
    updateSelectizeInput(session,
                         "OC_scatter",
                         choices = names_outputsR(),
                         selected = names_outputsR()[c(1,2)])
    
    
    
    updateSelectInput(session,
                      "boxplotOutputVar",
                      # choices = names_outputsR()
                      choices = names_outputsR_distribution()
    )
    
    ### display only columns with more than 1 unique entry ----
    uniques <- lapply(data_choose_defaultR(), unique)
    bUniques <- sapply(uniques, function(x) {length(x) == 1})
    data_filtered <<- data_choose_defaultR()[,which(!bUniques), drop = FALSE]
    
    # validate(
    #   need(ncol(data_filtered) > 1, "Data has to have more than 1 input variables with non-unique characteristics.")
    # )
    
    
    for(i in colnames(data_filtered)){
      # transforms variables to factors to be able to choose 1 factor level as default value
      data_filtered[,i] <<- factor(as.factor(data_filtered[,i]))  #factor(...) drops unused factor levels from prefiltering
    }
    
    # if(input$buttonDefault > input$buttonResetDefault) {
    
    # observeEvent(input$buttonDefault,{
    # 
    #   # Let first row be standard default value combination
    #   data_filtered_helper <- data.frame(lapply(data_filtered, as.character), stringsAsFactor = FALSE)
    # 
    #   first_row_filters <- paste0("'[\"", data_filtered_helper[1,], "\"]'")
    #   first_row_filters_string <<- paste0(
    #     "list(NULL, ",
    #     paste0("list(search = ", first_row_filters, ")", collapse = ", "),
    #     ")"
    #   )
    # }
    # )
    # 
    # observeEvent(input$buttonResetDefault ,{
    #   # } else {
    #   first_row_filters_string <<- "NULL"
    # }
    # )
    
    data_filtered
    
  },
  
  filter = "top",
  
  options = list(lengthChange = FALSE, 
                 autoWidth = TRUE, 
                 scrollX = TRUE, 
                 scrollY = TRUE,
                 pageLength = 5,
                 searchCols = eval(parse(text = reacVals$first_row_filters_string))
  )
  #, columns = list(search = "applied")
  
  )
  
  
  
  ## Buttons ----
  # Update Default value filters in Tab based on pre-defined buttons
  
  ### Filter for first row ----
  observeEvent(input$buttonDefault,{
    
    # Let first row be standard default value combination
    data_filtered_helper <- data.frame(lapply(data_filtered, as.character), stringsAsFactor = FALSE)
    
    first_row_filters <- paste0("'[\"", data_filtered_helper[1,], "\"]'")
    reacVals$first_row_filters_string <- paste0( 
      "list(NULL, ",
      paste0("list(search = ", first_row_filters, ")", collapse = ", "),
      ")"
    )
  })
  
  ### Reset filters----
  observeEvent(input$buttonResetDefault ,{
    reacVals$first_row_filters_string <- "NULL"
  })
  
  
  ## Vector column filter choices ----
  search_vector <- reactive({
    req(input$chooseDT_search_columns)
    
    vNamedSearch <- input$chooseDT_search_columns
    names(vNamedSearch) <- colnames(data_filtered)
    vNamedSearch
  })
  
  ## searchbar ----
  # named vector with names of input variables
  # filled successively after default values are chosen from DT
  output$search <- renderPrint({
    
    search_vector()
  })
  
  
  # Default values Tab----
  
  ## defaults_input ----
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
  
  ### output ----
  # print subsetted data frame with filled default values
  output$defaultsInput <- renderPrint({
    
    as.data.frame(t(defaults_input()))
    #dim(as.data.frame(defaults_input()))
  })
  
  ## defaults_df ----
  # table output in dropdown within plot tab, displaying chosen default values
  output$defaults_df <- renderTable({
    
    Values <- data.frame("Variable" = names(defaults_input()),
                         "Default value" = defaults_input(),
                         check.names = FALSE) # makes whitespace in header names possible
    Values
  })
  
  
  ## defaults_df_ui ----
  output$defaults_df_ui <- renderUI({
    
    tableOutput("defaults_df")
  })
  
  
  outputOptions(output, "defaults_df_ui", suspendWhenHidden=FALSE)
  
  
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
  
  
  
  
  # dynamic number of color selectors (one for every OC) ---------------------
  
  ## nOCR ----
  # number of Operating Characteristics
  nOCR <- reactive({
    req(data_filteredR())
    ncol(data_filteredR()) - ind_inputendR()
  })
  
  # nOCR <- reactive({
  #   length(input$OC)
  # })
  
  
  ## colors_ui ----
  # Create colorInput field for every OC
  output$colors_ui <- renderUI({
    
    # req(nOCR())
    # nWidgets <- as.integer(reacVals$nOC)
    # 
    # nWidgetsR <- reactive({
    #   nOCR()
    # })
    
    lapply(1:nOCR(), function(i) {
      colourInput(#inputId = paste0("col", i), 
        inputId = paste0("col_", names_outputsR()[i]),
        # label = reacVals$names_outputs[i],
        label = names_outputsR()[i],
        # label = input$OC[i],
        showColour = "both",
        # value = "black"
        # value = colors()[sample(1:length(colors()),
        #                         size = 1,
        #                         replace = FALSE)]
        
        value = scales::hue_pal()(nOCR())[i]
      )
    })
  })
  
  ## valColvarR ----
  valColvarR <- reactive({
    req(data_filteredR())
    unique(data_filteredR()[[input$color]])
  })

  ## nValColvarR ----
  nValColvarR <- reactive({
    req(valColvarR())
    length(valColvarR())
  })
  
  ## colordim_ui ----
  output$colordim_ui <- renderUI({
    
    lapply(1:nValColvarR(), function(i) {
      colourInput(
        inputId = paste0("col_", valColvarR()[i]),
        label = valColvarR()[i],
        showColour = "both",
        
        value = scales::hue_pal()(nValColvarR())[i]
      )
    })
  })

  
  ## nValColvarR ----
  # TODO: check: is this function repetition neccessery?
  nValColvarR <- reactive({
    req(valColvarR())
    length(valColvarR())
  })
  
  
  ## colordim_ui ----
  # TODO: check: is this function repetition neccessery?
  output$colordim_ui <- renderUI({
    
    lapply(1:nValColvarR(), function(i) {
      colourInput(
        inputId = paste0("col_", valColvarR()[i]),
        label = valColvarR()[i],
        showColour = "both",
        
        value = scales::hue_pal()(nValColvarR())[i]
      )
    })
  })
  
  ## valColvar_scatterR ----
  valColvar_scatterR <- reactive({
    req(data_filteredR())
    unique(data_filteredR()[[input$colvar_scatter]])
  })
  
  
  ## nValColvar_scatterR ----
  nValColvar_scatterR <- reactive({
    req(valColvar_scatterR())
    length(valColvar_scatterR())
  })
  
  
  ## colors_scatter_ui ----
  output$colors_scatter_ui <- renderUI({
    
    lapply(1:nValColvar_scatterR(), function(i) {
      colourInput(
        inputId = paste0("col_", valColvar_scatterR()[i], "_sc"),
        label = valColvar_scatterR()[i],
        showColour = "both",
        
        value = scales::hue_pal()(nValColvar_scatterR())[i]
      )
    })
  })
  
  outputOptions(output, "colors_ui", suspendWhenHidden =FALSE)
  outputOptions(output, "colordim_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "colors_scatter_ui", suspendWhenHidden = FALSE)
  
  
  ## valColvar_scatterR ----
  # TODO: check: is this function repetition neccessery?
  valColvar_scatterR <- reactive({
    req(data_filteredR())
    unique(data_filteredR()[[input$colvar_scatter]])
  })
  
  
  ## nValColvar_scatterR ----
  # TODO: check: is this function repetition neccessery? 
  nValColvar_scatterR <- reactive({
    req(valColvar_scatterR())
    length(valColvar_scatterR())
  })
  
  
  ## colors_scatter_ui ----
  # TODO: check: is this function repetition neccessery?
  output$colors_scatter_ui <- renderUI({
    
    lapply(1:nValColvar_scatterR(), function(i) {
      colourInput(
        inputId = paste0("col_", valColvar_scatterR()[i], "_sc"),
        label = valColvar_scatterR()[i],
        showColour = "both",
        
        value = scales::hue_pal()(nValColvar_scatterR())[i]
      )
    })
  })
  
  outputOptions(output, "colors_ui", suspendWhenHidden =FALSE)
  outputOptions(output, "colordim_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "colors_scatter_ui", suspendWhenHidden = FALSE)
  
  
  ## lUiColors ----
  lUiColors <- reactive({
    
    #nWidgets <- as.integer(reacVals$nOC)
    #names_outputs <- colnames(data_prefiltered()[,reacVals$ind_outputstart:ncol(data_prefiltered())])
    
    # df_colors <- data.frame(lapply(1:nOCR(), function(i) {
    #   input[[paste0("col", i)]]
    # }))
    
    df_colors <- data.frame(lapply(input$OC, function(i) {
      input[[paste0("col_", i)]]
    }))
    
    vColors <- as.vector(t(df_colors))
    
    # names(vColors) <- names_outputsR()
    names(vColors) <- input$OC
    
    # df_colors
    vColors
  })
  
  ## lUiColordim ----
  lUiColordim <- reactive({
    
    df_colors <- data.frame(lapply(valColvarR(), function(i) {
      input[[paste0("col_", i)]]
    }))
    
    vColors <- as.vector(t(df_colors))
    
    names(vColors) <- valColvarR()
    
    vColors
  })
  
  ## lUiColors_scatter ----
  lUiColors_scatter <- reactive({
    
    df_colors <- data.frame(lapply(valColvar_scatterR(), function(i) {
      input[[paste0("col_", i, "_sc")]]
    }))
    
    vColors <- as.vector(t(df_colors))
    
    names(vColors) <- valColvar_scatterR()
    
    vColors
  })

  
  # Plot Tab ----
  
  ## Errorbar Selection ----
  output$errorbar_var <- renderUI({

    # lapply(1:length(input$OC), function(i) {
    #   
    #   selectInput(
    #     inputId = paste0("errorvar_", input$OC[i]),
    #     label = input$OC[i],
    #     choices = names_outputsR(),
    #     selected = names_outputsR()[which(names_outputsR() == input$OC[i]) + 1]
    #   )
    # }) 
    
    if(input$radioErrorsymmetry == "symmetrical"){
      
      selectizeInput(
        inputId = "errorvars",
        label = "Choose all the error variables (sd)",
        choices = names_outputsR(),
        multiple = TRUE
      )
      
    } else{
      tagList(
        selectizeInput(
          inputId = "errorvars_upper",
          label = "Choose error variables for the upper KI",
          choices = names_outputsR(),
          multiple = TRUE
        ),
        
        selectizeInput(
          inputId = "errorvars_lower",
          label = "Choose error variables for the lower KI",
          choices = names_outputsR(),
          multiple = TRUE
        )
      )
    }
  })
  
  
  # observes ----
  
  ## boxplotGroupVar ----
  # only make variables with default values available for simulation parameter choice
  observe({
    updateSelectInput(session,
                      "boxplotGroupVar",
                      choices = names_inputsR()
    )
  })
  
  ## facet_distribution_rows ----
  observe({
    updateSelectInput(session,
                      "facet_distribution_rows",
                      choices = names_inputsR()
    )
  })
  
  ## facet_distribution_cols ----
  observe({
    updateSelectInput(session,
                      "facet_distribution_cols",
                      choices = names_inputsR()
    )
  })
  
  ## facet_distribution_wrap ----
  observe({
    updateSelectizeInput(session,
                         "facet_distribution_wrap",
                         choices = names_inputsR()
    )
  })
  
  ## x ----
  observe({
    updateSelectInput(session,
                      "x",
                      choices = names(defaults_input())
    )
  })
  
  ## facet_rows ----
  observe({
    updateSelectInput(session,
                      "facet_rows",
                      choices = names(defaults_input())
    )
  })
  
  ## facet_cols ----
  observe({
    updateSelectInput(session,
                      "facet_cols",
                      choices = names(defaults_input())
    )
  })
  
  ## facet_wrap ----
  observe({
    updateSelectizeInput(session,
                         "facet_wrap",
                         choices = names(defaults_input())
    )
  })
  
  ## shape ----
  observe({
    updateSelectInput(session,
                      "shape",
                      choices = names(defaults_input())
    )
  })
  
  ## linetype ----
  observe({
    updateSelectInput(session,
                      "linetype",
                      choices = names(defaults_input())
    )
  })
  
  ## color ----
  observe({
    updateSelectInput(session,
                      "color",
                      choices = names(defaults_input())
    )
  })
  
  ## checkboxColor ----
  observe({
    if(length(input$OC) != 1){
      updateCheckboxInput(session,
                          "checkboxColor",
                          value = FALSE
      )
    }
  })
  
  ## checkboxRepvar ----
  observe({
    if(input$checkboxExampleData == FALSE){
      updateCheckboxInput(session,
                          "checkboxRepvar",
                          value = FALSE)
    }
  })
  
  ## checkboxPalette_OC ----
  observe({
    if(input$checkboxColor == TRUE){
      updateCheckboxInput(session,
                          "checkboxPalette_OC",
                          value = FALSE)
    }
  })
  
  ## facet_rows_scatter ----
  observe({
    updateSelectInput(session,
                      "facet_rows_scatter",
                      choices = names(defaults_input()))
  })
  
  ## facet_cols_scatter ----
  observe({
    updateSelectInput(session,
                      "facet_cols_scatter",
                      choices = names(defaults_input()))
  })
  
  ## facet_wrap_scatter ----
  observe({
    updateSelectInput(session,
                      "facet_wrap_scatter",
                      choices = names(defaults_input()))
  })
  
  ## shape_scatter ----
  observe({
    updateSelectInput(session,
                      "shape_scatter",
                      choices = names(defaults_input()))
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
  
  
  
  # lDefault ----
  # variant with automatic input of chosen filters as default values
  lDefault <- eventReactive(input$updateDefaultList, 
                            {as.list(defaults_input())}
                            )
  ## output ----
  # Output of list with default values
  output$lDefault <- renderPrint({
    req(lDefault)
    print(lDefault())
  })
  
  
  # Boxplot -------------------------------------------------------------
  
  output$pBoxplot <- renderPlot({
    validate(
      need(input$repvar != input$inputend, "replication variable can't be an input variable ('Please alter last input variable or replication variable")
    )
    # validate(
    #   need(length(defaults_input()) != 0, "Please choose default values first")
    # )
    # req(names_outputsR, data_full(), names_inputsR)
    d <- data_prefiltered()
   
    for(i in names_inputsR()){
      d[,i] <- as.factor(d[,i])
    }
    
    boxplot <- ggplot(d, aes_string(fill = input$boxplotGroupVar,
                                    col = input$boxplotGroupVar,
                                    x = input$boxplotOutputVar)
    )
    
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
      if("Density" %in% input$densitytype){
        boxplot <-
          boxplot + 
          geom_density(aes(y = ..density..), 
                       alpha = input$alpha)
      }
      
      if("Histogram" %in% input$densitytype){
        boxplot <-
          boxplot + 
          geom_histogram(aes(y = ..density..),
                         bins = input$bins,
                         alpha = input$alpha,
                         position = input$hist_position
          )
      }
      
      boxplot
      
    } else {
      
      if(input$boxplottype == "Violinplot"){
        boxplot + geom_violin(aes_string(x = input$boxplotGroupVar,
                                         y = input$boxplotOutputVar,
                                         color = input$boxplotGroupVar,
                                         fill = input$boxplotGroupVar),
                              alpha = input$alpha)
      } else {
        boxplot + geom_boxplot(aes_string(x = input$boxplotGroupVar,
                                          y = input$boxplotOutputVar,
                                          color = input$boxplotGroupVar,
                                          fill = input$boxplotGroupVar),
                               alpha = input$alpha)
      }
    }
  })
  
  
  # PLOT -------------------------------------------------------------------
  
  ## df_scatterplot ----
  df_scatterplot <- reactive({
    
    # 1 line df with default values for variables that are checked

    # default_df <- defaults_input()
    # default_df <- as.data.frame(defaults_input())
    ### derfault_df ----
    default_df <- defaults_input()
    
    ## sim_par ----
    # vector of names of simulation parameters
    sim_par <- input$repvar_scatter
    
    sim_par <- c(sim_par, input$colvar_scatter)
    # if(input$checkboxShape_scatter){
    #   sim_par <- c(sim_par, input$shape_scatter)
    # }
    
    if(input$radioFacet_scatter == "grid"){
      sim_par <- c(sim_par, input$facet_rows_scatter, input$facet_cols_scatter)
    }
    
    if(input$radioFacet_scatter == "wrap"){
      sim_par <- c(sim_par, input$facet_wrap_scatter)
    }
    
    ## default_filter ----
    # exclude simulation parameters from df with default values
    default_filter <- default_df[!(names(default_df) %in% sim_par)]
    
    # default_filter <- gsub('\\[', "", default_filter)
    # default_filter <- gsub('\\]', "", default_filter)
    
    default_filter <- gsub('\\[\\"', "", default_filter)
    default_filter <- gsub('\\"\\]', "", default_filter)
    
    ## bedingung ----
    bedingung <- paste0(paste0("`", names(default_filter), "`"),
                        " == ",
                        paste0("'", default_filter, "'"),
                        # default_filter,
                        collapse = " & ")
    
    if(length(default_filter) != 0){
      df_scatterplot <- subset(data_filteredR(), eval(parse(text = bedingung)))
    } else {
      df_scatterplot <- data_filteredR()
    }
    
    df_scatterplot # return data frame

  })
  
  ## df_scatterplot output ----
  output$df_scatterplot <- DT::renderDataTable({
    
    df_scatterplot()
  },
  options = list(scrollX = TRUE)
  )
  
  ## data_longer_scatter ----
  data_longer_scatter <- reactive({
    req(input$OC_scatter)
    
    d <- df_scatterplot()
    d <- d[input$chooseDT_rows_all,]
    
    # d <-
    #   d %>%
    #   pivot_longer(cols = input$OC_scatter,
    #                names_to = "OC",
    #                values_to = "value")
    d
  })
  
  ## plot_object_scatter ----
  plot_object_scatter <- reactive({

    colScale_scatter <- scale_colour_manual(values = lUiColors_scatter())
    
    p1 <- ggplot(
      df_scatterplot(), 
      #aes_string(x = value, color = OC)
      aes_string(x = input$OC_scatter[1], y = input$OC_scatter[2])
    ) + geom_point(aes(colour = factor(get(input$colvar_scatter)))) + labs(colour = input$colvar_scatter)
    
    if(input$checkboxPalette_scatter){
      p1 <- p1 + colScale_scatter
    }
 
    # if(input$checkboxShape_scatter){
    #   p1 <-
    #     p1 +
    #     aes(
    #       
    #       shape =
    #         factor(get(
    #           input$shape_scatter
    #         ))
    #       ,
    #       group =
    #         interaction(
    #           factor(get(
    #             input$shape_scatter
    #           )),
    #           OC
    #         )
    #     )
    #   
    # } else {
    #   p1 <-  p1 + aes(group = OC)
    # }
    
    facets <- input$facet_wrap_scatter %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    frows <- input$facet_rows_scatter %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    fcols <- input$facet_cols_scatter %>%
      str_replace_all(",", "+") %>%
      rlang::parse_exprs()
    
    # if(input$radioFacet == "grid"){
    #   p1 <-
    #     p1 +
    #     facet_grid(vars(get(input$facet_rows)),
    #                vars(get(input$facet_cols))
    #     )
    # }
    
    if(input$radioFacet_scatter == "grid"){
      p1 <-
        p1 +
        facet_grid(vars(!!!frows),
                   vars(!!!fcols),
                   labeller = "label_both"
        )
    }
    
    if(input$radioFacet_scatter == "wrap"){
      
      p1 <-
        p1 +
        facet_wrap(vars(!!!facets)
                   # facet_wrap(vars(get(!!!(input$facet_wrap)))
                   , labeller = "label_both"
        )
    }
    
    p1
  })
  
  ## plot_scatter output ----
  output$plot_scatter <- renderPlot({
    
    plot_object_scatter()
  })
  ## scatter_ui output ----
  output$scatter_ui <- renderUI({
    plotOutput("plot_scatter")
  })
  
  
  # Plot Df ------------------------------------------
  
  # Data frame used for plot
  # Filters every variable for the specified default value except the chosen simulation parameters, which can have more distinguishable values
  
  ## df_plot ----
  df_plot <- reactive({
    
    # 1 line df with default values for variables that are checked

    # default_df <- defaults_input()
    # default_df <- as.data.frame(defaults_input())
    default_df <- defaults_input()

    # vector of names of simulation parameters
    sim_par <- input$x

    # Code$sim_par <- paste(input$x, input$facet_rows, input$facet_cols, input$linetype, sep = ", ")
    Code$x <- input$x

    # if(input$checkboxShape){
    #   sim_par <- c(sim_par, input$shape)
    # }
    
    if(input$checkboxLinetype){
      sim_par <- c(sim_par, input$linetype)

      # Code$sim_par <- paste0(Code$sim_par,", ", input$linetype)

    }
    
    if(input$radioFacet == "grid"){
      sim_par <- c(sim_par, input$facet_rows, input$facet_cols)
      # Code$sim_par <- paste0(Code$sim_par,", ", input$facet_rows,", ", input$facet_cols)
    }
    
    if(input$radioFacet == "wrap"){
      sim_par <- c(sim_par, input$facet_wrap)
      # Code$sim_par <- paste0(Code$sim_par,", ", input$facet_wrap)
    }
    
    if(input$checkboxColor){
      sim_par <- c(sim_par, input$color)
      # Code$sim_par <- c(Code$sim_par,", ", input$color)
    }
    
    if(input$checkboxColor){
      sim_par <- c(sim_par, input$color)
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
  
  
  # df_plot output ----
  output$df_plot <- DT::renderDataTable({
    
    df_plot()
  },
  options = list(scrollX = TRUE)
  )
  
  
  # data_longer ----
  # Transform dataset to long format on chosen output variables for easy plotting
  data_longer <- reactive({
    #req(input$OC)
    
    d <- df_plot()
    # d <- d[input$chooseDT_rows_all,]
    
    d <- 
      d %>%
      pivot_longer(
        cols = input$OC,
        names_to = "OC",
        values_to = "value"
      )
    
    if(input$checkboxErrorbar){
      
      # errorbar_vars <- c()
      # for(i in 1:input$OC){
      #   errorbar_vars <- c(errorbar_vars, input[[paste0("errorvar_", input$OC[i])]])
      # }
      # 
      # errorbar_vars <- "output2"
      
      if(input$radioErrorsymmetry == "symmetrical"){
        
        validate(
          need(!is.null(input$errorvars), "please define Error variables")
        )
        
        d <- d %>%
          pivot_longer(
            # cols = errorbar_vars,
            cols = input$errorvars,
            names_to = "errorvar_name",
            values_to = "error"
          )
        
        # bedingung <- paste0(paste0("`", names(default_filter), "`"),
        #                     " == ",
        #                     paste0("'", default_filter, "'"),
        #                     # default_filter,
        #                     collapse = " & ")
        # 
        # 
        # df_plot <- subset(data_filteredR(), eval(parse(text = bedingung)))
        # 
        bedingung_errorbar <- paste0("(OC == '", input$OC, "' & errorvar_name == '", input$errorvars, "')", collapse = " | ")
        
        d <- subset(d, eval(parse(text = bedingung_errorbar)))
        
      } else {
        
        validate(
          need(!is.null(input$errorvars_upper), "please define variable for upper bound/deviation")
        )
        
        validate(
          need(!is.null(input$errorvars_lower), "please define variable for lower bound/deviation")
        )
        
        d <- d %>%
          pivot_longer(
            cols = input$errorvars_upper,
            names_to = "errorvar_upper_name",
            values_to = "error_upper"
          )
        
        d <- d%>%
          pivot_longer(
            cols = input$errorvars_lower,
            names_to = "errorvar_lower_name",
            values_to = "error_lower"
          )
        
        bedingung_errorbar <- paste0("(OC == '", input$OC, "' & errorvar_upper_name == '", input$errorvars_upper, "' & errorvar_lower_name == '", input$errorvars_lower, "')", collapse = " | ")
        
        d <- subset(d, eval(parse(text = bedingung_errorbar)))
 
      }
    }
    
    d
    
  })
  
  #output$OClength <- renderPrint({c(input$OC, length(input$OC))})
  
  
  # Plots ---------------------------------------
  
  ## lineplot_object ----
  # Plot based on which dimensions are chosen
  lineplot_object <- reactive({
    
    data_lp <- reactive({
      
      data_output <- data_longer()
      
      if(input$checkboxLinetype){
        
        data_output[[input$linetype]] <- as.factor(data_output[[input$linetype]])
        
      }
      data_output
    })
    
    # validate(need(length(lUiColors) != 0, message = "Choose colors first"))
    
    # output$lineplot <-renderPlot({
    
    colScale <- reactive({
      if(input$checkboxColor){
        scale_colour_manual(values = lUiColordim())
      } else {
        scale_colour_manual(values = lUiColors())
      }
      
    })
    
    p1 <- ggplot(
      #req(data_longer()),

      data_lp(), 

      aes_string(x = input$x, y = "value")
    ) 
    
    if(input$checkboxPalette_OC){
      p1 <- p1 + colScale()
    }
    
    if(input$checkboxPalette_dim){
      p1 <- p1 + colScale()
    }
    
    # if(input$checkboxShape){
    #   colScale <- scale_colour_manual(values = lUiColors())
    #   p1 <- p1 + colScale
    #   }

    if(input$checkboxLine){
      if(input$checkboxColor){
        p1 <-
          p1 +
          geom_line(aes(
            y = value,
            color  = factor(get(input$color))),
            size = input$linesize)
        Code$colour <<- paste(input$color)

      } else {
        p1 <- 
          p1 + 
          geom_line(aes(
            y = value,
            color  = OC),
            size = input$linesize)
        Code$colour <<- "OC"

      }
    }
    
    if(input$checkboxPoint){
      if(input$checkboxColor){
        p1 <-
          p1  +
          geom_point(aes(
            y = value,
            color  = factor(get(input$color))),

            size = 3*input$linesize)
      } else {
        p1 <-
          p1  +
          geom_point(aes(
            y = value,
            color  = OC),

            size = 3*input$linesize)
      }
    }
  
    # 
    # if(input$checkboxShape){
    #   p1 <-
    #     p1 +
    #     aes(
    #       # linetype =
    #       #   factor(get(
    #       #     input$shape
    #       #   ))
    #       # 
    #       # ,
    #       shape =
    #         factor(get(
    #           input$shape
    #         ))
    #       # ,
    #       # group =
    #       #   interaction(
    #       #     factor(get(
    #       #       input$shape
    #       #     )),
    #       #   OC
    #       # )
    #     ) + labs(shape = input$shape)
    #   
    #   
    #   #, linetype = paste(input$shape)
    #   # )
    #   
    # } 
   
    if(input$checkboxLinetype){
  
      p1 <-
        p1 +
        aes_string(
          
          linetype =
            # factor(
            # get(
            input$linetype
          # )
          # )
          ,
          shape =
            # factor(
            # get(
            input$linetype
          # )
          # )
          # ,
          # group =
          #   interaction(
          #     factor(get(
          #       input$linetype
          #     )),
          #     OC
          #   )
        ) 
      # ) + guides(linetype = guide_legend(title = "Users By guides"))
      # ) + labs(linetype = input$linetype, shape = input$linetype)

    }
    # 
    # else {
    #  p1 <-  p1 
    #   + aes(group = OC)
    # }
    
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
    
    if(input$checkboxErrorbar){
      
      if(input$radioErrorsymmetry == "symmetrical"){
        p1 <- 
          p1 + geom_errorbar(aes(ymin = value - error, 
                                 ymax = value + error
                                 , color = OC), 
                             position=position_dodge(0.05))
      } else {
        
        if(input$radioErrorstructure == "deviation"){
          
          p1 <- 
            p1 + geom_errorbar(aes(ymin = value - error_lower,
                                   ymax = value + error_upper,
                                   color = OC),
                               position = position_dodge(0.05))
        } else {
          p1 <- 
            p1 + geom_errorbar(aes(ymin = error_lower,
                                   ymax = error_upper,
                                   color = OC),
                               position = position_dodge(0.05))
          
        }
      }
    }
    

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
    
    p1 +
      theme(legend.key.size = unit(4, 'cm'))
    # + 
    # guides(linetype = guide_legend(override.aes = list(size = 4)))
    
  }
  # res = exprToFunction(input$resolution)
  # res = input$resolution
  )
  
  
  # scatterplot_object <- reactive({
  #   
  #   p1 <- ggplot(
  #     df_plot(), 
  #     aes_string(x = input$OC[1], y = input$OC[2])
  #   ) + geom_point()
  #   
  #   p1
  #   
  # })
  # 
  
  ## plot_object ----
  # create plot_object with final settings
  plot_object <- reactive({
    
    # scatterplot or lineplot?
    # if(input$scatterplot)
    #   p1 <- scatterplot_object()
    # else
    
    p1 <- lineplot_object()
    
    
    ### THEME & other general plot options ----
    
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
    
    ## TITLE ----------------------------------------
    
    if (input$checkboxTitle){
      p1 <- p1 + 
        labs(title = input$plot_title)  + 
        theme(plot.title = element_text(colour = input$plot_title_colour,
                                        size = input$plot_title_size, 
                                        vjust = 1.5,
                                        hjust = input$plot_title_place))
    }
    
    ## LABS ----------------------------------------
    
    if(input$checkboxAxis){
      
      p1 <- p1 +
        labs(x = input$xLab,
             y = input$yLab)
    }
    
    p1 <- p1 + theme(legend.title = element_blank())
    
    #updateAceEditor(session, editorId = "print_code", value = plot_code() )

    p1
  })
  
  # plottype observe ----
  observe({
    
    if(input$plottype){
      
      output$lineplotly <- renderPlotly({
        ggplotly(plot_object())
        #ggplotly(lineplot_object())
      })
      
    } else {
      
      output$lineplot <- renderPlot({
        plot_object()
        #lineplot_object()
      })
    }
  })
  
  
  # scatterplot output----
  output$scatterplot <- renderPlot({
    scatterplot_object()
  })
  
  
  # lineplot_ui output ----
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
    # if(input$scatterplot){
    #   plotOutput("scatterplot",
    #              height = input$plotheight,
    #              width = input$plotwidth)
    # } else {
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
    #}
  })
  
  
  # Animation ----
  
  ## observe ----
  ### animateIteratorSelect ----
  
  observe({
    updateSelectInput(session,
                      "animateIteratorSelect",
                      choices = names(defaults_input())
    )
  })
  
  ## vars ----

  ## observeEvents ----
  
  observeEvent(input$animationRenderButton,{
    
    ## animationOutDynamic ---- 
    output$animationOutDynamic <- renderImage({
      
      if(input$animationRenderButton == 0) return()
      
      isolate({
        #validates select input
        validate(need(
          input$animateIteratorSelect,
          "please specify iteration variable first"))
        
        # temporary file, saves render
        outfileDyn <- tempfile(fileext='.gif')
        
        #Plot + animation attributes
        ap <- plot_object() + transition_states(input3,
                                                transition_length = 2,
                                                state_length = 1
        )
        
        # animation
        anim_save("outfileDyn.gif", animate(ap))
        
        
        # Returns list, contains filename
        list(src = "outfileDyn.gif",
             contentType = 'image/gif'
        )
      })
    },
    #Deletes File
    deleteFile = TRUE)
  })
  
  
  
  
  
  # observes ----
  
  observe({
    updateNumericInput(session,
                       "download_plotwidth",
                       value = input$plotwidth
    )
  })
  
  observe({
    updateNumericInput(session,
                       "download_plotheight",
                       value = input$plotheight
    )
  })
  
  observe({
    updateNumericInput(session,
                       "download_resolution",
                       value = input$resolution
    )
  })
  
  
  # File IO ----
  download_type <- reactive({input$download_type})
  
  output$download_plot <- downloadHandler(
    
    filename = function(){paste0(input$download_name,
                                 ".", 
                                 input$download_type)},
    
    content = function(file){
      fun <- match.fun(download_type() )
      
      fun(file, 
          height = input$download_plotheight, 
          width = input$download_plotwidth,
          units = input$download_unit,
          res = input$download_resolution)
      
      # fun(file,
      #     height = input$plotheight,
      #     width = input$plotwidth,
      #     res = input$resolution)
      print(plot_object())
      dev.off()
      
      # -----IFELSE for gr_Devices----------------------
      
      # if(download_type() == "png"){
      #   device <- function(..., width, height) {
      #     grDevices::png(...,
      #                    width = input$download_plotwidth,
      #                    height = input$download_plotheight,
      #                    res = input$download_resolution,
      #                    units = input$download_unit)
      #   }
      #   ggsave(file, plot = plot_object(), device = device)
      #   
      
      
      # ggsave(file,
      #        plot = plot_object(),
      #        device = png(res = input$download_resolution),
      #        width = input$download_plotwidth,
      #        height = input$download_plotheight,
      #        units = input$download_unit
      # )
      
      
      # plot_object()
      # png(file,
      #        width = input$download_plotwidth,
      #        height = input$download_plotheight,
      #        units = input$download_unit,
      #        res = input$resolution
      # )
      # 
      # dev.off()
      
      
      # } else {
      #   if(download_type() == "jpeg"){
      #     device <- function(..., width, height) {
      #       grDevices::jpeg(..., 
      #                       width = input$download_plotwidth, 
      #                       height = input$download_plotheight,
      #                       res = input$download_resolution,
      #                       units = input$download_unit)
      #     }
      #     ggsave(file, plot = plot_object(), device = device)
      #   } else {
      #     if(download_type() == "tiff"){
      #       device <- function(..., width, height) {
      #         grDevices::tiff(..., 
      #                         width = input$download_plotwidth, 
      #                         height = input$download_plotheight,
      #                         res = input$download_resolution, 
      #                         units = input$download_unit)
      #       }
      #       ggsave(file, plot = plot_object(), device = device)
      #     } else {
      #       if(download_type() == "pdf"){
      #         ggsave(file,
      #                plot = plot_object(),
      #                device = pdf(),
      #                width = input$download_plotwidth,
      #                height = input$download_plotheight,
      #                units = input$download_unit
      #         )
      #         
      #       }
      #       

      # ggsave(file, plot = plot_object(), device = device, width = 11, height = 4, dpi = 300, units = "in")
      # ggsave(file, plot = plot_object()
      #        ,width = input$download_plotwidth
      #        ,height = input$download_plotheight
      #        #,units = input$download_unit
      #        ,device = device
      # )
       
      #}
      
      # }
      # }
      
      
      # device <- function(..., width, height) {
      #   fun(..., width = width, height = height,
      #                  res = input$resolution, units = "icm")
      # }
      # ggsave(file, plot = lineplot_object(), device = device)
      #  
      
    }
  )
  
  
  # Code ----
  Code <- reactiveValues()
  # 
  # plot_code <- reactive({
  #   
  #   return(
  #     
  #     paste0("library(ggplot)", "\n\n", 
  #            "ggplot(data = data, aes(x = ", input$x, ", y = ", input$OC, for(i in 2:length(input$OC)){paste0(", ", input$OC[1][i])},"))","\n\n",
  #            "length input$OC:", length(input$OC),
  #            "sim_par: ", "paste0(Code$sim_par)",
  #            "colour:", Code$colour, "\n\n"#,
  #            
  #            # "data <- data %>%  pivot_longer(cols = ", input$OC, ",names_to = 'OC', values_to = 'value')"
  #     )
  #   )
  #   
  # })
}

# shinyApp ----
shinyApp(ui = ui, server = server
         , options = list(launch.browser = TRUE)
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


