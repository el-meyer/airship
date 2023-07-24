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
library(transformr)

library(bslib)
library(shinydashboard)
library(scales)
library(Cairo)
library(shinyAce)
options(shiny.usecairo = TRUE)

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
                             menuItem(
                                 "Scatterplot", 
                                 tabName = "scatterplot", 
                                 icon = icon("braille")
                             ),
                             menuItem("Help",
                                      tabName = "help",
                                      icon = icon("question")
                             ),
                             hr()
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
                    uiOutput("dataDT_summarized")
                ),
                
                ### DEFAULT VALUES ----
                tabItem(
                    tabName = "default",
                    br(),
                    actionButton("buttonDefault", "Take first row as default values"),
                    actionButton("buttonDefaultHighlighted", "Take highlighted row as default values"),
                    
                    actionButton("buttonResetDefault", "Reset selections"),
                    
                    DT::dataTableOutput("chooseDT")
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
                            uiOutput("lineplot_ui"),
                            imageOutput("animationOutDynamic",
                                        inline = TRUE)
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
                                        icon = icon("paintbrush"),
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
                                        icon = icon("paintbrush"),
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
                            )
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
                            
                            #### Style options ----
                            actionButton("change_style", label = "Style options"),
                            
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
                        ),
                        
                        #### Animation Input ----
                        column(3,
                               ##### animateIteratorSelect ----
                               selectInput(
                                   "animateIteratorSelect", 
                                   "Choose variable to animate over:", 
                                   choices = NULL
                               ),
                               actionButton(
                                   "animationRenderButton",
                                   "Render animation"
                               ),
                               
                               ##### render options ----
                               actionButton("changeRender", label = "Render options"),
                               
                               bsModal("modal_render",
                                       "Change render options",
                                       trigger = "changeRender",
                                       size = "large",
                                       
                                       sliderInput(
                                           "frameAmount",
                                           "number of frames to render",
                                           value = 100,
                                           min = 10,
                                           max = 1000
                                       ),
                                       
                                       sliderInput(
                                           "renderFPS",
                                           "frames per second",
                                           value = 10,
                                           min = 1,
                                           max = 120
                                       ),
                                       
                                       numericInput(
                                           "durationAnimation",
                                           "length of animation in seconds",
                                           value = 10,
                                           min = 1,
                                           max = 1000,
                                           step = 1
                                       )
                               ),
                               actionButton(
                                   "animationCloseButton",
                                   "Close animation"
                               )
                        )
                    ),
                    hr(),
                    
                    #### Plotted Data ----
                    h2("Plotted Data"),
                    br(),
                    fluidRow(
                        DT::dataTableOutput("df_plot")
                    ),
                    
                    hr(),
                    br(),
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
                                       icon = icon("paintbrush"),
                                       uiOutput("colors_scatter_ui"),
                                       inputId = "dropdown_colors_scatter"
                                   ),
                                   draggable = TRUE
                               )
                        )
                ),
                
                
                ### HELP ----
                tabItem("help",
                        
                        h4("Info"),
                        HTML("This app is designed to plot simulation results of clinical trials. It has been developed by Constantin Kumaus, Elias Meyer (both Medical University Vienna) and Michal Majka."),
                        
                        h2("User Manual"),
                        HTML("Following you will find details on every part of the app and how they are to be used."),
                        
                        h4("Data Settings"),
                        HTML("There are a few requirements to the data in order for the app to work. So far only .csv files can be uploaded. It is expected that the data is arranged in a way such that the input variables/design parameters precede the output variables/operating characteristics. Each row represents one simulation run with a different combination of input/design parameters. "),
                        HTML("If your data is not aggregated yet i.e. if you have every single simulation outcome as one row in your dataset, and a 'replication run index variable' you can click the checkbox and choose which of your variables is the 'replication run index'. The dataset is then averaging over the OCs either by mean or median. Additionally the 'Distribution' tab opens where you can investigate the behaviour of your variables and outcomes."),
                        
                        
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
            need(input$file, "No file")
        ) 
        
        # file = user uploaded file in tab Data Settings
        inFile <-input$file 
        
        try(read.csv(inFile$datapath,
                     header = TRUE,
                     sep = input$sep,
                     stringsAsFactors = TRUE))
    })
    
    
    # data_full ----
    # Use example data if checkbox is checked, otherwise use uploaded dataset
    # Update Input choices
    data_full <- reactive({
        
        reacVals$first_row_filters_string <- "NULL"
        updateTabItems(session, "sidebarMenu", "default")
        Sys.sleep(0.05)
        updateTabItems(session, "sidebarMenu", "data")
        
        
        if(input$checkboxExampleData){
            
            col_names_example_dat <- colnames(exampleData)
            updateSelectInput(session, 
                              "inputend", 
                              choices = col_names_example_dat,
                              selected = "input4"
            )
            
            updateSelectInput(session,
                              "repvar",
                              choices = col_names_example_dat,
                              selected = "replications"
            )
            
            updateSelectInput(session,
                              "repvar_scatter",
                              choices = col_names_example_dat
            )
            
            updateSelectInput(session,
                              "colvar_scatter",
                              choices = col_names_example_dat,
                              selected = col_names_example_dat[2]
            )
            
            return(exampleData)
            
            
        } else {
            
            col_names_upload <- colnames(upload())
            
            updateSelectInput(session,
                              "inputend",
                              choices = col_names_upload
            )
            
            updateSelectInput(session,
                              "repvar",
                              choices = col_names_upload,
                              selected = col_names_upload[1]
            )
            
            updateSelectInput(session,
                              "repvar_scatter",
                              choices = col_names_upload
            )
            
            updateSelectInput(session,
                              "colvar_scatter",
                              choices = col_names_upload,
                              selected = col_names_upload[2]
            )
            
            return(upload())
        }
    })
    
    
    # hide/show Tabs (aggregation) ----
    # Show aggregated datatable and distribution tab only if replication is chosen above
    
    observe({
        if(input$checkboxRepvar){
            showTab(inputId = "tabs", target = "distribution", select = FALSE, session = session)
        } else {
            hideTab(inputId = "tabs", target = "distribution",  session = session)
        }
    })
    
    
    # daata_full_norep ----
    # if there is replication variable 'data_full_norep' has one column less than 'data_full', otherwise they are identical
    data_full_norep <- reactive({
        
        validate(
            need(input$repvar != input$inputend, "Replication variable can't be input variable (Please alter last input variable or replication variable)")
        )
        
        tryCatch({
            ## data adapting ----
            if(input$checkboxRepvar){
                data_full() %>% select(-input$repvar)
            } else {
                data_full()
            }  
        }, error = function(e) {
            err_ <- ""
            validate(
                need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
            ) 
        })
        
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
                "Last input variable cannot be last variable in data frame")
        )
        ind_inputendR() + 1
    })
    
    
    # data_prefiltered ----
    # Table with all chosen filters in 'Pre-filter data' (for no-repvar scenario)
    data_prefiltered <- reactive({
        req(ind_outputstartR())
        req(data_full())
        validate(
            need(input$repvar != input$inputend, "Replication variable can't be input variable (Please alter last input variable or replication variable)")
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
        
        # extracts rows that fit the filter choices
        d[input$dataDT_rows_all,] 
        # }
        
    })
    
    
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
        # req(data_full())
        d <- data_full()[input$dataDT_rows_all,]
        
        if(input$checkboxRepvar){
            
            req(data_full_norep())
            req(ind_inputendR())
            req(ind_outputstartR())      
            
            inputs <<- colnames(data_full_norep())[1:ind_inputendR()]
            outputs <<- colnames(data_full_norep())[ind_outputstartR():ncol(data_full_norep())]
            
            output_class <- sapply(d[outputs], class)
            if (any(output_class != "numeric" & output_class != "integer")) {
              
              ind_not_num <- which(output_class != "numeric" & output_class != "integer")
                vars_not_num <- outputs[ind_not_num]
                n_vars <- length(vars_not_num)
                xx <- paste0("Currently `", paste0(vars_not_num, collapse = "`, "), "`", 
                             ifelse(n_vars == 1, " variable is", " variables are"),
                             " defined as output ",
                             ifelse(n_vars == 1, "variable", "variables"),
                             ifelse(n_vars == 1, 
                                    " but is not of class numeric",
                                    " but are not of class numeric")
                ) 
                text_err <- paste0("All defined output variables must be of class numeric. ", xx)
                
                validate(
                    need(all(output_class == "numeric"), text_err)
                )
            }
            
            d <- tryCatch({
                suppressMessages(group_by_at(d, vars(inputs)) %>%
                                     summarise(across(
                                         everything(),
                                         list(
                                             estimate = get(input$repvarMethod),
                                             deviation = get(input$deviationMethod)
                                         )
                                     )))
            }, warning = function(w) {
                err_ <- ""
                validate(
                    need(err_ != "", "Replication variable must be an integer variable")
                )}
            )
            
            d <- d %>% select(-paste0(input$repvar, "_estimate"))
            d <- d %>% select(-paste0(input$repvar, "_deviation"))
            
            d <- d %>% mutate(
                across(.cols = contains("_deviation"), ~ .x * input$sem_mult)
            )
        }
        as.data.frame(d)
    })
    
    
    # render Data Table ----
    output$repDataDT <- 
        DT::renderDataTable({
            d <- data_full_mean()
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
    
    # Default values Tab ----
    ## chooseDT ----
    # Choose default values Tab 
    output$chooseDT <- DT::renderDataTable({
        
        validate(
            need(data_full(), "No file")
        )
        
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
        
        input$buttonDefault
        input$buttonResetDefault # reset selection 
        
        for(i in colnames(data_filtered)){
            # transforms variables to factors to be able to choose 1 factor level as default value
            data_filtered[,i] <<- factor(as.factor(data_filtered[,i]))  #factor(...) drops unused factor levels from prefiltering
        }
        
        data_filtered
        
    },
    
    filter = "top",
    selection = "single",
    
    options = list(lengthChange = FALSE, 
                   autoWidth = TRUE, 
                   scrollX = TRUE, 
                   scrollY = TRUE,
                   pageLength = 5,
                   searchCols = eval(parse(text = reacVals$first_row_filters_string))
    )
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
    
    observeEvent(input$buttonDefaultHighlighted, {

        req(input$chooseDT_rows_selected)
        ith_row <- input$chooseDT_rows_selected
        
        data_filtered_helper <- data.frame(lapply(data_filtered, as.character), stringsAsFactor = FALSE)
        
        first_row_filters <- paste0("'[\"", data_filtered_helper[ith_row,], "\"]'")
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
        defaults_input <- search_vector()[search_vector() != ""]
        defaults_input
    })
    
    
    ### output ----
    # print subsetted data frame with filled default values
    output$defaultsInput <- renderPrint({
        as.data.frame(t(defaults_input()))
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
    
    # dynamic number of color selectors (one for every OC) ---------------------
    
    ## nOCR ----
    # number of Operating Characteristics
    nOCR <- reactive({
        req(data_filteredR())
        ncol(data_filteredR()) - ind_inputendR()
    })
    
    ## colors_ui ----
    # Create colorInput field for every OC
    output$colors_ui <- renderUI({
        
        lapply(1:nOCR(), function(i) {
            colourInput(
                inputId = paste0("col_", names_outputsR()[i]),
                label = names_outputsR()[i],
                showColour = "both",
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
        tryCatch({
            unique(data_filteredR()[[input$colvar_scatter]])  
        }, error = function(e) {
            err_ <- ""
            validate(
                need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
            )}
        )
        
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
        tryCatch({
            unique(data_filteredR()[[input$colvar_scatter]])
        }, error = function(e) {
            err_ <- ""
            validate(
                need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
            )}
        )
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
        
        req(input$OC)
        df_colors <- data.frame(lapply(input$OC, function(i) {
            input[[paste0("col_", i)]]
        }))
        
        vColors <- as.vector(t(df_colors))
        # names(vColors) <- names_outputsR()
        names(vColors) <- input$OC
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
        req(valColvar_scatterR)
        df_colors <- data.frame(lapply(valColvar_scatterR(), function(i) {
            input[[paste0("col_", i, "_sc")]]
        }))
        
        vColors <- as.vector(t(df_colors))
        req(vColors)
        names(vColors) <- valColvar_scatterR()
        
        vColors
    })
    
    
    # Plot Tab ----
    
    ## Errorbar Selection ----
    output$errorbar_var <- renderUI({
        
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
    
    
    # save default values in a list upon clicking action button
    
    # deactivated in ui -------------------------------------------
    
    # lDefault ----
    # variant with automatic input of chosen filters as default values
    lDefault <- eventReactive(input$updateDefaultList,  { as.list(defaults_input()) } )
    ## output ----
    # Output of list with default values
    output$lDefault <- renderPrint({
        req(lDefault)
        print(lDefault())
    })
    
    
    # Boxplot -------------------------------------------------------------
    
    output$pBoxplot <- renderPlot({
        
        validate(
            need(input$repvar != input$inputend, "Replication variable can't be an input variable")
        )
        
        validate(
            need(isTruthy(input$boxplotOutputVar), "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
        )
        
        req(input$boxplotGroupVar)
        
        d <- data_prefiltered()
        
        for(i in names_inputsR()){
            d[,i] <- as.factor(d[,i])
        }
        
        boxplot <- tryCatch({
            ggplot(d, aes_string(fill = input$boxplotGroupVar,
                                 col = input$boxplotGroupVar,
                                 x = input$boxplotOutputVar)
            )}, error = function(e) {
                err_ <- ""
                validate(
                    need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                )
            }
        )
        
        if(input$radioFacetDistribution == "grid"){
            
            if (any(input$facet_distribution_rows %in% input$facet_distribution_cols)) {
                err_ <- ""
                validate(
                    need(err_ != "", "Faceting variables can only appear in row or cols, not both")
                )
            }
            
            boxplot <- 
                tryCatch({
                    
                    frows_distribution <- input$facet_distribution_rows %>%
                        str_replace_all(",", "+") %>%
                        rlang::parse_exprs()
                    
                    fcols_distribution <- input$facet_distribution_cols %>%
                        str_replace_all(",", "+") %>%
                        rlang::parse_exprs()
                    
                    boxplot + 
                        facet_grid(vars(!!!frows_distribution),
                                   vars(!!!fcols_distribution),
                                   labeller = "label_both"
                        )}, error = function(e) {
                            err_ <- ""
                            validate(
                                need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                            )
                        })
        }
        
        if(input$radioFacetDistribution == "wrap"){
            
            boxplot <-
                tryCatch({
                    
                    facets_distribution <- input$facet_distribution_wrap %>%
                        str_replace_all(",", "+") %>%
                        rlang::parse_exprs()
                    
                    boxplot +
                        facet_wrap(vars(!!!facets_distribution),
                                   labeller = "label_both"
                        ) }, error = function(e) {
                            err_ <- ""
                            validate(
                                need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                            )
                        })
        }
        
        if(input$boxplottype == "Densityplot"){
            if("Density" %in% input$densitytype){
                boxplot <-
                    tryCatch({
                        boxplot + 
                            suppressWarnings(geom_density(aes(y = ..density..), 
                                                          alpha = input$alpha)
                            )}, error = function(e) {
                                err_ <- ""
                                validate(
                                    need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                                )
                            })
            }
            
            if("Histogram" %in% input$densitytype){
                boxplot <-
                    tryCatch({
                        boxplot + 
                            suppressWarnings(geom_histogram(aes(y = ..density..),
                                                            bins = input$bins,
                                                            alpha = input$alpha,
                                                            position = input$hist_position)  
                            )}, error = function(e) {
                                err_ <- ""
                                validate(
                                    need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                                )
                            })
            }
            
            tryCatch({
                suppressMessages(suppressWarnings(print(boxplot)))
                
            }, error = function(e) {
                err_ <- ""
                validate(
                    need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                )
            })
            
        } else {
            
            tryCatch({
                
                if(input$boxplottype == "Violinplot"){
                    suppressMessages(suppressWarnings(print(boxplot + geom_violin(aes_string(
                        x = input$boxplotGroupVar,
                        y = input$boxplotOutputVar,
                        color = input$boxplotGroupVar,
                        fill = input$boxplotGroupVar),
                        alpha = input$alpha))))
                } else {
                    suppressMessages(suppressWarnings(print(boxplot + geom_boxplot(aes_string(
                        x = input$boxplotGroupVar,
                        y = input$boxplotOutputVar,
                        color = input$boxplotGroupVar,
                        fill = input$boxplotGroupVar),
                        alpha = input$alpha))))
                }
            }, error = function(e) {
                err_ <- ""
                validate(
                    need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                )
            })
        }
    })
    
    
    # PLOT -------------------------------------------------------------------
    
    ## df_scatterplot ----
    df_scatterplot <- reactive({
        
        # 1 line df with default values for variables that are checked
        
        ### derfault_df ----
        default_df <- defaults_input()
        
        ## sim_par ----
        # vector of names of simulation parameters
        sim_par <- input$repvar_scatter
        
        sim_par <- c(sim_par, input$colvar_scatter)
        
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
        d
    })
    
    ## plot_object_scatter ----
    plot_object_scatter <- reactive({
        
        req(lUiColors_scatter())
        
        colScale_scatter <- scale_colour_manual(values = lUiColors_scatter())
        
        p1 <- tryCatch({
            p1 <- ggplot(
                df_scatterplot(), 
                aes_string(x = input$OC_scatter[1], y = input$OC_scatter[2])
            ) + geom_point(aes(colour = factor(get(input$colvar_scatter)))) + 
                labs(colour = input$colvar_scatter)
        }, error = function(e) {
            err_ <- ""
            validate(
                need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
            )
        }
        )
        
        if(input$checkboxPalette_scatter){
            p1 <- p1 + colScale_scatter
        }
        
        
        facets <- input$facet_wrap_scatter %>%
            str_replace_all(",", "+") %>%
            rlang::parse_exprs()
        
        frows <- input$facet_rows_scatter %>%
            str_replace_all(",", "+") %>%
            rlang::parse_exprs()
        
        fcols <- input$facet_cols_scatter %>%
            str_replace_all(",", "+") %>%
            rlang::parse_exprs()
        
        
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
                facet_wrap(vars(!!!facets), labeller = "label_both"
                )
        }
        
        p1
    })
    
    ## plot_scatter output ----
    output$plot_scatter <- renderPlot({
        
        validate(
            need(input$OC_scatter, "No OCs chosen")
        )
        
        validate(
            need(any(input$chooseDT_search_columns != ""), "Please specify default values first")
        )
        
        tryCatch({
            print(plot_object_scatter())  
        }, error = function(e) {
            err_ <- ""
            validate(
                need(err_ != "", "Change default values or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
            )  
        })
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
        default_df <- defaults_input()
        
        # vector of names of simulation parameters
        sim_par <- input$x
        
        # Code$sim_par <- paste(input$x, input$facet_rows, input$facet_cols, input$linetype, sep = ", ")
        Code$x <- input$x
        
        if(input$checkboxLinetype){
            sim_par <- c(sim_par, input$linetype)
        }
        
        if(input$radioFacet == "grid"){
            sim_par <- c(sim_par, input$facet_rows, input$facet_cols)
        }
        
        if(input$radioFacet == "wrap"){
            sim_par <- c(sim_par, input$facet_wrap)
        }
        
        if(input$checkboxColor){
            sim_par <- c(sim_par, input$color)
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
            df_plot_ <- tryCatch({ subset(data_filteredR(), eval(parse(text = bedingung))) },
                                 error = function(e) {
                                     err_ <- ""
                                     validate(
                                         need(err_ != "", "Change default values or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                                     )
                                 })
        } else {
            df_plot_ <- data_filteredR()
        }
        
        df_plot_ # return data frame
    })
    
    
    # df_plot output ----
    output$df_plot <- DT::renderDataTable({
        validate(
            need(any(input$chooseDT_search_columns != ""), "    Please specify default values first")
        )
        df_plot()
    },
    options = list(scrollX = TRUE)
    )
    
    
    # data_longer ----
    # Transform dataset to long format on chosen output variables for easy plotting
    data_longer <- reactive({
        req(input$OC)
        d <- df_plot()
        d <- tryCatch({ d %>%
                pivot_longer(
                    cols = input$OC,
                    names_to = "OC",
                    values_to = "value"
                )}, error = function(e) { 
                    err_ <- ""
                    validate(
                        need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                    )
                }
        )
        
        if(input$checkboxErrorbar){
            
            if(input$radioErrorsymmetry == "symmetrical"){
                
                validate(
                    need(isTruthy(input$errorvars), "Please define error variables")
                )
                
                d <- tryCatch({
                    d %>%
                        pivot_longer(
                            cols = input$errorvars,
                            names_to = "errorvar_name",
                            values_to = "error"
                        )}, error = function(e) {
                            err_ <- ""
                            validate(
                                need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
                            )   
                        })
                
                bedingung_errorbar <- paste0("(OC == '", input$OC, "' & errorvar_name == '", input$errorvars, "')", collapse = " | ")
                
                d <- subset(d, eval(parse(text = bedingung_errorbar)))
                
            } else {
                
                validate(
                    need(isTruthy(input$errorvars_upper), "Please define variable for upper bound/deviation")
                )
                
                validate(
                    need(isTruthy(input$errorvars_lower), "Please define variable for upper bound/deviation")
                )
                
                d <- tryCatch({
                    d %>%
                        pivot_longer(
                            cols = input$errorvars_upper,
                            names_to = "errorvar_upper_name",
                            values_to = "error_upper"
                        )
                }, error = function(e) {
                    err_ <- ""
                    validate(
                        need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go to the tab with the data and then re-define default values")
                    )     
                })
                
                d <- tryCatch({
                    d %>%
                        pivot_longer(
                            cols = input$errorvars_lower,
                            names_to = "errorvar_lower_name",
                            values_to = "error_lower"
                        )
                }, error = function(e) {
                    err_ <- ""
                    validate(
                        need(err_ != "", "Select a different variable or if a new dataset has been uploaded, go to the tab with the data and then re-define default values")
                    )     
                })
                
                bedingung_errorbar <- paste0("(OC == '", input$OC, "' & errorvar_upper_name == '", input$errorvars_upper, "' & errorvar_lower_name == '", input$errorvars_lower, "')", collapse = " | ")
                
                d <- subset(d, eval(parse(text = bedingung_errorbar)))
                
            }
        }
        as.data.frame(d)
    })
    
    
    # Plots ---------------------------------------
    
    ## lineplot_object ----
    # Plot based on which dimensions are chosen
    lineplot_object <- reactive({
        
        # data_lp <- reactive({
        #     
        #     data_output <- data_longer()
        #     
        #     if(input$checkboxLinetype){
        #         
        #         data_output[[input$linetype]] <- as.factor(data_output[[input$linetype]])
        #         
        #     }
        #     data_output
        # })
        data_lp <- {
            
            data_output <- data_longer()
            
            if(input$checkboxLinetype){
                
                data_output[[input$linetype]] <- as.factor(data_output[[input$linetype]])
                
            }
            data_output
        }
        
        # colScale <- reactive({
        #     if(input$checkboxColor){
        #         scale_colour_manual(values = lUiColordim())
        #     } else {
        #         scale_colour_manual(values = lUiColors())
        #     }
        # })
        colScale <- {
            if(input$checkboxColor){
                scale_colour_manual(values = lUiColordim())
            } else {
                scale_colour_manual(values = lUiColors())
            }
        }
        
        p1 <- ggplot(
            data_lp, 
            aes_string(x = input$x, y = "value")
        ) 
        
        if(input$checkboxPalette_OC){
            p1 <- p1 + colScale
        }
        
        if(input$checkboxPalette_dim){
            p1 <- p1 + colScale
        }
        
        if(input$checkboxLine){
            if(input$checkboxColor){
                p1 <-
                    p1 +
                    geom_line(aes(
                        y = value,
                        color  = factor(get(input$color))),
                        size = input$linesize) + 
                  labs(colour = input$color)
                
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
        
        
        if(input$checkboxLinetype){
            
            p1 <-
                p1 +
                aes_string(
                    linetype = input$linetype,
                    shape = input$linetype
                ) + 
              labs(linetype = input$linetype, shape = input$linetype)
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
                facet_wrap(vars(!!!facets), labeller = "label_both"
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
        
        p1 + theme(legend.key.size = unit(4, 'cm'))
        
    })
    
    ## plot_object ----
    # create plot_object with final settings
    plot_object <- reactive({
        
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
        
        p1
    })
    
    # plottype observe ----
    observe({
        
        if(input$plottype){
            
            output$lineplotly <- renderPlotly({
                ggplotly(plot_object())
            })
            
        } else {
            
            output$lineplot <- renderPlot({
                plot_object()
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
            need(input$OC, "No OCs chosen")
        )
        
        validate(
            need(input$x, "Please specify default values first")
        )
        
        validate(
            need(any(input$chooseDT_search_columns != ""), "Please specify default values first")
        )
        
        if(input$plottype){
            plotlyOutput("lineplotly",
                         height = input$plotheight,
                         width = input$plotwidth
            )
        }else{
            plotOutput("lineplot",
                       height = input$plotheight,
                       width = input$plotwidth
            )
        }
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
    
    
    ## observeEvents ----
    
    observeEvent(input$animationCloseButton, {
        output$animationOutDynamic <- renderImage({ req("") })
    })
    
    
    #only renders if Button is clicked (isolate prevents reload on tabswitch)
    observeEvent(input$animationRenderButton,{
        
        ### animationOutDynamic ---- 
        output$animationOutDynamic <- renderImage({
            
            tryCatch({
                #prevents rendering if button isnt clicked
                if(input$animationRenderButton == 0) return()
                
                #isolate prevents rerender on tabswitch
                isolate({
                    #validates select input
                    validate(need(
                        input$animateIteratorSelect,
                        "Please specify iteration variable first"))
                    
                    # temporary file, saves render
                    outfileDyn <- tempfile(fileext='.gif')
                    
                    #Plot(with PlotTab customisation) + animation attributes
                    ap <- plot_object() + 
                        transition_states(states = !!(as.symbol(input$animateIteratorSelect)) ,
                                          transition_length = 1,
                                          state_length = 1,
                                          wrap = TRUE
                        ) +
                        enter_fade()+
                        exit_fade()
                    
                    # animation rendering
                    anim_save("outfileDyn.gif", 
                              animate(ap,
                                      nframes = input$frameAmount,
                                      fps = input$renderFPS,
                                      duration = input$durationAnimation,
                                      height = input$plotheight,
                                      width = input$plotwidth,
                              ))
                    
                    
                    # Returns rendering in gif-form
                    list(src = "outfileDyn.gif",
                         contentType = 'image/gif'
                    )
                })
                
            }, error = function(e) {
                err_ <- ""
                validate(need(err_ != "", "Animation cannot be created"))
            })
        },
        #Deletes temporary Files after execution
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
            
            print(plot_object())
            dev.off()
        }
    )
    
    # Code ----
    Code <- reactiveValues()
}

# shinyApp ----
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
