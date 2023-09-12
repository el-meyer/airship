#' Runs the app "AIRSHIP".
#'
#' @param ...  Further arguments to be passed to Shiny's "shinyApp()" function
#' 
#' @return No return value
#'
#' @examples
#' \dontrun{
#' airship()
#' }
#' @export
airship <- function(...) {
  
  # Global Options ----
  options(shiny.sanitize.errors = FALSE) 
  options(shiny.maxRequestSize = 50*1024^2)
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
        title = "AIRSHIP"
      ), 
      
      ## Sidebar -----
      shinydashboard::dashboardSidebar(
        width = 300,
        
        shinydashboard::sidebarMenu(
          id = "sidebarMenu",
          
          ### Data Settings ----
          shinydashboard::menuItem(
            text = "Data Settings",
            tabName = "data_settings", 
            icon = shiny::icon("gear"),
            
            shiny::checkboxInput(
              inputId = "checkboxExampleData", 
              label = "Use example dataset"
            ),
            
            shiny::conditionalPanel(
              condition = "input.checkboxExampleData == 0",
              
              shiny::checkboxInput(
                inputId = "checkboxFactsData", 
                label = "Use FACTS aggregated simulations"
              ),
              
              shiny::fileInput(
                inputId = "file", 
                label = "Choose file to upload"
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
            
            shiny::conditionalPanel(
              condition = "input.checkboxExampleData == 0 && input.checkboxFactsData == 0",
              
              shiny::checkboxInput(
                inputId = "checkboxRepvar",
                label = "Aggregate over individual simulations?"
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
                label = "Select the deviation you want to calculate",
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
          
          ### Data ----               
          shinydashboard::menuItem(
            text = "Data",
            tabName = "data",
            icon = shiny::icon("database")
          ),
          
          ### Default Values ----
          shinydashboard::menuItem(
            text = "Default values", 
            tabName = "default", 
            icon = shiny::icon("pen")
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
          
          ### Line/Dotplot ----
          shinydashboard::menuItem(
            text = "Line/Dotplot", 
            tabName = "plot", 
            icon = shiny::icon("chart-line")
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
          
          ### Help ----
          shinydashboard::menuItem(
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
        tags$head(tags$style(css)),
        shinybusy::add_busy_spinner(spin = "fading-circle"),
        shinybusy::add_busy_bar(color = "red", height = "8px"),
        
        shinydashboard::tabItems(
          
          ### Data Settings ----
          shinydashboard::tabItem(
            tabName = "data_settings",
          ),
          
          ### Data ----
          shinydashboard::tabItem(
            tabName = "data",
            DT::dataTableOutput("dataDT"),
            shiny::uiOutput("dataDT_summarized")
          ),
          
          ### Default Values ----
          shinydashboard::tabItem(
            tabName = "default",
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
                width = 12,
                #### Scatterplot Output ----
                shiny::uiOutput("boxplot_ui")
              )
            ),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(
                width = 4,
                
                #### DV overview ----
                shinyWidgets::dropdown(
                  label = "Default value overview",
                  shiny::HTML("Current default value settings"),
                  shiny::uiOutput("defaults_df_ui_box")
                ),
                
                shiny::hr(),
                
                #### x-axis ----
                shiny::selectInput(
                  inputId = "boxplotGroupVar",
                  label = "Select x-axis",
                  choices = NULL
                ),
                
                #### y-Axis ----
                shiny::selectInput(
                  inputId = "boxplotOutputVar",
                  label = "Select y-axis",
                  choices = NULL
                ),
                
                #### Plottype ----
                shiny::HTML("<b>Choose plot type</b>"),
                
                shiny::radioButtons(
                  inputId = "boxplottype",
                  label = "Boxplot or Violinplot",
                  choices =c("Boxplot", "Violinplot"),
                  selected = "Boxplot",
                ),
                
                #### Coord Flip ----
                shiny::checkboxInput(
                  inputId = "boxplot_flip",
                  label = "Flip coordinates?",
                  value = FALSE,
                )
                
              ),
              
              shiny::column(
                width = 4,
                
                #### Facet Dimension ----
                shiny::radioButtons(
                  inputId = "radioFacetDistribution",
                  label = "Add a facet dimension?",
                  choices = c("no", "grid", "wrap")
                ),
                
                ##### Grid ----
                shiny::conditionalPanel(
                  condition = "input.radioFacetDistribution == 'grid'",
                  
                  shiny::selectInput(
                    inputId = "facet_distribution_rows", 
                    label = "Add row variable", 
                    choices = NULL,
                    multiple = TRUE
                  ),
                  
                  shiny::selectInput(
                    inputId = "facet_distribution_cols", 
                    label = "Add column variable", 
                    choices = NULL,
                    multiple = TRUE
                  )
                  
                ),
                
                ##### Wrap ----
                shiny::conditionalPanel(
                  condition = "input.radioFacetDistribution == 'wrap'",
                  shiny::selectizeInput(
                    inputId = "facet_distribution_wrap", 
                    label = "Choose variables to facet wrap",
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                #### Color Dimension ----
                shiny::checkboxInput(
                  inputId = "checkboxColorDist",
                  label = "Add a color dimension?"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxColorDist != 0",
                  shiny::selectInput(
                    inputId = "colvar_dist",
                    label = "Choose color variable",
                    choices = NULL
                  ),
                  
                  #### Color Button ----
                  shiny::checkboxInput(
                    inputId = "checkboxPalette_boxplot",
                    label = "Specify your own colors?"
                  ),
                  
                  shiny::absolutePanel(
                    shinyWidgets::dropdownButton(
                      label = "Color choices",
                      status = "primary",
                      circle = TRUE,
                      right = TRUE,
                      icon = icon("paintbrush"),
                      shiny::uiOutput("colors_boxplot_ui"),
                      inputId = "dropdown_colors_boxplot"
                    ),
                    draggable = TRUE
                  )
                  
                )
                
              ),
              
              shiny::column(
                width = 4,
                
                #### Interactive Plot ----
                shinyWidgets::switchInput(
                  inputId = "plottype_boxplot",
                  label = "Interactive Plot?",
                  value = FALSE,
                  size = "small"
                ),
                
                #### Style options ----
                shiny::actionButton(
                  inputId = "change_style_boxplot", 
                  label = "Style options"
                ),
                
                shinyBS::bsModal(
                  id = "modal_style_boxplot", 
                  title = "Change style and size of plot", 
                  trigger = "change_style_boxplot", 
                  size = "large",
                  
                  shiny::sliderInput(
                    inputId = "plotwidth_boxplot",
                    label = "Plot width (px)",
                    value = 1000,
                    min = 600,
                    max = 1500
                  ),
                  
                  shiny::sliderInput(
                    inputId = "plotheight_boxplot",
                    label = "Plot height (px)",
                    value = 600,
                    min = 300,
                    max = 1000
                  ),
                  
                  shiny::sliderInput(
                    inputId = "alpha",
                    label = "Select transparency",
                    min = 0,
                    max = 1,
                    value = 0.1,
                    step = 0.1
                  ),
                  
                  shiny::numericInput(
                    inputId = "plotfontsize_boxplot",
                    label = "Font size",
                    value = 11,
                    min = 1,
                    max = 30,
                    step = 0.5
                  ),
                  
                  shiny::selectInput(
                    inputId = "plotfont_boxplot",
                    label = "Font",
                    choices = 
                      c(
                        "sans", 
                        "serif", 
                        "mono"
                      )
                  ),
                  
                  shiny::checkboxInput(
                    inputId = "checkboxTheme_boxplot",
                    label = "Change the theme?"
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxTheme_boxplot != 0",
                    
                    shiny::radioButtons(
                      inputId = "plottheme_boxplot",
                      label = "Select the theme",
                      choices = c(
                        "Grey", 
                        "White", 
                        "Linedraw",
                        "Light", 
                        "Minimal", 
                        "Classic"
                      )
                    )
                    
                  )
                ),
                
                #### Download Plot Button ----
                shiny::actionButton(
                  inputId = "save_plot_boxplot", 
                  label = "Download plot"
                ),
                
                #### Add Titel ----
                shiny::checkboxInput(
                  inputId = "checkboxTitle_boxplot",
                  label = "Add title"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxTitle_boxplot != 0",
                  
                  shiny::textInput(
                    inputId = "plot_title_boxplot",
                    label = "Enter the plot title"
                  ),
                  
                  shiny::radioButtons(
                    inputId = "plot_title_place_boxplot",
                    label = "Title alignment",
                    choices = c(
                      "left" = 0, 
                      "center" = 0.5, 
                      "right" = 1
                    )
                  ),
                  
                  shiny::numericInput(
                    inputId = "plot_title_size_boxplot",
                    label = "Size",
                    value = 30,
                    min = 1,
                    max = 50,
                    step = 1
                  ),
                  
                  colourpicker::colourInput(
                    inputId = "plot_title_colour_boxplot", 
                    label = "Title colour:",
                    showColour = "both",
                    value = "black",
                    allowTransparent = FALSE
                  )
                  
                ),
                
                shiny::hr(),
                
                #### Change Axis Labels ----
                shiny::checkboxInput(
                  inputId = "checkboxAxis_boxplot",
                  label = "Change axis labels"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxAxis_boxplot != 0",
                  
                  shiny::textInput(
                    inputId = "xLab_boxplot",
                    label = "X-axis label:"
                  ),
                  
                  shiny::textInput(
                    inputId = "yLab_boxplot",
                    label = "Y-axis label:"
                  )
                  
                ),
                
                #### Download Plot Window ----
                shinyBS::bsModal(
                  id = "modal_boxplot", 
                  title = "Download plot", 
                  trigger = "save_plot_boxplot", 
                  size = "medium",
                  
                  shiny::selectInput(
                    inputId = "download_type_boxplot", 
                    label = "Choose file type",
                    choices = c(
                      "png", 
                      "jpeg", 
                      "tiff"
                    )
                  ),
                  
                  shiny::selectInput(
                    inputId = "download_unit_boxplot", 
                    label = "Choose unit",
                    choices = c(
                      "px", 
                      "in", 
                      "cm", 
                      "mm"
                    )
                  ),
                  
                  shiny::numericInput(
                    inputId = "download_plotwidth_boxplot",
                    label = "Plot width",
                    value = 1000,
                    min = 1,
                    max = 2000
                  ),
                  
                  shiny::numericInput(
                    inputId = "download_plotheight_boxplot",
                    label = "Plot height",
                    value = 600,
                    min = 1,
                    max = 2000
                  ),
                  
                  shiny::numericInput(
                    inputId = "download_resolution_boxplot",
                    label = "Resolution",
                    value = 72,
                    min = 1, 
                    max = 1000
                  ),
                  
                  shiny::textInput(
                    inputId = "download_name_boxplot", 
                    label = "Specify file name"
                  ),
                  
                  shiny::downloadButton(
                    outputId = "download_plot_boxplot", 
                    label = "Download"
                  )
                )
                
              ),
              
            ),
            
            shiny::hr(),
            shiny::hr(),
            
            #### Plotted Data ----
            shiny::h2("Plotted Data"),
            shiny::br(),
            shiny::fluidRow(
              DT::dataTableOutput(
                outputId = "df_boxplot"
              )
            ),
            shiny::hr(),
            shiny::br(),
            shiny::br(),
            shiny::br()
            
          ),
          
          
          ### Line/Dotplot ----
          shinydashboard::tabItem(
            tabName = "plot",
            
            shiny::fluidRow(
              shiny::column(
                width = 12,
                #### Plot Output ----
                shiny::uiOutput("lineplot_ui"),
                # imageOutput("animationOutDynamic",
                #             inline = TRUE)
              )
            ),
            
            shiny::hr(),
            
            shiny::fluidRow(
              shiny::column(
                width = 3,
                
                #### DV overview----
                shinyWidgets::dropdown(
                  label = "Default value overview",
                  shiny::HTML("Current default value settings"),
                  shiny::uiOutput("defaults_df_ui_line")
                ),
                
                shiny::hr(),
                
                #### x-axis ----
                shiny::selectInput(
                  inputId = "x", 
                  label = "x-axis", 
                  choices = NULL
                ),
                
                #### y-axis ----
                shiny::selectizeInput(
                  inputId = "OC", 
                  label = "y-axis", 
                  choices = NULL,
                  multiple = TRUE
                ),
                
                #### Errorbars ----
                shiny::checkboxInput(
                  inputId = "checkboxErrorbar",
                  label = "Add errorbars?"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxErrorbar != 0",
                  
                  shiny::radioButtons(
                    inputId = "radioErrorsymmetry",
                    label = "Are the errors symmetrical (1 error variable) or asymmetrical (2 error variables)",
                    choices = c("symmetrical", "asymmetrical")
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.radioErrorsymmetry == 'asymmetrical'",
                    
                    shiny::radioButtons(
                      inputId = "radioErrorstructure",
                      label = "Do the variables represent the upper and lower deviation from the estimate or the upper and lower bounds?",
                      choices = c("deviation", "bounds")
                    )
                  ),
                  shiny::HTML("Select the corresponding error variable (Sd) for every OC chosen."),
                  shiny::HTML("For a correct display, the error variables have to be in the same order as the OCs chosen above"),
                  shiny::uiOutput("errorbar_var")
                )
              ),
              
              shiny::column(
                width = 3,
                
                #### Facet Dimension ----
                shiny::radioButtons(
                  inputId = "radioFacet",
                  label = "Add a facet dimension?",
                  choices = c("no", "grid", "wrap")
                ),
                
                ##### Grid ----
                shiny::conditionalPanel(
                  condition = "input.radioFacet == 'grid'",
                  
                  shiny::selectInput(
                    inputId = "facet_rows", 
                    label = "Add row variable", 
                    choices = NULL,
                    multiple = TRUE
                  ),
                  
                  shiny::selectInput(
                    inputId = "facet_cols", 
                    label = "Add column variable", 
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                ##### Wrap ----
                shiny::conditionalPanel(
                  condition = "input.radioFacet == 'wrap'",
                  
                  shiny::selectizeInput(
                    inputId = "facet_wrap", 
                    label = "Choose variables to facet wrap",
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                #### Linetype ----
                shiny::checkboxInput(
                  inputId = "checkboxLinetype",
                  label = "Add a linetype dimension?"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxLinetype != 0",
                  
                  shiny::selectInput(
                    inputId = "linetype",
                    label = "Choose linetype variable",
                    choices = NULL
                  )
                ),
                
                #### Color dimension ----
                shiny::conditionalPanel(
                  condition = "input.OC.length == 1",
                  
                  shiny::checkboxInput(
                    inputId = "checkboxColor",
                    label = "Add a color dimension?"
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxColor != 0",
                    
                    shiny::selectInput(
                      inputId = "color",
                      label = "Choose color variable",
                      choices = NULL
                    )
                  )
                )
              ),
              
              shiny::column(
                width = 3,
                
                #### Interactive Plot ----
                shinyWidgets::switchInput(
                  inputId = "plottype",
                  label = "Interactive Plot?",
                  value = FALSE,
                  size = "small"
                ),
                
                #### Style options ----
                shiny::actionButton(
                  inputId = "change_style", 
                  label = "Style options"),
                
                shinyBS::bsModal(
                  id = "modal_style", 
                  title = "Change style and size of plot", 
                  trigger = "change_style", 
                  size = "large",
                  
                  shiny::sliderInput(
                    inputId = "plotwidth",
                    label = "Plot width (px)",
                    value = 1000,
                    min = 600,
                    max = 1500
                  ),
                  
                  shiny::sliderInput(
                    inputId = "plotheight",
                    label = "Plot height (px)",
                    value = 600,
                    min = 300,
                    max = 1000
                  ),
                  
                  shiny::sliderInput(
                    inputId = "linesize",
                    label = "Line and point size",
                    value = 0.6,
                    min = 0.1,
                    max = 3,
                    step = 0.1
                  ),
                  
                  shiny::numericInput(
                    inputId = "plotfontsize",
                    label = "Font size",
                    value = 11,
                    min = 1,
                    max = 30,
                    step = 0.5
                  ),
                  
                  shiny::selectInput(
                    inputId = "plotfont",
                    label = "Font",
                    choices = 
                      c(
                        "sans", 
                        "serif", 
                        "mono"
                      )
                  ),
                  
                  shiny::checkboxInput(
                    inputId = "checkboxLine",
                    label = "Add lines?",
                    value = TRUE
                  ),
                  
                  shiny::checkboxInput(
                    inputId = "checkboxPoint",
                    label = "Add points?",
                    value = TRUE
                  ),
                  
                  shiny::checkboxInput(
                    inputId = "checkboxTheme",
                    label = "Change the theme?"
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxTheme != 0",
                    
                    shiny::radioButtons(
                      inputId = "plottheme",
                      label = "Select the theme",
                      choices = c(
                        "Grey", 
                        "White", 
                        "Linedraw",
                        "Light", 
                        "Minimal", 
                        "Classic"
                      )
                    )
                    
                  )
                ),
                
                #### Download Plot Button ----
                shiny::actionButton(
                  inputId = "save_plot", 
                  label = "Download plot"
                ),
                
                #### Color choice ----
                shiny::conditionalPanel(
                  condition = "input.checkboxColor == 0",
                  shiny::checkboxInput(
                    inputId = "checkboxPalette_OC", 
                    label = "Specify your own colors?"
                  ),
                  
                  ##### Brush button ----
                  shiny::absolutePanel(
                    shinyWidgets::dropdownButton(
                      label = "Color Choices",
                      status = "primary",
                      circle = TRUE,
                      right = TRUE,
                      icon = icon("paintbrush"),
                      tooltip = TRUE,
                      shiny::uiOutput("colors_ui"),
                      inputId = "dropdown_colors"
                    ),
                    draggable = TRUE
                  )
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxColor != 0",
                  shiny::checkboxInput(
                    inputId = "checkboxPalette_dim",
                    # TODO: Check if need Fix: This text wont change on checking checkboxColor
                    label = "Specify your own colors?"
                  ),
                  shiny::absolutePanel(
                    shinyWidgets::dropdownButton(
                      label = "Color choices",
                      status = "primary",
                      circle = TRUE,
                      right = TRUE,
                      icon = icon("paintbrush"),
                      shiny::uiOutput("colordim_ui"),
                      inputId = "dropdown_colordim"
                    ),
                    draggable = TRUE
                  )
                )
                
              ),
              
              shiny::column(
                width = 3,
                
                #### Add Titel ----
                shiny::checkboxInput(
                  inputId = "checkboxTitle",
                  label = "Add title"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxTitle != 0",
                  
                  shiny::textInput(
                    inputId = "plot_title",
                    label = "Enter the plot title"
                  ),
                  
                  shiny::radioButtons(
                    inputId = "plot_title_place",
                    label = "Title alignment",
                    choices = c(
                      "left" = 0, 
                      "center" = 0.5, 
                      "right" = 1
                    )
                  ),
                  
                  shiny::numericInput(
                    inputId = "plot_title_size",
                    label = "Size",
                    value = 30,
                    min = 1,
                    max = 50,
                    step = 1
                  ),
                  
                  colourpicker::colourInput(
                    inputId = "plot_title_colour", 
                    label = "Title colour:",
                    showColour = "both",
                    value = "black",
                    allowTransparent = FALSE
                  )
                  
                ),
                
                shiny::hr(),
                
                #### Change Axis Labels ----
                shiny::checkboxInput(
                  inputId = "checkboxAxis",
                  label = "Change axis labels"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxAxis != 0",
                  
                  shiny::textInput(
                    inputId = "xLab",
                    label = "X-axis label:"
                  ),
                  
                  shiny::textInput(
                    inputId = "yLab",
                    label = "Y-axis label:"
                  )
                  
                ),
                
                shiny::hr(),
                
                #### Download Plot Window ----
                shinyBS::bsModal(
                  id = "modal", 
                  title = "Download plot", 
                  trigger = "save_plot", 
                  size = "medium",
                  
                  shiny::selectInput(
                    inputId = "download_type", 
                    label = "Choose file type",
                    choices = c(
                      "png", 
                      "jpeg", 
                      "tiff"
                    )
                  ),
                  
                  shiny::selectInput(
                    inputId = "download_unit", 
                    label = "Choose unit",
                    choices = c(
                      "px", 
                      "in", 
                      "cm", 
                      "mm"
                    )
                  ),
                  
                  shiny::numericInput(
                    inputId = "download_plotwidth",
                    label = "Plot width",
                    value = 1000,
                    min = 1,
                    max = 2000
                  ),
                  
                  shiny::numericInput(
                    inputId = "download_plotheight",
                    label = "Plot height",
                    value = 600,
                    min = 1,
                    max = 2000
                  ),
                  
                  shiny::numericInput(
                    inputId = "download_resolution",
                    label = "Resolution",
                    value = 72,
                    min = 1, 
                    max = 1000
                  ),
                  
                  shiny::textInput(
                    inputId = "download_name", 
                    label = "Specify file name"),
                  
                  shiny::downloadButton(
                    outputId = "download_plot", 
                    label = "Download")
                )
              ),
              
              #### Animation Input ----
              # shiny::column(3,
              ##### animateIteratorSelect ----
              #        shiny::selectInput(
              #          "animateIteratorSelect", 
              #          "Choose variable to animate over:", 
              #          choices = NULL
              #        ),
              #        shiny::actionButton(
              #          "animationRenderButton",
              #          "Render animation"
              #        ),
              #        
              ##### render options ----
              #        shiny::actionButton("changeRender", label = "Render options"),
              #        
              #        shinyBS::bsModal("modal_render",
              #                "Change render options",
              #                trigger = "changeRender",
              #                size = "large",
              #                
              #                shiny::sliderInput(
              #                  "frameAmount",
              #                  "number of frames to render",
              #                  value = 100,
              #                  min = 10,
              #                  max = 1000
              #                ),
              #                
              #                shiny::sliderInput(
              #                  "renderFPS",
              #                  "frames per second",
              #                  value = 10,
              #                  min = 1,
              #                  max = 120
              #                ),
              #                
              #                shiny::numericInput(
              #                  "durationAnimation",
              #                  "length of animation in seconds",
              #                  value = 10,
              #                  min = 1,
              #                  max = 1000,
              #                  step = 1
              #                )
              #        ),
              #        shiny::actionButton(
              #          "animationCloseButton",
              #          "Close animation"
              #        )
              # )
            ),
            shiny::hr(),
            
            #### Plotted Data ----
            shiny::h2("Plotted Data"),
            shiny::br(),
            shiny::fluidRow(
              DT::dataTableOutput("df_plot")
            ),
            
            shiny::hr(),
            shiny::br(),
            shiny::br(),
            shiny::br()
          ),
          
          ### Scatterplot ----
          shinydashboard::tabItem(
            tabName = "scatterplot",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                #### Scatterplot Output ----
                shiny::uiOutput("scatter_ui")
              )
            ),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(
                width = 3,
                
                #### DV overview----
                shinyWidgets::dropdown(
                  label = "Default value overview",
                  shiny::HTML("Current default value settings"),
                  shiny::uiOutput("defaults_df_ui_scatter")
                ),
                
                shiny::hr(),
                
                ##### OC to plot ----
                shiny::selectizeInput(
                  inputId = "OC_scatter", 
                  label = "Choose variables to plot (only first two will be plotted)", 
                  choices = NULL,
                  multiple = TRUE
                ),
                
              ),
              
              shiny::column(
                width = 3,
                
                ##### Facet dimension ----
                shiny::radioButtons(
                  inputId = "radioFacet_scatter",
                  label = "Add a facet dimension?",
                  choices = c("no", "grid", "wrap")
                ),
                
                ###### Grid ----
                shiny::conditionalPanel(
                  condition = "input.radioFacet_scatter == 'grid'",
                  
                  shiny::selectInput(
                    inputId = "facet_rows_scatter", 
                    label = "Add row variable", 
                    choices = NULL,
                    multiple = TRUE
                  ),
                  
                  shiny::selectInput(
                    inputId = "facet_cols_scatter", 
                    label = "Add column variable", 
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                ###### Wrap ----
                shiny::conditionalPanel(
                  condition = "input.radioFacet_scatter == 'wrap'",
                  
                  shiny::selectizeInput(
                    inputId = "facet_wrap_scatter", 
                    label = "Choose variables to facet wrap",
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                ###### Color param ----
                shiny::checkboxInput(
                  inputId = "checkboxColorScatter",
                  label = "Add a color dimension?"
                ),
                shiny::conditionalPanel(
                  condition = "input.checkboxColorScatter != 0",
                  
                  shiny::selectInput(
                    inputId = "colvar_scatter",
                    label = "Choose color variable",
                    choices = NULL
                  ),
                  
                  #### Color Button ----
                  shiny::checkboxInput(
                    inputId = "checkboxPalette_scatter",
                    label = "Specify your own colors?"
                  ),
                  
                  shiny::absolutePanel(
                    shinyWidgets::dropdownButton(
                      label = "Color choices",
                      status = "primary",
                      circle = TRUE,
                      right = TRUE,
                      icon = icon("paintbrush"),
                      shiny::uiOutput("colors_scatter_ui"),
                      inputId = "dropdown_colors_scatter"
                    ),
                    draggable = TRUE
                  )
                  
                )
                
              ),
              
              shiny::column(
                width = 4,
                
                #### Interactive Plot ----
                shinyWidgets::switchInput(
                  inputId = "plottype_scatter",
                  label = "Interactive Plot?",
                  value = FALSE,
                  size = "small"
                ),
                
                #### Style options ----
                shiny::actionButton(
                  inputId = "change_style_scatter", 
                  label = "Style options"
                ),
                
                shinyBS::bsModal(
                  id = "modal_style_scatter", 
                  title = "Change style and size of plot", 
                  trigger = "change_style_scatter", 
                  size = "large",
                  
                  shiny::sliderInput(
                    inputId = "plotwidth_scatter",
                    label = "Plot width (px)",
                    value = 1000,
                    min = 600,
                    max = 1500
                  ),
                  
                  shiny::sliderInput(
                    inputId = "plotheight_scatter",
                    label = "Plot height (px)",
                    value = 600,
                    min = 300,
                    max = 1000
                  ),
                  
                  shiny::sliderInput(
                    inputId = "pointsize_scatter",
                    label = "Point size",
                    value = 1,
                    min = 0.1,
                    max = 3,
                    step = 0.1
                  ),
                  
                  shiny::sliderInput(
                    inputId = "transparency_scatter",
                    label = "Transparency",
                    value = 0.8,
                    min = 0,
                    max = 1,
                    step = 0.05
                  ),
                  
                  shiny::numericInput(
                    inputId = "plotfontsize_scatter",
                    label = "Font size",
                    value = 11,
                    min = 1,
                    max = 30,
                    step = 0.5
                  ),
                  
                  shiny::selectInput(
                    inputId = "plotfont_scatter",
                    label = "Font",
                    choices = 
                      c(
                        "sans", 
                        "serif", 
                        "mono"
                      )
                  ),
                  
                  shiny::checkboxInput(
                    inputId = "checkboxTheme_scatter",
                    label = "Change the theme?"
                  ),
                  
                  shiny::conditionalPanel(
                    condition = "input.checkboxTheme_scatter != 0",
                    
                    shiny::radioButtons(
                      inputId = "plottheme_scatter",
                      label = "Select the theme",
                      choices = c(
                        "Grey", 
                        "White", 
                        "Linedraw",
                        "Light", 
                        "Minimal", 
                        "Classic"
                      )
                    )
                    
                  )
                ),
                
                #### Download Plot Button ----
                shiny::actionButton(
                  inputId = "save_plot_scatter", 
                  label = "Download plot"
                ),
                
                #### Add Titel ----
                shiny::checkboxInput(
                  inputId = "checkboxTitle_scatter",
                  label = "Add title"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxTitle_scatter != 0",
                  
                  shiny::textInput(
                    inputId = "plot_title_scatter",
                    label = "Enter the plot title"
                  ),
                  
                  shiny::radioButtons(
                    inputId = "plot_title_place_scatter",
                    label = "Title alignment",
                    choices = c(
                      "left" = 0, 
                      "center" = 0.5, 
                      "right" = 1
                    )
                  ),
                  
                  shiny::numericInput(
                    inputId = "plot_title_size_scatter",
                    label = "Size",
                    value = 30,
                    min = 1,
                    max = 50,
                    step = 1
                  ),
                  
                  colourpicker::colourInput(
                    inputId = "plot_title_colour_scatter", 
                    label = "Title colour:",
                    showColour = "both",
                    value = "black",
                    allowTransparent = FALSE
                  )
                  
                ),
                
                shiny::hr(),
                
                #### Change Axis Labels ----
                shiny::checkboxInput(
                  inputId = "checkboxAxis_scatter",
                  label = "Change axis labels"
                ),
                
                shiny::conditionalPanel(
                  condition = "input.checkboxAxis_scatter != 0",
                  
                  shiny::textInput(
                    inputId = "xLab_scatter",
                    label = "X-axis label:"
                  ),
                  
                  shiny::textInput(
                    inputId = "yLab_scatter",
                    label = "Y-axis label:"
                  )
                  
                ),
                
              ),
              
              #### Download Plot Window ----
              shinyBS::bsModal(
                id = "modal_scatter", 
                title = "Download plot", 
                trigger = "save_plot_scatter", 
                size = "medium",
                
                shiny::selectInput(
                  inputId = "download_type_scatter", 
                  label = "Choose file type",
                  choices = c(
                    "png", 
                    "jpeg", 
                    "tiff"
                  )
                ),
                
                shiny::selectInput(
                  inputId = "download_unit_scatter", 
                  label = "Choose unit",
                  choices = c(
                    "px", 
                    "in", 
                    "cm", 
                    "mm"
                  )
                ),
                
                shiny::numericInput(
                  inputId = "download_plotwidth_scatter",
                  label = "Plot width",
                  value = 1000,
                  min = 1,
                  max = 2000
                ),
                
                shiny::numericInput(
                  inputId = "download_plotheight_scatter",
                  label = "Plot height",
                  value = 600,
                  min = 1,
                  max = 2000
                ),
                
                shiny::numericInput(
                  inputId = "download_resolution_scatter",
                  label = "Resolution",
                  value = 72,
                  min = 1, 
                  max = 1000
                ),
                
                shiny::textInput(
                  inputId = "download_name_scatter", 
                  label = "Specify file name"),
                
                shiny::downloadButton(
                  outputId = "download_plot_scatter", 
                  label = "Download")
              )
              
            ),
            
            shiny::hr(),
            shiny::hr(),
            
            #### Plotted Data ----
            shiny::h2("Plotted Data"),
            shiny::br(),
            shiny::fluidRow(
              DT::dataTableOutput("df_scatterplot")
            ),
            
            shiny::hr(),
            shiny::br(),
            shiny::br(),
            shiny::br()
          ),
          
          
          ### HELP ----
          shinydashboard::tabItem(
            tabName = "help",
            
            shiny::h1("Info"),
            shiny::HTML("This app is designed to plot simulation results of clinical trials. It has been developed by Elias Laurin Meyer, Constantin Kumaus, Michal Majka and Franz König.
                     It has been published in <a href='https://www.softxjournal.com/article/S2352-7110(23)00043-2/fulltext'>SoftwareX</a>."),
            
            shiny::h2("User Manual"),
            shiny::HTML("Following you will find details on every part of the app and how they are to be used."),
            
            shiny::h3("Data Settings"),
            shiny::HTML("There are a few requirements to the data in order for the app to work. So far only .csv files can be uploaded. It is expected that the data is arranged in a way such that the input variables/design parameters precede the output variables/operating characteristics. Each row represents one simulation run with a different combination of input/design parameters. "),
            shiny::HTML("If your data is not aggregated yet i.e. if you have every single simulation outcome as one row in your dataset, and a 'replication run index variable' you can click the checkbox and choose which of your variables is the 'replication run index'. The dataset is then averaging over the OCs either by mean or median. Additionally the 'Distribution' tab opens where you can investigate the behaviour of your variables and outcomes."),
            
            
            shiny::h3("Data"),
            shiny::HTML("In the Data tab you find an overview of your data. Already here you can set filters for your input parameters, if you are not interested in some observations."),
            
            shiny::h3("Default values"),
            shiny::HTML("The default value tab is a key tab in this App. Please choose one default value for every input variable that can take on more than one unique value. Later in the plot tab the dataset is filtered for these values, unless the respective variable is chosen to be one of the dimensions in the graph (See 'plot' tab)."),
            shiny::h3("Distribution"),
            shiny::HTML("This tab only appears when the checkbox regarding 'replication run index/variables' is checked. You can create boxplots or distribution plots for your output variables to get an overview of the distribution behaviour."),
            
            shiny::h3("Plot"),
            shiny::HTML("After uploading the data and establishing the settings, you can visualize your simulation results on up to 4 dimensions. An x-axis variable as well as at least one OC have to be specified in order for the plot to show up: You can opt to add further design parameters on the 'facet' dimensions (row and column), which splits the plot into a grid as well as the 'shape' dimension, which adds lines/points in different shapes according to the value of the respective input parameter."),
            shiny::HTML("Furthermore you can change the style of your plot when clicking the 'style options' button and download a plot in the exact size and quality you need when clicking the 'Download plot' button"),
            
            shiny::h3("Scatterplot"),
            shiny::HTML("If you are interested in the variability of certain operating characteristics in 1 specific scenario, you can look at the settings in this tab which generates a scatterplot of 2 output variables, with the possibility of adding a grid. This is especially suitable if you ran e.g. 10000 simulation runs with the same setting and have not aggregated your data yet. Then you can choose your 'replication index variable' and investigate the variability of the outcome.")
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
    
    ## Upload Data Input ----
    # widget for user data upload
    upload <- shiny::reactive({
      
      shiny::validate(
        # if no file is uploaded yet "no file" appears everywhere upload() is called
        shiny::need(input$file, "No file")
      ) 
      
      # file = user uploaded file in tab Data Settings
      inFile <- input$file 
      
      df_candidate <- 
        try(
          utils::read.csv(
            inFile$datapath,
            header = TRUE,
            sep = input$sep,
            skip = input$rowSkip,
            stringsAsFactors = TRUE
          )
        )
      
      # Get rid of empty columns?
      if ("X" %in% colnames(df_candidate)) {
        df_candidate <- 
          df_candidate[, -which(colnames(df_candidate) == "X")]
      }
      
      # Return df_candidate
      df_candidate
      
    })
    
    
    ## Create Dataset ----
    # Dataset to be used (before possible aggregation)
    # Use example data if checkbox is checked, otherwise use uploaded dataset
    # Update Input choices
    data_full <- shiny::reactive({
      
      reacVals$first_row_filters_string <- "NULL"
      
      # Force users to go back to data tab after modifying the data
      shinydashboard::updateTabItems(
        session = session,
        inputId = "sidebarMenu",
        selected ="data"
      )
      
      # If example data is chosen, update other input options
      # as they are hidden automatically in GUI
      
      if(input$checkboxExampleData){
        
        # # read in Example data and convert some variables for correct display
        # exampleData <- utils::read.csv(
        #   "../data/ExampleData1.csv",
        #   header = TRUE,
        #   sep = ",",
        #   stringsAsFactors = TRUE
        # )
        
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
        
        return(exampleData)
        
        
      } else {
        # If not using default data, differentiate between behviour when 
        # FACTS data is used vs. custom uploaded data
        
        # Get column names
        col_names_upload <- colnames(upload())
        
        # FACTS Data
        if (input$checkboxFactsData == 1) {
          
          # Update inputs
          shiny::updateNumericInput(
            session = session,
            inputId = "rowSkip",
            value = 2
          )
          
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
            selected = "X.Sim"
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
        shiny::need(input$repvar != input$inputend, "Replication variable can't be input variable (Please alter last input variable or replication variable)")
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
          shiny::need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
        ) 
      })
      
    })
    
    
    ## Pre-DV data Tab ----------------------------------------------
    
    # display dataset as DT
    # Table in tab 'Pre-filter data'
    output$dataDT <- DT::renderDataTable({
      shiny::req(data_full())
      data_full()},
      filter = "top",
      options = list(
        lengthChange = FALSE, 
        autoWidth = TRUE,
        scrollX = TRUE
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
          "Last input variable cannot be last variable in data frame")
      )
      ind_inputendR() + 1
    })
    
    
    # Table with all chosen filters in 'Pre-filter data'
    # I.e. Data pre DV tab
    data_prefiltered <- shiny::reactive({
      shiny::req(ind_outputstartR())
      shiny::req(data_full())
      shiny::validate(
        shiny::need(input$repvar != input$inputend, "Replication variable can't be input variable (Please alter last input variable or replication variable)")
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
        shiny::need(!is.integer(input$repvar), 
                    "Replication variable can't be input variable (Please alter last input variable or replication run variable)")
      )
      
      shiny::validate(
        shiny::need(input$repvar != input$inputend, 
                    "Replication variable can't be input variable (Please alter last input variable or replication run variable)")
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
            "All defined output variables must be of class numeric. ", 
            xx
          )
          
          shiny::validate(
            shiny::need(all(output_class == "numeric"), text_err)
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
            shiny::h3("Aggregated Data"),
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
    names_outputsR_unaggregated <-  shiny::reactive({
      
      #shiny::req(data_filteredR, ind_inputendR)
      nm <- names(
        data_prefiltered() %>% 
          dplyr::select(-input$repvar)
      )
      nm <- nm[!(nm %in% names_inputsR())]
      nm
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
    
    ### defaults_df ----
    # table output in dropdown within plot tab, displaying chosen default values
    output$defaults_df_line <- shiny::renderTable({
      Values <- data.frame(
        "Variable" = names(defaults_input()),
        "Default value" = defaults_input(),
        check.names = FALSE
      ) # makes whitespace in header names possible
      Values
    })
    
    output$defaults_df_box <- shiny::renderTable({
      Values <- data.frame(
        "Variable" = names(defaults_input()),
        "Default value" = defaults_input(),
        check.names = FALSE
      ) # makes whitespace in header names possible
      Values
    })
    
    output$defaults_df_scatter <- shiny::renderTable({
      Values <- data.frame(
        "Variable" = names(defaults_input()),
        "Default value" = defaults_input(),
        check.names = FALSE
      ) # makes whitespace in header names possible
      Values
    })
    
    
    ### defaults_df_ui ----
    output$defaults_df_ui_box <- shiny::renderUI({
      shiny::tableOutput("defaults_df_box")
    })
    
    ### defaults_df_ui ----
    output$defaults_df_ui_line <- shiny::renderUI({
      shiny::tableOutput("defaults_df_line")
    })
    
    ### defaults_df_ui ----
    output$defaults_df_ui_scatter <- shiny::renderUI({
      shiny::tableOutput("defaults_df_scatter")
    })
    
    shiny::outputOptions(
      x = output, 
      name = "defaults_df_ui_box", 
      suspendWhenHidden = FALSE
    )
    
    shiny::outputOptions(
      x = output, 
      name = "defaults_df_ui_line", 
      suspendWhenHidden = FALSE
    )
    
    shiny::outputOptions(
      x = output,
      name = "defaults_df_ui_scatter", 
      suspendWhenHidden = FALSE
    )
    
    
    ## Color vector specification ------------------------------------
    
    # dynamic number of color selectors (one for every OC) 
    
    ### nOCR 
    # number of Operating Characteristics
    nOCR <- shiny::reactive({
      shiny::req(data_filteredR())
      ncol(data_filteredR()) - ind_inputendR()
    })
    
    ### colors_ui 
    # Create colorInput field for every OC
    output$colors_ui <- shiny::renderUI({
      lapply(1:nOCR(), function(i) {
        colourpicker::colourInput(
          inputId = paste0("col_", names_outputsR()[i]),
          label = names_outputsR()[i],
          showColour = "both",
          value = scales::hue_pal()(nOCR())[i]
        )
      })
    })
    
    ### valColvarR 
    valColvarR <- shiny::reactive({
      shiny::req(data_filteredR())
      gsub(" ", "", unique(data_filteredR()[[input$color]]))
    })
    
    ### nValColvarR 
    nValColvarR <- shiny::reactive({
      shiny::req(valColvarR())
      length(valColvarR())
    })
    
    ### colordim_ui 
    output$colordim_ui <- shiny::renderUI({
      lapply(1:nValColvarR(), function(i) {
        colourpicker::colourInput(
          inputId = paste0("col_", valColvarR()[i]),
          label = valColvarR()[i],
          showColour = "both",
          value = scales::hue_pal()(nValColvarR())[i]
        )
      })
    })
    
    ### valColvar_scatterR 
    valColvar_scatterR <- shiny::reactive({
      shiny::req(data_prefiltered())
      tryCatch({
        gsub(" ", "", unique(data_prefiltered()[[input$colvar_scatter]]))  
      }, 
      error = function(e) {
        err_ <- ""
        shiny::validate(
          shiny::need(
            err_ != "", 
            "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
          )
        )}
      )
    })
    
    ### nValColvar_scatterR 
    nValColvar_scatterR <- shiny::reactive({
      shiny::req(valColvar_scatterR())
      length(valColvar_scatterR())
    })
    
    
    ### colors_scatter_ui 
    output$colors_scatter_ui <- shiny::renderUI({
      lapply(1:nValColvar_scatterR(), function(i) {
        colourpicker::colourInput(
          inputId = paste0("col_", valColvar_scatterR()[i], "_sc"),
          label = valColvar_scatterR()[i],
          showColour = "both",
          value = scales::hue_pal()(nValColvar_scatterR())[i]
        )
      })
    })
    
    
    ### valColvar_boxplotR 
    valColvar_boxplotR <- shiny::reactive({
      shiny::req(data_prefiltered())
      tryCatch({
        gsub(" ", "", unique(data_prefiltered()[[input$colvar_dist]]))  
      }, 
      error = function(e) {
        err_ <- ""
        shiny::validate(
          shiny::need(
            err_ != "", 
            "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
          )
        )}
      )
    })
    
    ### nValColvar_boxplotR 
    nValColvar_boxplotR <- shiny::reactive({
      shiny::req(valColvar_boxplotR())
      length(valColvar_boxplotR())
    })
    
    
    ### colors_boxplot_ui 
    output$colors_boxplot_ui <- shiny::renderUI({
      lapply(1:nValColvar_boxplotR(), function(i) {
        colourpicker::colourInput(
          inputId = paste0("col_", valColvar_boxplotR()[i], "_bx"),
          label = valColvar_boxplotR()[i],
          showColour = "both",
          value = scales::hue_pal()(nValColvar_boxplotR())[i]
        )
      })
    })
    
    shiny::outputOptions(
      x = output, 
      name = "colors_ui", 
      suspendWhenHidden = FALSE
    )
    
    shiny::outputOptions(
      x = output, 
      name = "colordim_ui", 
      suspendWhenHidden = FALSE
    )
    
    shiny::outputOptions(
      x = output, 
      name = "colors_scatter_ui", 
      suspendWhenHidden = FALSE
    )
    
    shiny::outputOptions(
      x = output, 
      name = "colors_boxplot_ui", 
      suspendWhenHidden = FALSE
    )
    
    ### lUiColors 
    lUiColors <- shiny::reactive({
      shiny::req(input$OC)
      df_colors <- data.frame(lapply(input$OC, function(i) {
        input[[paste0("col_", i)]]
      }))
      vColors <- as.vector(t(df_colors))
      # names(vColors) <- names_outputsR()
      names(vColors) <- input$OC
      vColors
    })
    
    ### lUiColordim 
    lUiColordim <- shiny::reactive({
      df_colors <- data.frame(lapply(valColvarR(), function(i) {
        input[[paste0("col_", i)]]
      }))
      vColors <- as.vector(t(df_colors))
      names(vColors) <- unique(data_filteredR()[[input$color]])
      vColors
    })
    
    ### lUiColors_scatter 
    lUiColors_scatter <- shiny::reactive({
      shiny::req(valColvar_scatterR)
      df_colors <- data.frame(lapply(valColvar_scatterR(), function(i) {
        input[[paste0("col_", i, "_sc")]]
      }))
      vColors <- as.vector(t(df_colors))
      shiny::req(vColors)
      names(vColors) <- unique(data_prefiltered()[[input$colvar_scatter]])
      vColors
    })
    
    ### lUiColors_boxplot
    lUiColors_boxplot <- shiny::reactive({
      shiny::req(valColvar_boxplotR)
      df_colors <- data.frame(lapply(valColvar_boxplotR(), function(i) {
        input[[paste0("col_", i, "_bx")]]
      }))
      vColors <- as.vector(t(df_colors))
      shiny::req(vColors)
      names(vColors) <- unique(data_prefiltered()[[input$colvar_dist]])
      vColors
    })
    
    
    ## Plot Tab ----
    
    ### Errorbar Selection ----
    output$errorbar_var <- shiny::renderUI({
      
      if(input$radioErrorsymmetry == "symmetrical") {
        shiny::selectizeInput(
          inputId = "errorvars",
          label = "Choose all the error variables (sd)",
          choices = names_outputsR(),
          multiple = TRUE
        )
        
      } else{
        
        shiny::tagList(
          shiny::selectizeInput(
            inputId = "errorvars_upper",
            label = "Choose error variables for the upper KI",
            choices = names_outputsR(),
            multiple = TRUE
          ),
          
          shiny::selectizeInput(
            inputId = "errorvars_lower",
            label = "Choose error variables for the lower KI",
            choices = names_outputsR(),
            multiple = TRUE
          )
        )
      }
    })
    
    
    ## observes ----
    
    ### boxplotGroupVar ----
    # only make variables with default values available for simulation parameter choice
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "boxplotGroupVar",
        choices = names_inputsR()
      )
    })
    
    ### facet_distribution_rows ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_distribution_rows",
        choices = names_inputsR()
      )
    })
    
    ### facet_distribution_cols ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_distribution_cols",
        choices = names_inputsR()
      )
    })
    
    ### facet_distribution_wrap ----
    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "facet_distribution_wrap",
        choices = names_inputsR()
      )
    })
    
    ### x ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "x",
        choices = names(defaults_input())
      )
    })
    
    ### facet_rows ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_rows",
        choices = names(defaults_input())
      )
    })
    
    ### facet_cols ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_cols",
        choices = names(defaults_input())
      )
    })
    
    ### facet_wrap ----
    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "facet_wrap",
        choices = names(defaults_input())
      )
    })
    
    ### shape ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "shape",
        choices = names(defaults_input())
      )
    })
    
    ### linetype ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "linetype",
        choices = names(defaults_input())
      )
    })
    
    ### color ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "color",
        choices = names(defaults_input())
      )
    })
    
    ### checkboxColor ----
    shiny::observe({
      if(length(input$OC) != 1){
        shiny::updateCheckboxInput(
          session = session,
          inputId = "checkboxColor",
          value = FALSE
        )
      }
    })
    
    ### checkboxPalette_OC ----
    shiny::observe({
      if(input$checkboxColor == TRUE){
        shiny::updateCheckboxInput(
          session = session,
          inputId = "checkboxPalette_OC",
          value = FALSE)
      }
    })
    
    ### facet_rows_scatter ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_rows_scatter",
        choices = names(defaults_input()))
    })
    
    ### facet_cols_scatter ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_cols_scatter",
        choices = names(defaults_input()))
    })
    
    ### facet_wrap_scatter ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_wrap_scatter",
        choices = names(defaults_input()))
    })
    
    ### shape_scatter ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "shape_scatter",
        choices = names(defaults_input()))
    })
    
    ### color_scatter -----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "colvar_scatter",
        choices = names(defaults_input())
      )
    })
    
    ### color_boxplot -----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "colvar_dist",
        choices = names(defaults_input())
      )
    })
    
    ### xaxis_boxplot -----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "boxplotGroupVar",
        choices = names(defaults_input())
      )
    })
    
    ### facet_rows_boxplot ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_distribution_rows",
        choices = names(defaults_input()))
    })
    
    ### facet_cols_boxplot ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_distribution_cols",
        choices = names(defaults_input()))
    })
    
    ### facet_wrap_boxplot ----
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_distribution_wrap",
        choices = names(defaults_input()))
    })
    
    
    ## Plots -------------------------------------------------------------------
    
    ### Boxplot -------------------------------------------------------------
    
    df_boxplot <- shiny::reactive({
      
      # 1 line df with default values for variables that are checked
      
      default_df <- defaults_input()
      
      # vector of names of simulation parameters
      sim_par <- c(
        input$repvar, 
        input$boxplotGroupVar
      )
      
      if (input$checkboxColorDist) {
        sim_par <- c(
          sim_par, 
          input$colvar_dist
        )
      }
      
      if(input$radioFacetDistribution == "grid"){
        sim_par <- c(
          sim_par, 
          input$facet_distribution_rows, 
          input$facet_distribution_cols
        )
      }
      
      if(input$radioFacetDistribution == "wrap"){
        sim_par <- c(
          sim_par, 
          input$facet_distribution_wrap
        )
      }
      
      # exclude simulation parameters from df with default values
      default_filter <- default_df[!(names(default_df) %in% sim_par)]
      
      default_filter <- gsub(
        '\\[\\"', 
        "", 
        default_filter
      )
      
      default_filter <- gsub(
        '\\"\\]', 
        "", 
        default_filter
      )
      
      # create condition
      bedingung <- paste0(
        paste0(
          "`", 
          names(default_filter), 
          "`"
        ),
        " == ",
        paste0(
          "'", 
          default_filter, 
          "'"
        ),
        collapse = " & "
      )
      
      if(length(default_filter) != 0){
        df_boxplot <- subset(
          data_prefiltered(), 
          eval(parse(text = bedingung))
        )
      } else {
        df_boxplot <- data_prefiltered()
      }
      
      df_boxplot # return data frame
    })
    
    output$df_boxplot <- DT::renderDataTable({
      df_boxplot()
    },
    extensions = 'Buttons', 
    options = list(
      scrollX = TRUE, 
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    )
    )
    
    plot_boxplot <- shiny::reactive({
      
      shiny::validate(
        shiny::need(
          input$repvar != input$inputend, 
          "Replication variable can't be an input variable"
        )
      )
      
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$boxplotOutputVar), 
          "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
        )
      )
      
      shiny::req(input$boxplotGroupVar)
      
      d <- df_boxplot()
      
      for(i in names_inputsR()){
        d[,i] <- as.factor(d[,i])
      }
      
      shiny::req(lUiColors_boxplot())
      
      colScale_boxplot <-
        ggplot2::scale_colour_manual(values = lUiColors_boxplot())
      
      fillScale_boxplot <- 
        ggplot2::scale_fill_manual(values = lUiColors_boxplot())
      
      boxplot <- tryCatch({
        ggplot2::ggplot(
          d, 
          ggplot2::aes_string(
            x = input$boxplotGroupVar,
            y = input$boxplotOutputVar
          )
        )
      }, error = function(e) {
        err_ <- ""
        shiny::validate(
          shiny::need(err_ != "", "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values")
        )
      }
      )
      
      if (input$checkboxColorDist) {
        boxplot <-
          boxplot +
          ggplot2::aes_string(
            fill = input$colvar_dist,
            col = input$colvar_dist
          ) 
        
        if(input$checkboxPalette_boxplot){
          boxplot <-
            boxplot +
            colScale_boxplot + 
            fillScale_boxplot
        }
      }
      
      if (input$boxplottype == "Violinplot") {
        boxplot <- 
          boxplot + 
          ggplot2::geom_violin(alpha = input$alpha)
      } else if (input$boxplottype == "Boxplot") {
        boxplot <- 
          boxplot + 
          ggplot2::geom_boxplot(alpha = input$alpha)
      }
      
      if(input$radioFacetDistribution == "grid"){
        
        if (
          any(
            input$facet_distribution_rows %in% input$facet_distribution_cols
          )
        ) {
          err_ <- ""
          shiny::validate(
            shiny::need(
              err_ != "", 
              "Faceting variables can only appear in row or cols, not both"
            )
          )
        }
        
        boxplot <-
          tryCatch({
            
            frows_distribution <- 
              input$facet_distribution_rows %>%
              stringr::str_replace_all(",", "+") %>%
              rlang::parse_exprs()
            
            fcols_distribution <- 
              input$facet_distribution_cols %>%
              stringr::str_replace_all(",", "+") %>%
              rlang::parse_exprs()
            
            boxplot +
              ggplot2::facet_grid(
                ggplot2::vars(!!!frows_distribution),
                ggplot2::vars(!!!fcols_distribution),
                labeller = "label_both"
              )}, error = function(e) {
                err_ <- ""
                shiny::validate(
                  shiny::need(
                    err_ != "", 
                    "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
                  )
                )
                
              })
      }
      
      if(input$radioFacetDistribution == "wrap"){
        
        boxplot <-
          tryCatch({
            
            facets_distribution <- 
              input$facet_distribution_wrap %>%
              stringr::str_replace_all(",", "+") %>%
              rlang::parse_exprs()
            
            boxplot +
              ggplot2::facet_wrap(
                ggplot2::vars(!!!facets_distribution),
                labeller = "label_both"
              ) }, error = function(e) {
                err_ <- ""
                shiny::validate(
                  shiny::need(
                    err_ != "", 
                    "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
                  )
                )
              })
      }
      
      if (input$boxplot_flip) {
        boxplot <- 
          boxplot + 
          ggplot2::coord_flip()
      }
      
      plot_theme <- input$plottheme_boxplot
      plot_fontsize <- input$plotfontsize_boxplot
      plot_font <- input$plotfont_boxplot
      
      if (plot_theme == "Grey") {
        boxplot <- 
          boxplot + 
          ggplot2::theme_gray(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "White") {
        boxplot <- 
          boxplot + 
          ggplot2::theme_bw(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Linedraw") {
        boxplot <- 
          boxplot + 
          ggplot2::theme_linedraw(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Light") {
        boxplot <- 
          boxplot + 
          ggplot2::theme_light(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Minimal") {
        boxplot <- 
          boxplot + 
          ggplot2::theme_minimal(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Classic") {
        boxplot <- 
          boxplot + 
          ggplot2::theme_classic(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (input$checkboxTitle_boxplot){
        boxplot <- 
          boxplot +
          ggplot2::labs(
            title = input$plot_title_boxplot
          )  +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              colour = input$plot_title_colour_boxplot,
              size = input$plot_title_size_boxplot,
              vjust = 1.5,
              hjust = input$plot_title_place_boxplot
            )
          )
      }
      
      if(input$checkboxAxis_boxplot){
        boxplot <- 
          boxplot +
          ggplot2::labs(
            x = input$xLab_boxplot,
            y = input$yLab_boxplot
          )
      }
      
      boxplot
      
    })
    
    ### Render Plot ----
    shiny::observe({
      if(input$plottype_boxplot){
        output$pBoxplotly <- plotly::renderPlotly({
          plotly::ggplotly(plot_boxplot())
        })
      } else {
        output$pBoxplot <- shiny::renderPlot({
          plot_boxplot()
        })
      }
    })
    
    ### Render UI ----
    output$boxplot_ui <- shiny::renderUI({
      
      if(input$plottype_boxplot){
        plotly::plotlyOutput(
          "pBoxplotly",
          height = input$plotheight_boxplot,
          width = input$plotwidth_boxplot
        )
      } else {
        shiny::plotOutput(
          "pBoxplot",
          height = input$plotheight_boxplot,
          width = input$plotwidth_boxplot
        )
      }
      
    })
    
    ### Download Handler ----
    download_type_boxplot <- shiny::reactive({input$download_type_boxplot})
    
    output$download_plot_boxplot <- shiny::downloadHandler(
      
      filename = function() {
        paste0(
          input$download_name_boxplot,
          ".",
          input$download_type_boxplot
        )
      },
      
      content = function(file) {
        fun <- match.fun(download_type_boxplot())
        
        fun(
          file,
          height = input$download_plotheight_boxplot,
          width = input$download_plotwidth_boxplot,
          units = input$download_unit_boxplot,
          res = input$download_resolution_boxplot
        )
        
        print(plot_boxplot())
        dev.off()
      }
    )
    
    ## Scatterplot ----
    df_scatterplot <- shiny::reactive({
      
      # 1 line df with default values for variables that are checked
      
      default_df <- defaults_input()
      
      # vector of names of simulation parameters
      sim_par <- input$repvar
      
      if (input$checkboxColorScatter) {
        sim_par <- c(
          sim_par, 
          input$colvar_scatter
        )
      }
      
      if(input$radioFacet_scatter == "grid"){
        sim_par <- c(
          sim_par, 
          input$facet_rows_scatter, 
          input$facet_cols_scatter
        )
      }
      
      if(input$radioFacet_scatter == "wrap"){
        sim_par <- c(
          sim_par, 
          input$facet_wrap_scatter
        )
      }
      
      # exclude simulation parameters from df with default values
      default_filter <- default_df[!(names(default_df) %in% sim_par)]
      
      default_filter <- gsub(
        '\\[\\"', 
        "", 
        default_filter
      )
      
      default_filter <- gsub(
        '\\"\\]', 
        "", 
        default_filter
      )
      
      # create condition
      bedingung <- paste0(
        paste0(
          "`", 
          names(default_filter), 
          "`"
        ),
        " == ",
        paste0(
          "'", 
          default_filter, 
          "'"
        ),
        collapse = " & "
      )
      
      if(length(default_filter) != 0){
        df_scatterplot <- subset(
          data_prefiltered(), 
          eval(parse(text = bedingung))
        )
      } else {
        df_scatterplot <- data_prefiltered()
      }
      
      df_scatterplot # return data frame
      
    })
    
    output$df_scatterplot <- DT::renderDataTable({
      df_scatterplot()
    },
    extensions = 'Buttons', 
    options = list(
      scrollX = TRUE, 
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    )
    )
    
    plot_object_scatter <- shiny::reactive({
      
      shiny::req(lUiColors_scatter())
      
      colScale_scatter <- 
        ggplot2::scale_colour_manual(values = lUiColors_scatter())
      
      p1 <- tryCatch({
        p1 <- 
          ggplot2::ggplot(
            df_scatterplot(), 
            ggplot2::aes_string(
              x = input$OC_scatter[1], 
              y = input$OC_scatter[2]
            )
          ) +
          ggplot2::geom_point(
            size = input$pointsize_scatter,
            alpha = input$transparency_scatter
          )
      }, 
      error = function(e) {
        err_ <- ""
        shiny::validate(
          shiny::need(
            err_ != "", 
            "If a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
          )
        )
      }
      )
      
      if(input$checkboxColorScatter){
        
        p1 <-
          p1 +
          ggplot2::aes(
            color = factor(get(input$colvar_scatter))
          ) + 
          ggplot2::labs(
            colour = input$colvar_scatter
          )
      }
      
      if(input$checkboxPalette_scatter){
        p1 <- 
          p1 + 
          colScale_scatter
      }
      
      
      facets <- 
        input$facet_wrap_scatter %>%
        stringr::str_replace_all(",", "+") %>%
        rlang::parse_exprs()
      
      frows <- 
        input$facet_rows_scatter %>%
        stringr::str_replace_all(",", "+") %>%
        rlang::parse_exprs()
      
      fcols <- 
        input$facet_cols_scatter %>%
        stringr::str_replace_all(",", "+") %>%
        rlang::parse_exprs()
      
      
      if(input$radioFacet_scatter == "grid"){
        p1 <-
          p1 +
          ggplot2::facet_grid(
            ggplot2::vars(!!!frows),
            ggplot2::vars(!!!fcols),
            labeller = "label_both"
          )
      }
      
      if(input$radioFacet_scatter == "wrap"){
        
        p1 <-
          p1 +
          ggplot2::facet_wrap(
            ggplot2::vars(!!!facets), 
            labeller = "label_both"
          )
      }
      
      plot_theme <- input$plottheme_scatter
      plot_fontsize <- input$plotfontsize_scatter
      plot_font <- input$plotfont_scatter
      
      if (plot_theme == "Grey") {
        p1 <- 
          p1 + 
          ggplot2::theme_gray(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "White") {
        p1 <- 
          p1 + 
          ggplot2::theme_bw(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Linedraw") {
        p1 <- 
          p1 + 
          ggplot2::theme_linedraw(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Light") {
        p1 <- 
          p1 + 
          ggplot2::theme_light(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Minimal") {
        p1 <- 
          p1 + 
          ggplot2::theme_minimal(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Classic") {
        p1 <- 
          p1 + 
          ggplot2::theme_classic(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (input$checkboxTitle_scatter){
        p1 <- 
          p1 +
          ggplot2::labs(
            title = input$plot_title_scatter
          )  +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              colour = input$plot_title_colour_scatter,
              size = input$plot_title_size_scatter,
              vjust = 1.5,
              hjust = input$plot_title_place_scatter
            )
          )
      }
      
      if(input$checkboxAxis_scatter){
        p1 <- 
          p1 +
          ggplot2::labs(
            x = input$xLab_scatter,
            y = input$yLab_scatter
          )
      }
      
      p1
      
    })
    
    
    ### Render Plot ----
    shiny::observe({
      if(input$plottype_scatter){
        output$pScatterly <- plotly::renderPlotly({
          plotly::ggplotly(plot_object_scatter())
        })
      } else {
        output$pScatter <- shiny::renderPlot({
          plot_object_scatter()
        })
      }
    })
    
    
    ### Render UI ----
    output$scatter_ui <- shiny::renderUI({
      
      shiny::validate(
        shiny::need(input$OC_scatter, "No OCs chosen")
      )
      
      shiny::validate(
        shiny::need(any(input$chooseDT_search_columns != ""), "Please specify default values first")
      )
      
      if(input$plottype_scatter){
        plotly::plotlyOutput(
          "pScatterly",
          height = input$plotheight_scatter,
          width = input$plotwidth_scatter
        )
      } else {
        shiny::plotOutput(
          "pScatter",
          height = input$plotheight_scatter,
          width = input$plotwidth_scatter
        )
      }
      
    })
    
    ### Download Handler ----
    download_type_scatter <- shiny::reactive({input$download_type_scatter})
    
    output$download_plot_scatter <- shiny::downloadHandler(
      
      filename = function() {
        paste0(
          input$download_name_scatter,
          ".",
          input$download_type_scatter
        )
      },
      
      content = function(file) {
        fun <- match.fun(download_type_scatter())
        
        fun(
          file,
          height = input$download_plotheight_scatter,
          width = input$download_plotwidth_scatter,
          units = input$download_unit_scatter,
          res = input$download_resolution_scatter
        )
        
        print(plot_object_scatter())
        dev.off()
      }
    )
    
    
    ## Lineplot -------------
    
    # Data frame used for plot
    # Filters every variable for the specified default value except the chosen simulation parameters, which can have more distinguishable values
    
    df_plot <- shiny::reactive({
      
      # 1 line df with default values for variables that are checked
      default_df <- defaults_input()
      
      # vector of names of simulation parameters
      sim_par <- input$x
      
      # Code$sim_par <- paste(input$x, input$facet_rows, input$facet_cols, input$linetype, sep = ", ")
      Code$x <- input$x
      
      if(input$checkboxLinetype){
        sim_par <- c(
          sim_par, 
          input$linetype
        )
      }
      
      if(input$radioFacet == "grid"){
        sim_par <- c(
          sim_par, 
          input$facet_rows, 
          input$facet_cols
        )
      }
      
      if(input$radioFacet == "wrap"){
        sim_par <- c(
          sim_par, 
          input$facet_wrap
        )
      }
      
      if(input$checkboxColor){
        sim_par <- c(
          sim_par, 
          input$color
        )
      }
      
      if(input$checkboxColor){
        sim_par <- c(
          sim_par, 
          input$color
        )
      }
      
      
      # exclude simulation parameters from df with default values
      default_filter <- default_df[!(names(default_df) %in% sim_par)]
      
      default_filter <- gsub(
        '\\[\\"', 
        "", 
        default_filter
      )
      
      default_filter <- gsub(
        '\\"\\]', 
        "", 
        default_filter
      )
      
      bedingung <- paste0(
        paste0(
          "`", 
          names(default_filter), 
          "`"
        ),
        " == ",
        paste0(
          "'", 
          default_filter, 
          "'"
        ),
        collapse = " & "
      )
      
      if(length(default_filter) != 0) {
        df_plot_ <- tryCatch({ 
          subset(
            data_filteredR(), 
            eval(parse(text = bedingung))
          )},
          error = function(e) {
            err_ <- ""
            shiny::validate(
              shiny::need(
                err_ != "", 
                "This is not working. Probably because the underlying dataset has changed. 1) Go back to the data tab. 2) Re-define default values. If this does not fix it, please report a bug."
              )
            )
          })
      } else {
        df_plot_ <- data_filteredR()
      }
      
      df_plot_ # return data frame
    })
    
    
    output$df_plot <- DT::renderDataTable({
      shiny::validate(
        shiny::need(
          any(input$chooseDT_search_columns != ""), 
          "Please specify default values first"
        )
      )
      df_plot()
    },
    extensions = 'Buttons', 
    options = list(
      scrollX = TRUE, 
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    )
    )
    
    # Transform dataset to long format on chosen output variables for easy plotting
    data_longer <- shiny::reactive({
      shiny::req(input$OC)
      d <- df_plot()
      d <- tryCatch({ 
        d %>%
          tidyr::pivot_longer(
            cols = input$OC,
            names_to = "OC",
            values_to = "value"
          )
      }, 
      error = function(e) { 
        err_ <- ""
        shiny::validate(
          shiny::need(
            err_ != "", 
            "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
          )
        )
      }
      )
      
      if(input$checkboxErrorbar){
        
        if(input$radioErrorsymmetry == "symmetrical") {
          
          shiny::validate(
            shiny::need(
              shiny::isTruthy(input$errorvars), "Please define error variables")
          )
          
          d <- tryCatch({
            d %>%
              tidyr::pivot_longer(
                cols = input$errorvars,
                names_to = "errorvar_name",
                values_to = "error"
              )
          }, 
          error = function(e) {
            err_ <- ""
            shiny::validate(
              shiny::need(
                err_ != "", 
                "Select a different variable or if a new dataset has been uploaded, go first to the tab with the data and then re-define default values"
              )
            )   
          })
          
          bedingung_errorbar <- paste0(
            "(OC == '", 
            input$OC, 
            "' & errorvar_name == '", 
            input$errorvars, 
            "')", 
            collapse = " | "
          )
          
          d <- subset(
            d, 
            eval(parse(text = bedingung_errorbar))
          )
          
        } else {
          
          shiny::validate(
            shiny::need(
              shiny::isTruthy(input$errorvars_upper), 
              "Please define variable for upper bound/deviation"
            )
          )
          
          shiny::validate(
            shiny::need(
              shiny::isTruthy(input$errorvars_lower), 
              "Please define variable for upper bound/deviation"
            )
          )
          
          d <- tryCatch({
            d %>%
              tidyr::pivot_longer(
                cols = input$errorvars_upper,
                names_to = "errorvar_upper_name",
                values_to = "error_upper"
              )
          }, 
          error = function(e) {
            err_ <- ""
            shiny::validate(
              shiny::need(
                err_ != "", 
                "Select a different variable or if a new dataset has been uploaded, go to the tab with the data and then re-define default values"
              )
            )     
          })
          
          d <- tryCatch({
            d %>%
              tidyr::pivot_longer(
                cols = input$errorvars_lower,
                names_to = "errorvar_lower_name",
                values_to = "error_lower"
              )
          }, 
          error = function(e) {
            err_ <- ""
            shiny::validate(
              shiny::need(
                err_ != "", 
                "Select a different variable or if a new dataset has been uploaded, go to the tab with the data and then re-define default values"
              )
            )     
          })
          
          bedingung_errorbar <- paste0(
            "(OC == '", 
            input$OC, 
            "' & errorvar_upper_name == '", 
            input$errorvars_upper, 
            "' & errorvar_lower_name == '", 
            input$errorvars_lower, 
            "')", 
            collapse = " | "
          )
          
          d <- subset(
            d, 
            eval(parse(text = bedingung_errorbar))
          )
          
        }
      }
      # return data frame
      as.data.frame(d)
    })
    
    lineplot_object <- shiny::reactive({
      
      data_lp <- {
        data_output <- data_longer()
        if(input$checkboxLinetype) {
          data_output[[input$linetype]] <- 
            as.factor(data_output[[input$linetype]])
        }
        data_output
      }
      
      colScale <- {
        if(input$checkboxColor){
          ggplot2::scale_colour_manual(values = lUiColordim())
        } else {
          ggplot2::scale_colour_manual(values = lUiColors())
        }
      }
      
      p1 <- 
        ggplot2::ggplot(
          data_lp, 
          ggplot2::aes_string(
            x = input$x, 
            y = "value"
          )
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
            ggplot2::geom_line(
              ggplot2::aes(
                y = value,
                color = factor(get(input$color))
              ),
              size = input$linesize
            ) + 
            ggplot2::labs(colour = input$color) + 
            ggplot2::ylab(input$OC)
          
          Code$colour <<- paste(input$color)
          
        } else {
          
          p1 <- 
            p1 + 
            ggplot2::geom_line(
              ggplot2::aes(
                y = value,
                color  = OC
              ),
              size = input$linesize
            )
          Code$colour <<- "OC"
          
        }
      }
      
      if (input$checkboxPoint) {
        if (input$checkboxColor) {
          p1 <-
            p1  +
            ggplot2::geom_point(
              ggplot2::aes(
                y = value,
                color = factor(get(input$color))
              ),
              size = 3*input$linesize
            ) + 
            ggplot2::labs(
              colour = input$color
            ) + 
            ggplot2::ylab(input$OC)
          
        } else {
          
          p1 <-
            p1  +
            ggplot2::geom_point(
              ggplot2::aes(
                y = value,
                color  = OC
              ),
              size = 3*input$linesize
            )
          
        }
      }
      
      
      if(input$checkboxLinetype){
        
        p1 <-
          p1 +
          ggplot2::aes_string(
            linetype = input$linetype,
            shape = input$linetype
          ) + 
          ggplot2::labs(
            linetype = input$linetype, 
            shape = input$linetype
          )
        
      }
      
      facets <- 
        input$facet_wrap %>% 
        stringr::str_replace_all(",", "+") %>% 
        rlang::parse_exprs()
      
      frows <- 
        input$facet_rows %>%
        stringr::str_replace_all(",", "+") %>%
        rlang::parse_exprs()
      
      fcols <- 
        input$facet_cols %>%
        stringr::str_replace_all(",", "+") %>%
        rlang::parse_exprs()
      
      if(input$radioFacet == "grid"){
        p1 <- 
          p1 + 
          ggplot2::facet_grid(
            ggplot2::vars(!!!frows),
            ggplot2::vars(!!!fcols),
            labeller = "label_both"
          )
      }
      
      if(input$radioFacet == "wrap"){
        
        p1 <-
          p1 +
          ggplot2::facet_wrap(
            ggplot2::vars(!!!facets), 
            labeller = "label_both"
          )
      }
      
      if(input$checkboxErrorbar){
        
        if(input$radioErrorsymmetry == "symmetrical") {
          
          if (input$checkboxColor) {
            
            p1 <- 
              p1 + ggplot2::geom_errorbar(
                ggplot2::aes(
                  ymin = value - error, 
                  ymax = value + error, 
                  color = factor(get(input$color))
                ), 
                position = ggplot2::position_dodge(0.05)
              )
            
          } else {
            
            p1 <- 
              p1 + ggplot2::geom_errorbar(
                ggplot2::aes(
                  ymin = value - error, 
                  ymax = value + error, 
                  color = OC
                ), 
                position = ggplot2::position_dodge(0.05)
              )
            
          }
          
        } else {
          
          if(input$radioErrorstructure == "deviation") {
            
            if (input$checkboxColor) {
              
              p1 <- 
                p1 + ggplot2::geom_errorbar(
                  ggplot2::aes(
                    ymin = value - error_lower,
                    ymax = value + error_upper,
                    color = factor(get(input$color))
                  ),
                  position = ggplot2::position_dodge(0.05)
                )
              
            } else {
              
              p1 <- 
                p1 + ggplot2::geom_errorbar(
                  ggplot2::aes(
                    ymin = value - error_lower,
                    ymax = value + error_upper,
                    color = OC
                  ),
                  position = ggplot2::position_dodge(0.05)
                )
              
            }
            
          } else {
            
            if (input$checkboxColor) {
              
              p1 <- 
                p1 + ggplot2::geom_errorbar(
                  ggplot2::aes(
                    ymin = error_lower,
                    ymax = error_upper,
                    color = factor(get(input$color))
                  ),
                  position = ggplot2::position_dodge(0.05)
                )
              
            } else {
              
              p1 <- 
                p1 + ggplot2::geom_errorbar(
                  ggplot2::aes(
                    ymin = error_lower,
                    ymax = error_upper,
                    color = OC
                  ),
                  position = ggplot2::position_dodge(0.05)
                )
              
            }
            
          }
        }
      }
      
      # Return plot with modified theme
      p1 + 
        ggplot2::theme(legend.key.size = ggplot2::unit(4, 'cm'))
      
    })
    
    ### Plot Object ----
    
    # create plot_object with final settings
    plot_object <- shiny::reactive({
      
      p1 <- lineplot_object()
      plot_theme <- input$plottheme
      plot_fontsize <- input$plotfontsize
      plot_font <- input$plotfont
      
      if (plot_theme == "Grey") {
        p1 <- 
          p1 + 
          ggplot2::theme_gray(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "White") {
        p1 <- 
          p1 + 
          ggplot2::theme_bw(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Linedraw") {
        p1 <- 
          p1 + 
          ggplot2::theme_linedraw(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Light") {
        p1 <- 
          p1 + 
          ggplot2::theme_light(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Minimal") {
        p1 <- 
          p1 + 
          ggplot2::theme_minimal(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (plot_theme == "Classic") {
        p1 <- 
          p1 + 
          ggplot2::theme_classic(
            plot_fontsize, 
            plot_font
          )
      }
      
      if (input$checkboxTitle){
        p1 <- 
          p1 +
          ggplot2::labs(
            title = input$plot_title
          )  +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              colour = input$plot_title_colour,
              size = input$plot_title_size,
              vjust = 1.5,
              hjust = input$plot_title_place
            )
          )
      }
      
      if(input$checkboxAxis){
        p1 <- 
          p1 +
          ggplot2::labs(
            x = input$xLab,
            y = input$yLab
          )
      }
      
      p1
      
    })
    
    ### Render Plot ----
    shiny::observe({
      if(input$plottype){
        output$lineplotly <- plotly::renderPlotly({
          plotly::ggplotly(plot_object())
        })
      } else {
        output$lineplot <- shiny::renderPlot({
          plot_object()
        })
      }
    })
    
    ### Render UI ----
    output$lineplot_ui <- shiny::renderUI({
      
      shiny::validate(
        shiny::need(
          input$OC, 
          "No OCs chosen"
        )
      )
      
      shiny::validate(
        shiny::need(
          input$x, 
          "Please specify default values first"
        )
      )
      
      shiny::validate(
        shiny::need(
          any(input$chooseDT_search_columns != ""), 
          "Please specify default values first"
        )
      )
      
      if(input$plottype){
        plotly::plotlyOutput(
          "lineplotly",
          height = input$plotheight,
          width = input$plotwidth
        )
      } else {
        shiny::plotOutput(
          "lineplot",
          height = input$plotheight,
          width = input$plotwidth
        )
      }
      
    })
    
    
    # # Animation ----
    # 
    # ## observe ----
    # ### animateIteratorSelect ----
    # 
    # shiny::observe({
    #   shiny::updateSelectInput(session,
    #                     "animateIteratorSelect",
    #                     choices = names(defaults_input())
    #   )
    # })
    # 
    # 
    # ## observeEvents ----
    # 
    # shiny::observeEvent(input$animationCloseButton, {
    #   output$animationOutDynamic <- renderImage({ shiny::req("") })
    # })
    # 
    # 
    # #only renders if Button is clicked (isolate prevents reload on tabswitch)
    # shiny::observeEvent(input$animationRenderButton,{
    #   
    #   ### animationOutDynamic ---- 
    #   output$animationOutDynamic <- renderImage({
    #     
    #     tryCatch({
    #       #prevents rendering if button isnt clicked
    #       if(input$animationRenderButton == 0) return()
    #       
    #       #isolate prevents rerender on tabswitch
    #       isolate({
    #         #validates select input
    #         shiny::validate(shiny::need(
    #           input$animateIteratorSelect,
    #           "Please specify iteration variable first"))
    #         
    #         # temporary file, saves render
    #         outfileDyn <- tempfile(fileext='.gif')
    #         
    #         #Plot(with PlotTab customisation) + animation attributes
    #         ap <- plot_object() + 
    #           transition_states(states = !!(as.symbol(input$animateIteratorSelect)) ,
    #                             transition_length = 1,
    #                             state_length = 1,
    #                             wrap = TRUE
    #           ) +
    #           enter_fade()+
    #           exit_fade()
    #         
    #         # animation rendering
    #         anim_save("outfileDyn.gif", 
    #                   animate(ap,
    #                           nframes = input$frameAmount,
    #                           fps = input$renderFPS,
    #                           duration = input$durationAnimation,
    #                           height = input$plotheight,
    #                           width = input$plotwidth,
    #                   ))
    #         
    #         
    #         # Returns rendering in gif-form
    #         list(src = "outfileDyn.gif",
    #              contentType = 'image/gif'
    #         )
    #       })
    #       
    #     }, error = function(e) {
    #       err_ <- ""
    #       shiny::validate(shiny::need(err_ != "", "Animation cannot be created"))
    #     })
    #   },
    #   #Deletes temporary Files after execution
    #   deleteFile = TRUE)
    # })
    
    shiny::observe({
      shiny::updateNumericInput(
        session = session,
        inputId = "download_plotwidth",
        value = input$plotwidth
      )
    })
    
    shiny::observe({
      shiny::updateNumericInput(
        session = session,
        inputId = "download_plotheight",
        value = input$plotheight
      )
    })
    
    shiny::observe({
      shiny::updateNumericInput(
        session = session,
        inputId = "download_plotwidth_boxplot",
        value = input$plotwidth_boxplot
      )
    })
    
    shiny::observe({
      shiny::updateNumericInput(
        session = session,
        inputId = "download_plotheight_boxplot",
        value = input$plotheight_boxplot
      )
    })
    
    shiny::observe({
      shiny::updateNumericInput(
        session = session,
        inputId = "download_plotwidth_scatter",
        value = input$plotwidth_scatter
      )
    })
    
    shiny::observe({
      shiny::updateNumericInput(
        session = session,
        inputId = "download_plotheight_scatter",
        value = input$plotheight_scatter
      )
    })
    
    
    ### Download Handler ----
    download_type <- shiny::reactive({input$download_type})
    
    output$download_plot <- shiny::downloadHandler(
      
      filename = function() {
        paste0(
          input$download_name,
          ".",
          input$download_type
        )
      },
      
      content = function(file) {
        fun <- match.fun(download_type())
        
        fun(
          file, 
          height = input$download_plotheight, 
          width = input$download_plotwidth,
          units = input$download_unit,
          res = input$download_resolution
        )
        
        print(plot_object())
        dev.off()
      }
    )
    
    Code <- shiny::reactiveValues()
  }
  
  # Run Shiny App ----
  shiny::shinyApp(
    ui = ui, 
    server = server, 
    options = list(launch.browser = TRUE)
  )
  
}
