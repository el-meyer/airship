
fnStyleOptionsUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### Style options ----
      shiny::actionButton(
        inputId = shiny::NS(cID, "StyleOptions"), 
        label = "Style options"
      ),
      
      shinyBS::bsModal(
        id = shiny::NS(cID, "ModalStyleOptions"), 
        title = "Change style and size of plot", 
        trigger = shiny::NS(cID, "StyleOptions"), 
        size = "large",
        
        shiny::column(
          width = 4,
          
          shiny::sliderInput(
            inputId = shiny::NS(cID, "plotwidth"),
            label = "Plot width (px)",
            value = 1000,
            min = 600,
            max = 1500
          ),
          
          shiny::sliderInput(
            inputId = shiny::NS(cID, "plotheight"),
            label = "Plot height (px)",
            value = 600,
            min = 300,
            max = 1000
          ),
          
          shiny::hr(),
          
          shiny::checkboxInput(
            inputId = shiny::NS(cID, "checkboxTheme"),
            label = "Change the theme?"
          ),
          
          shiny::conditionalPanel(
            condition = "input.checkboxTheme != 0",
            ns = shiny::NS(cID),
            
            shiny::radioButtons(
              inputId = shiny::NS(cID, "plottheme"),
              label = "Select the theme",
              choices = c(
                "theme_gray", 
                "theme_bw", 
                "theme_linedraw",
                "theme_light",
                "theme_dark",
                "theme_minimal", 
                "theme_classic"
              )
            )
            
          )
        ),
        
        shiny::column(
          width = 4,
          
          shiny::numericInput(
            inputId = shiny::NS(cID, "plotfontsize"),
            label = "Font size",
            value = 11,
            min = 1,
            max = 30,
            step = 0.5
          ),
          
          shiny::selectInput(
            inputId = shiny::NS(cID, "plotfont"),
            label = "Font",
            choices = 
              c(
                "sans", 
                "serif", 
                "mono"
              )
          ),
          
          #### Add Titel ----
          shiny::checkboxInput(
            inputId = shiny::NS(cID, "checkboxTitle"),
            label = "Add title"
          ),
          
          shiny::conditionalPanel(
            condition = "input.checkboxTitle != 0",
            ns = NS(cID),
            
            shiny::textInput(
              inputId = shiny::NS(cID, "plotTitle"),
              label = "Enter the plot title"
            ),
            
            shiny::radioButtons(
              inputId = shiny::NS(cID, "plotTitlePlace"),
              label = "Title alignment",
              choices = c(
                "left" = 0, 
                "center" = 0.5, 
                "right" = 1
              )
            ),
            
            shiny::numericInput(
              inputId = shiny::NS(cID, "plotTitleSize"),
              label = "Size",
              value = 30,
              min = 1,
              max = 50,
              step = 1
            ),
            
            colourpicker::colourInput(
              inputId = shiny::NS(cID, "plotTitleColour"), 
              label = "Title colour:",
              showColour = "both",
              value = "black",
              allowTransparent = FALSE
            )
            
          ),
          
          shiny::hr(),
          
          #### Change Axis Labels ----
          shiny::checkboxInput(
            inputId = shiny::NS(cID, "checkboxAxis"),
            label = "Change axis labels"
          ),
          
          shiny::conditionalPanel(
            condition = "input.checkboxAxis != 0",
            ns = NS(cID),
            
            shiny::textInput(
              inputId = shiny::NS(cID, "xLab"),
              label = "X-axis label:"
            ),
            
            shiny::textInput(
              inputId = shiny::NS(cID, "yLab"),
              label = "Y-axis label:"
            )
            
          ),
          
        )
        
      )
      
    )
    
  }

fnStyleOptionsServer <-
  function(
    cID,
    lPlot
  ) {

    shiny::moduleServer(
      cID,
      function(input, output, session)
      {
        
        ### Update Plot -----
        
        #### Theme
        fnPlotTheme <- match.fun(utils::getFromNamespace(input$plottheme, "ggplot2"))
        plot_fontsize <- input$plotfontsize
        plot_font <- input$plotfont
        
        lPlot$lggPlot <-
          lPlot$lggPlot +
          fnPlotTheme(
            plot_fontsize, 
            plot_font
          )
        
        #### Title
        if (input$checkboxTitle) {
          lPlot$lggPlot <- 
            lPlot$lggPlot +
            ggplot2::labs(
              title = input$plotTitle
            )  +
            ggplot2::theme(
              plot.title = ggplot2::element_text(
                colour = input$plotTitleColour,
                size = input$plotTitleSize,
                vjust = 1.5,
                hjust = input$plotTitlePlace
              )
            )
        }
        
        #### Axes
        if (input$checkboxAxis) {
          lPlot$lggPlot <- 
            lPlot$lggPlot +
            ggplot2::labs(
              x = input$xLab,
              y = input$yLab
            )
        }
        
        ### Update Code ------
        
        #### Theme
        if (input$checkboxTheme) {
          lPlot$lCode$theme <- 
            paste0(
              " + ggplot2::",
              input$plottheme,
              "(",
              input$plotfontsize,
              ", '",
              input$plotfont,
              "')"
            )
        }
        
        #### Title
        if (input$checkboxTitle) {
          lPlot$lCode$title <-  
            paste0(
              " + ggplot2::labs(title = '",
              input$plotTitle,
              "')",
              
              " + ggplot2::theme(plot.title = ggplot2::element_text(colour = '",
              input$plotTitleColour,
              "', size = ",
              input$plotTitleSize,
              ", vjust = 1.5, hjust = ",
              input$plotTitlePlace,
              "))"
            )
        }
        
        #### Axes
        if (input$checkboxAxis) {
          lPlot$lCode$axes <- 
            paste0(
              " + ggplot2::labs(x = '",
              input$xLab,
              "', y = '",
              input$yLab,
              "')"
            )
        }
        
        return(lPlot)

      }

    )}
