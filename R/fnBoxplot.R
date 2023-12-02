
# Options specific to Boxplot Tab

fnBoxplotUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### Plottype ----
      shiny::HTML("<b>Choose plot type</b>"),
      
      shiny::radioButtons(
        inputId = shiny::NS(cID, "cType"),
        label = "Boxplot or Violinplot",
        choices = c("Boxplot", "Violinplot"),
        selected = "Boxplot",
      ),
      
      #### Coord Flip ----
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "bCoordFlip"),
        label = "Flip coordinates?",
        value = FALSE,
      ),
      
      shiny::sliderInput(
        inputId = shiny::NS(cID, "dAlpha"),
        label = "Select transparency",
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.1
      ),
        
      )
    
  }

fnBoxplotOutlierUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      shiny::conditionalPanel(
        condition = "input.cType == 'Boxplot'",
        ns = shiny::NS("boxplot"),
        
        checkboxInput(NS(cID, "showoutliers"), 
                      label = "Show outliers?",
                      value = TRUE)
      ) 
      
    )
  }

fnBoxplotServer <- 
  function(
    cID,
    lPlot
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        if (input$cType == "Violinplot") {
          
          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::geom_violin(alpha = input$dAlpha)
          
          lPlot$lCode$boxplot <- 
            paste0(
              " + ggplot2::geom_violin(alpha = ", 
              input$dAlpha,
              ")"
            )
          
        } else if (input$cType == "Boxplot") {
          
          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::geom_boxplot(alpha = input$dAlpha,
                                  outlier.shape = if (input$showoutliers) NULL else ""
                                  )
          
          lPlot$lCode$boxplot <- 
            paste0(
              " + ggplot2::geom_boxplot(alpha = ", input$dAlpha,
              ifelse(!input$showoutliers, ", outlier.shape = ''", ""),
              ")"
            )
          
        }
        
        if (input$bCoordFlip) {
          
          lPlot$lggPlot <- 
            lPlot$lggPlot + 
            ggplot2::coord_flip()
          
          lPlot$lCode$coordflip <- 
            paste0(
              " + ggplot2::coord_flip()"
            )
          
        }
        
        return(lPlot)
      }
      
    )}


