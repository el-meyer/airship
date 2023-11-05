
# Options specific to Scatterplot Tab

fnScatterplotUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### Point Size ----
      shiny::sliderInput(
        inputId = shiny::NS(cID, "dPointSize"),
        label = "Point Size",
        min = 0,
        max = 10,
        value = 1,
        step = 0.1
      ),
      
      #### Coord Flip ----
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "bCoordFlip"),
        label = "Flip coordinates?",
        value = FALSE,
      ),
      
      #### Transparency -----
      shiny::sliderInput(
        inputId = shiny::NS(cID, "dAlpha"),
        label = "Select transparency",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.1
      ),
      
    )
    
  }

fnScatterplotServer <- 
  function(
    cID,
    lPlot
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        lPlot$lggPlot <-
          lPlot$lggPlot +
          ggplot2::geom_point(
            alpha = input$dAlpha,
            size = input$dPointSize
          )
        
        lPlot$lCode$scatterplot <- 
          paste0(
            " + ggplot2::geom_point(alpha = ", 
            input$dAlpha,
            ", size = ",
            input$dPointSize,
            ")"
          )
        
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
