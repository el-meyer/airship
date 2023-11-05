
fnTogglePlotlyUI <- 
  function(
    cID
  ) {
    
    #### Interactive Plot ----
    shinyWidgets::switchInput(
      inputId = shiny::NS(cID, "bPlotly"),
      label = "Interactive Plot?",
      value = FALSE,
      size = "small"
    )
    
  }
  
