
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

fnDisablePlotlyToggleServer <- 
  function(
    cID, 
    data,
    threshold = 1e5,
    global_session
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        observe({
          
          req(data()) 
          
          disable_switch <- nrow(data()) > threshold
          
          shinyWidgets::updateSwitchInput(
            session = global_session,
            inputId = shiny::NS(cID, "bPlotly"),
            disabled = disable_switch,
            value = FALSE
          )
          
          if (disable_switch) {
            shinyBS::addTooltip(
              session = global_session,
              id = shiny::NS(cID, "bPlotly"), 
              title = "Interactive mode is disabled due to the too big dataset size.",
              trigger = "hover"
            )    
          }
        })
      }
    )
  }


