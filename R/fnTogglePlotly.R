
fnTogglePlotlyUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### Interactive Plot ----
      
      shiny::h5("Interactivity is currently turned off and in testing."),
      
      shinyWidgets::switchInput(
        inputId = shiny::NS(cID, "bPlotly"),
        label = "Interactive Plot?",
        value = FALSE,
        size = "small"
      )
      
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
          
          # Needs to be changed to activate Plotly
          # disable_switch <- nrow(data()) > threshold
          disable_switch <- TRUE
          
          shinyWidgets::updateSwitchInput(
            session = global_session,
            inputId = shiny::NS(cID, "bPlotly"),
            disabled = disable_switch,
            value = FALSE
          )
          
          # if (disable_switch) {
          #   shinyBS::addTooltip(
          #     session = global_session,
          #     id = shiny::NS(cID, "bPlotly"), 
          #     title = "Interactive mode is disabled due to the too big dataset size.",
          #     trigger = "hover"
          #   )    
          # }
        })
      }
    )
  }


