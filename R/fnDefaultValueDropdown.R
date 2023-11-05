
fnDefaultValueDropdownUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### DV overview ----
      shinyWidgets::dropdown(
        label = "Default value overview",
        shiny::HTML("Current default value settings"),
        shiny::uiOutput(shiny::NS(cID, "DvDropdown"))
      )
      
    )
    
  }


fnDefaultValueDropdownServer <- 
  function(
    cID,
    dfDefaultValues
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        ### Render DV Dropdown ----
        output$DvDropdown <- shiny::renderTable({
          Values <- data.frame(
            "Variable" = names(dfDefaultValues()),
            "Default value" = dfDefaultValues(),
            check.names = FALSE
          ) # makes whitespace in header names possible
          Values
        })
        
      }
      
    )}
