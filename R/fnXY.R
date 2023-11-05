
fnXYUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### x-axis ----
      shiny::selectInput(
        inputId = shiny::NS(cID, "x"),
        label = "Select x-axis",
        choices = NULL
      ),
      
      
      #### y-axis ----
      if (cID != "ldplot") {
      
        shiny::selectInput(
          inputId = shiny::NS(cID, "y"),
          label = "Select y-axis",
          choices = NULL
        )
        
      } else {
        
        shiny::selectizeInput(
          inputId = shiny::NS(cID, "y"), 
          label = "y-axis", 
          choices = NULL,
          multiple = TRUE
        )
        
      }
        
    )
    
  }

fnXYUpdateInput <- 
  function(
    cID,
    cNamesX,
    cNamesY
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        #### x_update ----
        shiny::updateSelectInput(
          session = session,
          inputId = "x",
          choices = cNamesX
        )
        
        #### y_update ----
        if (cID != "ldplot") {
          
          shiny::updateSelectInput(
            session = session,
            inputId = "y",
            choices = cNamesY
          )
          
        } else {
          
          shiny::updateSelectizeInput(
            session = session,
            inputId = "y",
            choices = cNamesY,
            selected = cNamesY[1]
          )
          
        }

      })
    
  }

fnXYServer <- 
  function(
    cID,
    lPlot
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        if (cID != "ldplot") {
          
          if (cID == "boxplot") {
            
            ### Update Plot -----
            lPlot$lggPlot <- 
              lPlot$lggPlot +  
              ggplot2::aes(
                x = factor(!!rlang::sym(input$x)), 
                y = !!rlang::sym(input$y)
              )
            
          } else {
            
            ### Update Plot -----
            lPlot$lggPlot <- 
              lPlot$lggPlot +  
              ggplot2::aes(
                x = !!rlang::sym(input$x), 
                y = !!rlang::sym(input$y)
              )
            
          }
          
          ### Update Code -----
          lPlot$lCode$xy <- 
            paste0(
              " + ggplot2::aes(x = ", 
              input$x ,
              ", y = ", 
              input$y, 
              ")"
            )
          
        } else {
          
          ### Update Plot -----
          lPlot$lggPlot <- 
            lPlot$lggPlot +  
            ggplot2::aes_string(
              x = input$x
            )
          
          ### Update Code -----
          lPlot$lCode$xy <- 
            paste0(
              " + ggplot2::aes(x = ", 
              input$x ,
              ")"
            )
          
        }
        
        ### Update Filters -----
        lPlot$cSimPars <- 
          c(
            lPlot$cSimPars, 
            input$x
          )
        
        return(lPlot)
        
      })
    
  }

