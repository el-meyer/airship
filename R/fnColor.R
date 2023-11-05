
fnColorUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### Color Dimension ----
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "checkboxColor"),
        label = "Add a color dimension?"
      ),
      
      shiny::conditionalPanel(
        condition = "input.checkboxColor != 0",
        ns = shiny::NS(cID),
        
        shiny::selectInput(
          inputId = shiny::NS(cID, "colVar"),
          label = "Choose color variable",
          choices = NULL
        ),
        
        #### Color Button ----
        shiny::checkboxInput(
          inputId = shiny::NS(cID, "checkboxPalette"),
          label = "Specify your own colors?"
        ),
        
        shiny::absolutePanel(
          shinyWidgets::dropdownButton(
            label = "Color choices",
            status = "primary",
            circle = TRUE,
            right = TRUE,
            icon = shiny::icon("paintbrush"),
            shiny::uiOutput(shiny::NS(cID, "colorsUI")),
            inputId = shiny::NS(cID, "dropdownColors")
          ),
          draggable = TRUE
        )
        
      )
      
    )
    
  }

fnColorUpdateInput <- 
  function(
    cID,
    cNamesInputs
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        ### Color Input -----
        shiny::updateSelectInput(
          session = session,
          inputId = "colVar",
          choices = cNamesInputs
        )
        
      })
    
  }

fnColorCreateDropdown <- 
  function(
    cID,
    dfPrefilter
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        tryCatch({
          
          ### valColvar
          valColvar <-
            gsub(
              " ",
              "",
              unique(dfPrefilter[[input$colVar]])
            )
          
          ### nValColvar
          nValColvar <- length(valColvar)
          
          # make sure at least one color is selected
          if (nValColvar == 0) {
            
            ### colorsUI
            output$colorsUI <- shiny::renderUI({
              ns <- shiny::NS(cID)
              lapply(1:1, function(i) {
                colourpicker::colourInput(
                  inputId = paste0("col", 1) %>% ns,
                  label = "",
                  showColour = "both",
                  value = scales::hue_pal()(1)[1]
                )
              })
            })
            
          } else {
            
            ### colorsUI
            output$colorsUI <- shiny::renderUI({
              ns <- shiny::NS(cID)
              lapply(1:nValColvar, function(i) {
                colourpicker::colourInput(
                  inputId = paste0("col", valColvar[i]) %>% ns,
                  label = valColvar[i],
                  showColour = "both",
                  value = scales::hue_pal()(nValColvar)[i]
                )
              })
            })
            
          }
          
          # remember colors even when dropdown is deactivated
          shiny::outputOptions(
            x = output,
            name = "colorsUI",
            suspendWhenHidden = FALSE
          )
          
        }, error = function(e) {
          err_ <- ""
          shiny::validate(
            shiny::need(
              err_ != "",
              "Please wait. If you have to wait too long, please report a bug."
            )
          )
        }
        )
        
      })
    
  }

fnColorServer <- 
  function(
    cID,
    lPlot,
    dfPrefilter
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        if (input$checkboxColor) {
          
          ### Update Plot -----
          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::aes(
              fill = factor(!!rlang::sym(input$colVar)),
              col = factor(!!rlang::sym(input$colVar))
            ) + 
            ggplot2::labs(
              fill = input$colVar,
              col = input$colVar
            )
          
          ### Update Filters -----
          lPlot$cSimPars <- 
            c(
              lPlot$cSimPars, 
              input$colVar
            )
          
          ### Update Code ----
          lPlot$lCode$color <- 
            paste0(
              " + ggplot2::aes(fill = ", 
              input$colVar ,
              ", col = ", 
              input$colVar, 
              ")"
            )
          
          if(input$checkboxPalette){
            
            ### Get custom colors -----
            tryCatch({
              
              ### valColvar
              valColvar <-
                gsub(
                  " ",
                  "",
                  unique(dfPrefilter[[input$colVar]])
                )
              
              ### lUiColors
              df_colors <-
                data.frame(
                  lapply(
                    valColvar,
                    function(i) {
                      input[[paste0("col", i)]]
                    }
                  )
                )
              
              vColors <- as.vector(t(df_colors))
              names(vColors) <- unique(dfPrefilter[[input$colVar]])
              
              colScale <-
                ggplot2::scale_colour_manual(values = vColors)
              
              fillScale <-
                ggplot2::scale_fill_manual(values = vColors)
              
            }, error = function(e) {
              err_ <- ""
              shiny::validate(
                shiny::need(
                  err_ != "",
                  "Please wait. If you have to wait too long, please report a bug."
                )
              )
            }
            )

            ### Update Plot -----
            lPlot$lggPlot <-
              lPlot$lggPlot +
              colScale +
              fillScale

            ### Update Code ----
            lPlot$lCode$colorScale <-
              paste0(
                " + ggplot2::scale_colour_manual(values = c('",
                paste0(vColors, collapse = "','"),
                "')) + ggplot2::scale_fill_manual(values = c('",
                paste0(vColors, collapse = "','"),
                "'))"
              )
          }
          
        }
        
        return(lPlot)
        
      })
    
  }

