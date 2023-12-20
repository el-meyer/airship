
# Options specific to LDPlot Tab

fnLDplotUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "checkboxLine"),
        label = "Add lines?",
        value = TRUE
      ),
      
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "checkboxPoint"),
        label = "Add points?",
        value = TRUE
      ),
      
      #### Point Size ----
      shiny::sliderInput(
        inputId = shiny::NS(cID, "dPointSize"),
        label = "Line and point Size",
        min = 0,
        max = 10,
        value = 1,
        step = 0.1
      ),
      
      #### Linetype ----
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "checkboxLinetype"),
        label = "Add a linetype/shape dimension?"
      ),
      
      shiny::conditionalPanel(
        condition = "input.checkboxLinetype != 0",
        ns = shiny::NS(cID),
        
        shiny::selectInput(
          inputId = shiny::NS(cID, "linetype"),
          label = "Choose linetype/shape variable",
          choices = NULL
        )
      ),
      
      #### Errorbars ----
    #   shiny::checkboxInput(
    #     inputId = shiny::NS(cID, "checkboxErrorbar"),
    #     label = "Add errorbars?"
    #   ),
    #   
    #   shiny::conditionalPanel(
    #     condition = "input.checkboxErrorbar != 0",
    #     ns = shiny::NS(cID),
    #     
    #     shiny::radioButtons(
    #       inputId = shiny::NS(cID, "radioErrorsymmetry"),
    #       label = "Are the errors symmetrical (1 error variable) or asymmetrical (2 error variables)",
    #       choices = c("symmetrical", "asymmetrical")
    #     ),
    #     
    #     shiny::conditionalPanel(
    #       condition = "input.radioErrorsymmetry == 'asymmetrical'",
    #       ns = shiny::NS(cID),
    #       
    #       shiny::radioButtons(
    #         inputId = shiny::NS(cID, "radioErrorstructure"),
    #         label = "Do the variables represent the upper and lower deviation from the estimate or the upper and lower bounds?",
    #         choices = c("deviation", "bounds")
    #       )
    #     ),
    #     shiny::HTML("Select the corresponding error variable (Sd) for every OC chosen."),
    #     shiny::HTML("For a correct display, the error variables have to be in the same order as the OCs chosen above"),
    #     shiny::uiOutput(shiny::NS(cID, "errorbar_var"))
    #   )
    #   
    
    )
    
  }

fnLDplotColorUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      shiny::HTML("<b>By default, color is assigned to y-axis variables. Assigning to input will plot only first selected y-axis variable.</b>"),
      
      #### Color Dimension input ----
      shiny::checkboxInput(
        inputId = shiny::NS(cID, "checkboxColor"),
        label = "Add a color input dimension instead?"
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
        
      ),
      
      # Color on the y-axis ----
      shiny::conditionalPanel(
        condition = "input.checkboxColor == 0",
        ns = shiny::NS(cID),
        
        #### Color Button ----
        shiny::checkboxInput(
          inputId = shiny::NS(cID, "checkboxPaletteY"),
          label = "Specify your own colors?"
        ),
        
        shiny::absolutePanel(
          shinyWidgets::dropdownButton(
            label = "Color choices",
            status = "primary",
            circle = TRUE,
            right = TRUE,
            icon = shiny::icon("paintbrush"),
            shiny::uiOutput(shiny::NS(cID, "colorsUIY")),
            inputId = shiny::NS(cID, "dropdownColorsY")
          ),
          draggable = TRUE
        )
        
      ),
      
    )
    
  }

# fnLDPlotColorUpdateInput <- 
#   function(
#     cID,
#     cNamesInputs
#   ) {
#     shiny::moduleServer(
#       cID, 
#       function(input, output, session) 
#       {
#         ### Color Input -----
#         shiny::updateSelectInput(
#           session = session,
#           inputId = "colVar",
#           choices = cNamesInputs
#         )
#         
#       })
#     
#   }

# fnLDPlotErrorbarUpdateInput <- 
#   function(
#     cID,
#     cNamesOutputs
#   ) {
#     shiny::moduleServer(
#       cID, 
#       function(input, output, session) 
#       {
#         ### Errorbar Selection 
#         output$errorbar_var <- shiny::renderUI({
#           ns <- shiny::NS(cID)
#           
#           if(input$radioErrorsymmetry == "symmetrical") {
#             shiny::selectizeInput(
#               inputId = "errorvars"  %>% ns,
#               label = "Choose all the error variables (sd)",
#               choices = cNamesOutputs,
#               multiple = TRUE
#             )
#             
#           } else{
#             
#             shiny::tagList(
#               shiny::selectizeInput(
#                 inputId = "errorvars_upper"  %>% ns,
#                 label = "Choose error variables for the upper KI",
#                 choices = cNamesOutputs,
#                 multiple = TRUE
#               ),
#               
#               shiny::selectizeInput(
#                 inputId = "errorvars_lower"  %>% ns,
#                 label = "Choose error variables for the lower KI",
#                 choices = cNamesOutputs,
#                 multiple = TRUE
#               )
#             )
#           }
#         })
#         
#       })
#     
#   }
  

fnLDPlotUpdateInput <- 
  function(
    cID,
    cNamesInputs
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        shiny::updateSelectInput(
          session = session,
          inputId = "colVar",
          choices = cNamesInputs
        )
        
        shiny::updateSelectInput(
          session = session,
          inputId = "linetype",
          choices = cNamesInputs
        )

        
      })
    
  }

fnLDPlotColorCreateDropdown <- 
  function(
    cID,
    dfPrefilter,
    cNamesOutput,
    dfSummarized
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        # Y Axis Colors -----
        tryCatch({
          
          # number of Operating Characteristics
          nOCR <- length(cNamesOutput)
            
          # Create colorInput field for every OC
          output$colorsUIY <- shiny::renderUI({
            ns <- shiny::NS(cID)
            lapply(1:nOCR, function(i) {
              colourpicker::colourInput(
                inputId = paste0("col_", cNamesOutput[i]) %>% ns,
                label = cNamesOutput[i],
                showColour = "both",
                value = scales::hue_pal()(nOCR)[i]
              )
            })
          })
          
          # remember colors even when dropdown is deactivated
          shiny::outputOptions(
            x = output,
            name = "colorsUIY",
            suspendWhenHidden = FALSE
          )
          
        }, error = function(e) {
          err_ <- ""
          shiny::validate(
            shiny::need(
              err_ != "",
              "Please wait. If you have to wait too long, please report a bug in fnLDPlotColorCreateDropdown Y."
            )
          )
        }
        )
        
        # Colvar Colors -----
        tryCatch({
          
          ### valColvar
          valColvar <-
            gsub(
              " ",
              "",
              unique(dfSummarized[[input$colVar]])
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
              "Please wait. If you have to wait too long, please report a bug in fnLDPlotColorCreateDropdown Colvar."
            )
          )
        }
        )
        
      })
    
  }

fnLDplotServer <- 
  function(
    cID,
    lPlot
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        if (input$checkboxColor) {
          
          lPlot$lggPlot <-
            lPlot$lggPlot + 
            ggplot2::aes(
              y = !!rlang::sym(input$y[1])
            )
          
          # Update Code ----
          lPlot$lCode$ldplot <-
            paste0(
              " + ggplot2::aes(y = ",
              input$y[1],
              ")"
            )
          
        } else {
          
          lPlot$lggPlot <-
            lPlot$lggPlot + 
            ggplot2::aes(
              y = value
            )
          
          # Update Code ----
          lPlot$lCode$ldplot <-
            " + ggplot2::aes(y = value)"
          
        }
        
        if(input$checkboxLine){
          
          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::geom_line(
              size = input$dPointSize
            )
          
          lPlot$lCode$line <-
            paste0(
              " + ggplot2::geom_line(size = ",
              input$dPointSize,
              ")"
            )
          
        }
        
        if (input$checkboxPoint) {
          lPlot$lggPlot <-
            lPlot$lggPlot  +
            ggplot2::geom_point(
              size = 3*input$dPointSize
            )
          
          lPlot$lCode$point <-
            paste0(
              " + ggplot2::geom_point(size = ",
              3*input$dPointSize,
              ")"
            )
          
        }
        
        if(input$checkboxLinetype){

          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::aes(
              linetype = factor(!!rlang::sym(input$linetype)),
              shape = factor(!!rlang::sym(input$linetype))
            ) +
            ggplot2::labs(
              linetype = input$linetype,
              shape = input$linetype
            )
          
          lPlot$lCode$linetype <-
            paste0(
              " + ggplot2::aes(linetype = ",
              input$linetype,
              ", shape = ",
              input$linetype,
              ")"
            )
          
          lPlot$cSimPars <- 
            c(
              lPlot$cSimPars, 
              input$linetype
            )

        }
        # 
        # if(input$checkboxErrorbar){
        #   
        #   if(input$radioErrorsymmetry == "symmetrical") {
        #     
        #     if (input$checkboxColor) {
        #       
        #       p1 <- 
        #         p1 + ggplot2::geom_errorbar(
        #           ggplot2::aes(
        #             ymin = value - error, 
        #             ymax = value + error, 
        #             color = factor(get(input$color))
        #           ), 
        #           position = ggplot2::position_dodge(0.05)
        #         )
        #       
        #     } else {
        #       
        #       p1 <- 
        #         p1 + ggplot2::geom_errorbar(
        #           ggplot2::aes(
        #             ymin = value - error, 
        #             ymax = value + error, 
        #             color = OC
        #           ), 
        #           position = ggplot2::position_dodge(0.05)
        #         )
        #       
        #     }
        #     
        #   } else {
        #     
        #     if(input$radioErrorstructure == "deviation") {
        #       
        #       if (input$checkboxColor) {
        #         
        #         p1 <- 
        #           p1 + ggplot2::geom_errorbar(
        #             ggplot2::aes(
        #               ymin = value - error_lower,
        #               ymax = value + error_upper,
        #               color = factor(get(input$color))
        #             ),
        #             position = ggplot2::position_dodge(0.05)
        #           )
        #         
        #       } else {
        #         
        #         p1 <- 
        #           p1 + ggplot2::geom_errorbar(
        #             ggplot2::aes(
        #               ymin = value - error_lower,
        #               ymax = value + error_upper,
        #               color = OC
        #             ),
        #             position = ggplot2::position_dodge(0.05)
        #           )
        #         
        #       }
        #       
        #     } else {
        #       
        #       if (input$checkboxColor) {
        #         
        #         p1 <- 
        #           p1 + ggplot2::geom_errorbar(
        #             ggplot2::aes(
        #               ymin = error_lower,
        #               ymax = error_upper,
        #               color = factor(get(input$color))
        #             ),
        #             position = ggplot2::position_dodge(0.05)
        #           )
        #         
        #       } else {
        #         
        #         p1 <- 
        #           p1 + ggplot2::geom_errorbar(
        #             ggplot2::aes(
        #               ymin = error_lower,
        #               ymax = error_upper,
        #               color = OC
        #             ),
        #             position = ggplot2::position_dodge(0.05)
        #           )
        #         
        #       }
        #       
        #     }
        #   }
        # }
        
        ## Update Code ----
        # lPlot$lCode$ldplot <- 
        #   paste0(
        #     " + ggplot2::geom_point(alpha = ", 
        #     input$dAlpha,
        #     ", size = ",
        #     input$dPointSize,
        #     ")"
        #   )
        
        return(lPlot)
      }
      
    )
  }

fnLDPlotColorServer <- 
  function(
    cID,
    lPlot,
    dfPrefilter,
    dfSummarized
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        ### Get custom colors -----
        tryCatch({
          
          ### valColvar
          valColvar <-
            gsub(
              " ",
              "",
              unique(dfPrefilter[[input$colVar]])
            )
          
          ### lUiColorsY
          df_colorsY <- data.frame(lapply(input$y, function(i) {
            input[[paste0("col_", i)]]
          }))
          vColorsY <- as.vector(t(df_colorsY))
          names(vColorsY) <- input$y
          
          ### lUiColorsColvar
          df_colorsColvar <-
            data.frame(
              lapply(
                valColvar,
                function(i) {
                  input[[paste0("col", i)]]
                }
              )
            )
          
          vColorsColvar <- as.vector(t(df_colorsColvar))
          names(vColorsColvar) <- unique(dfPrefilter[[input$colVar]])
          
          if (input$checkboxColor) {
            vColors <- vColorsColvar
          } else {
            vColors <- vColorsY
          }
          
          colScale <-
            ggplot2::scale_colour_manual(values = vColors)
          
          fillScale <-
            ggplot2::scale_fill_manual(values = vColors)
          
        }, error = function(e) {
          err_ <- ""
          shiny::validate(
            shiny::need(
              err_ != "",
              "Please wait. If you have to wait too long, please report a bug in fnLDPlotColorServer getcolors."
            )
          )
        }
        )
        
        if (input$checkboxColor) {
          
          ### Update Plot -----
          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::aes(
              col = factor(!!rlang::sym(input$colVar))
            )  + 
            ggplot2::labs(
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
              " + ggplot2::aes(col = ", 
              input$colVar, 
              ")"
            )
          
          if(input$checkboxPalette){
            
            ### Update Plot -----
            lPlot$lggPlot <-
              lPlot$lggPlot +
              colScale
            
            ### Update Code ----
            lPlot$lCode$colorScale <-
              paste0(
                " + ggplot2::scale_colour_manual(values = c('",
                paste0(vColors, collapse = "','"),
                "'))"
              )
          }
          
        } else {
          
          ### Update Plot -----
          lPlot$lggPlot <-
            lPlot$lggPlot +
            ggplot2::aes(
              col = OC
            )
          
          ### Update Code ----
          lPlot$lCode$color <- 
            " + ggplot2::aes(col = OC)"
          
          if(input$checkboxPaletteY){
            
            ### Update Plot -----
            lPlot$lggPlot <-
              lPlot$lggPlot +
              colScale
            
            ### Update Code ----
            lPlot$lCode$colorScale <-
              paste0(
                " + ggplot2::scale_colour_manual(values = c('",
                paste0(vColors, collapse = "','"),
                "'))"
              )
          }
          
        }
        
        return(lPlot)
      }
      
    )
  }
