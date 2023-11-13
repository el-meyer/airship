
fnStandardOutputUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      shiny::fluidRow(
        shiny::column(
          width = 12,
          #### Boxplot Output ----
          shiny::uiOutput(shiny::NS(cID, "PlotOutput"))
        )
      ),
      
      shiny::hr(),
      
      #### Code Output ----
      shiny::h2("R code to create plot"),
      shiny::hr(),
      
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::textOutput(
            outputId = shiny::NS(cID, "Code")
          )
        ),
      ),
      
      ### Plotted Data ----
      shiny::h2("Plotted Data"),
      shiny::hr(),

      shiny::fluidRow(
        DT::dataTableOutput(
          outputId = shiny::NS(cID, "PlotData")
        )
      )
      
    )
    
  }


fnStandardOutputServer <- 
  function(
    cID,
    lPlot,
    bEnablePlotly = TRUE
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        tryCatch({
        
        # Should Plotly features be enabled
        if (bEnablePlotly) {
          
          #### Render Plot ----
          if (input$bPlotly) {
            output$Plot <- plotly::renderPlotly({
              plotly::ggplotly(lPlot()$lggPlot)
            })
          } else {
            output$Plot <- shiny::renderPlot({
              lPlot()$lggPlot
            })
          }
          
          
          #### Render UI ----
          output$PlotOutput <- shiny::renderUI({
            if (input$bPlotly) {
              plotly::plotlyOutput(
                shiny::NS(cID, "Plot"),
                height = input$plotheight,
                width = input$plotwidth
              )
            } else {
              shiny::plotOutput(
                shiny::NS(cID, "Plot"),
                height = input$plotheight,
                width = input$plotwidth
              )
            }
          })
          
        } else {
          
          #### Render Plot ----
          output$Plot <- shiny::renderPlot({
            lPlot()$lggPlot
          })
          
          
          #### Render UI ----
          output$PlotOutput <- shiny::renderUI({
            shiny::plotOutput(
              shiny::NS(cID, "Plot"),
              height = input$plotheight,
              width = input$plotwidth
            )
          })
          
        }
        
        
        #### Render Code -----
        output$Code <- shiny::renderText({
          paste(lPlot()$lCode, collapse = "")
        })
        
        #### Render Dataset ----
        output$PlotData <- DT::renderDT(
          {
            lPlot()$lData
          },
        options = list(
          scrollX = TRUE,
          dom = 'Bfrtip'
          )
        )
        
        }, error = function(e) {
          err_ <- ""
          shiny::validate(
            shiny::need(
              err_ != "",
              "Please wait. If you have to wait too long, please report a bug in fnStandardOutput."
            )
          )
        }
        )
        
      }
      
    )}
