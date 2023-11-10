
fnDownloadUI <- 
  function(
    cID
  ) {
    
    shiny::tagList(
      
      #### Download Plot Button ----
      shiny::actionButton(
        inputId = shiny::NS(cID, "SavePlot"), 
        label = "Download"
      ),
      
      #### Download Plot Window ----
      shinyBS::bsModal(
        id = shiny::NS(cID, "DownloadModal"), 
        title = "Download plot and data", 
        trigger = shiny::NS(cID, "SavePlot"), 
        size = "medium",
        
        shiny::selectInput(
          inputId = shiny::NS(cID, "DownloadType"), 
          label = "Choose plot file type",
          choices = c(
            "png", 
            "jpeg", 
            "tiff"
          )
        ),
        
        shiny::selectInput(
          inputId = shiny::NS(cID, "DownloadUnit"), 
          label = "Choose plot unit",
          choices = c(
            "px", 
            "in", 
            "cm", 
            "mm"
          )
        ),
        
        shiny::numericInput(
          inputId = shiny::NS(cID, "DownloadPlotwidth"),
          label = "Plot width",
          value = 1000,
          min = 1,
          max = 2000
        ),
        
        shiny::numericInput(
          inputId = shiny::NS(cID, "DownloadPlotheight"),
          label = "Plot height",
          value = 600,
          min = 1,
          max = 2000
        ),
        
        shiny::numericInput(
          inputId = shiny::NS(cID, "DownloadResolution"),
          label = "Plot resolution",
          value = 72,
          min = 1, 
          max = 1000
        ),
        
        shiny::textInput(
          inputId = shiny::NS(cID, "DownloadName"), 
          label = "Specify plot file name"
        ),
        
        shiny::downloadButton(
          outputId = shiny::NS(cID, "DownloadPlot"), 
          label = "Download Plot"
        ),
        
        shiny::downloadButton(
          outputId = shiny::NS(cID, "DownloadData"), 
          label = "Download Data"
        )
        
      )
      
    )
    
  }

fnDownloadUpdateInput <-
  function(
    cID
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        shiny::updateNumericInput(
          session = session,
          inputId = "DownloadPlotwidth",
          value = input$plotwidth
        )
        
        shiny::updateNumericInput(
          session = session,
          inputId = "DownloadPlotheight",
          value = input$plotheight
        )
        
      }
    )
  }

fnDownloadServer <- 
  function(
    cID,
    lPlot
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        
        ### Download Handler ----
        fnDownloadType <- shiny::reactive({input$DownloadType})
        
        output$DownloadPlot <- shiny::downloadHandler(
          
          filename = function() {
            paste0(
              input$DownloadName,
              ".",
              input$DownloadType
            )
          },
          
          content = function(file) {
            fun <- match.fun(fnDownloadType())
            
            fun(
              file,
              height = input$DownloadPlotheight,
              width = input$DownloadPlotwidth,
              units = input$DownloadUnit,
              res = input$DownloadResolution
            )
            
            print(lPlot()$lggPlot)
            dev.off()
          }
        )
        
        output$DownloadData <- shiny::downloadHandler(
          
          filename = function() {
            "data.csv"
          },
          
          content = function(file) {
            write.csv(
              lPlot()$lData,
              file
            )
          }
        )
        
      }
      
    )}
