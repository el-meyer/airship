
fnFacetGridUI <- 
  function(
    cID
  ) {

    shiny::tagList(
      
      #### Facet Dimension ----
      shiny::radioButtons(
        inputId = shiny::NS(cID, "radioFacet"),
        label = "Add a facet dimension?",
        choices = c("no", "grid", "wrap")
      ),
      
      ##### Grid ----
      shiny::conditionalPanel(
        condition = "input.radioFacet == 'grid'",
        ns = shiny::NS(cID),
        
        shiny::selectInput(
          inputId = shiny::NS(cID, "facetRows"), 
          label = "Add row variable", 
          choices = NULL,
          multiple = TRUE
        ),
        
        shiny::selectInput(
          inputId = shiny::NS(cID, "facetCols"), 
          label = "Add column variable", 
          choices = NULL,
          multiple = TRUE
        )
        
      ),
      
      ##### Wrap ----
      shiny::conditionalPanel(
        condition = "input.radioFacet == 'wrap'",
        ns = shiny::NS(cID),
        
        shiny::selectizeInput(
          inputId = shiny::NS(cID, "facetWrap"), 
          label = "Choose variables to facet wrap",
          choices = NULL,
          multiple = TRUE
        )
      )
      
    )
    
  }

fnFacetGridUpdateInput <- 
  function(
    cID,
    cNamesInputs
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
        #### facet_rows_update ----
          shiny::updateSelectInput(
            session = session,
            inputId = "facetRows",
            choices = cNamesInputs
          )
        
        #### facet_cols_update ----
          shiny::updateSelectInput(
            session = session,
            inputId = "facetCols",
            choices = cNamesInputs
          )
        
        #### facet_wrap_update ----
          shiny::updateSelectInput(
            session = session,
            inputId = "facetWrap",
            choices = cNamesInputs
          )
      })
    
  }

fnFacetGridServer <- 
  function(
    cID,
    lPlot
  ) {
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {

    ## Facet Grid ----    
        
    if(input$radioFacet == "grid") {

      if (
        any(
          input$facetRows %in% input$facetCols
        )
      ) {
        err_ <- ""
        shiny::validate(
          shiny::need(
            err_ != "",
            "Faceting variables can only appear in rows or columns, not both."
          )
        )
      }

      ### Update Plot -----
      
      lPlot$lggPlot <-
        tryCatch({

          cRows <-
            input$facetRows %>%
            stringr::str_replace_all(",", "+") %>%
            rlang::parse_exprs()

          cCols <-
            input$facetCols %>%
            stringr::str_replace_all(",", "+") %>%
            rlang::parse_exprs()

          lPlot$lggPlot +
            ggplot2::facet_grid(
              ggplot2::vars(!!!cRows),
              ggplot2::vars(!!!cCols),
              labeller = "label_both"
            )}, error = function(e) {
              err_ <- ""
              shiny::validate(
                shiny::need(
                  err_ != "",
                  "Something went wrong. If changing some settings does not solve the problem, re-visit the data tab, re-upload the data and wait for computations and then re-define focus variables. If this error persists, contact the package maintainer."
                )
              )

            })
      
      
      ### Update Filters -----
      lPlot$cSimPars <- 
        c(
          lPlot$cSimPars, 
          input$facetRows,
          input$facetCols
        )
      
      ### Update Code ----
      lPlot$lCode$facet <- 
        paste0(
          " + ggplot2::facet_grid(rows = ggplot2::vars(", 
          paste0(input$facetRows, collapse = ","),
          "), cols = ggplot2::vars(", 
          paste0(input$facetCols, collapse = ","), 
          "))"
        )
      
    }
        
    ## Facet Wrap -----

    if(input$radioFacet == "wrap") {

      ### Update Plot -----
      
      lPlot$lggPlot <-
        tryCatch({

          cWrap <-
            input$facetWrap %>%
            stringr::str_replace_all(",", "+") %>%
            rlang::parse_exprs()

          lPlot$lggPlot +
            ggplot2::facet_wrap(
              ggplot2::vars(!!!cWrap),
              labeller = "label_both"
            ) }, error = function(e) {
              err_ <- ""
              shiny::validate(
                shiny::need(
                  err_ != "",
                  "Something went wrong. If changing some settings does not solve the problem, e-visit the data tab, re-upload the data and wait for computations and then re-define focus variables. If this error persists, contact the package maintainer."
                )
              )
            })
      
      ### Update Filters -----
      lPlot$cSimPars <- 
        c(
          lPlot$cSimPars, 
          input$facetWrap
        )
      
      ### Update Code ------
      lPlot$lCode$facet <- 
        paste0(
          " + ggplot2::facet_wrap(facets = ggplot2::vars(", 
          paste0(input$facetWrap, collapse = ","),
          "))"
        )
      
    }

    return(lPlot)
    
    })
    
  }

