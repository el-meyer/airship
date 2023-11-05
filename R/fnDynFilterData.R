
fnDynFilterData <- 
  function(
  cID,
  dfFilter,
  dfData,
  cSimPars,
  cInputNames
  ) {
    
    shiny::moduleServer(
      cID, 
      function(input, output, session) 
      {
    
    # exclude simulation parameters from df with default values
    default_filter <- dfFilter[!(names(dfFilter) %in% cSimPars)]
    
    default_filter <- gsub(
      '\\[\\"', 
      "", 
      default_filter
    )
    
    default_filter <- gsub(
      '\\"\\]', 
      "", 
      default_filter
    )
    
    # create condition
    bedingung <- paste0(
      paste0(
        "`", 
        names(default_filter), 
        "`"
      ),
      " == ",
      paste0(
        "'", 
        default_filter, 
        "'"
      ),
      collapse = " & "
    )
    
    if(length(default_filter) != 0){
      dfFinal <- subset(
        dfData, 
        eval(parse(text = bedingung))
      )
    } else {
      dfFinal <- dfData
    }
    
    if (cID == "ldplot") {
      
      if (!input$checkboxColor) {
        
        dfFinal <- 
          dfFinal %>%
          tidyr::pivot_longer(
            cols = input$y,
            names_to = "OC",
            values_to = "value"
          )
        
      }
      
      # if(input$checkboxErrorbar){
      # 
      #   if(input$radioErrorsymmetry == "symmetrical") {
      # 
      #     shiny::validate(
      #       shiny::need(
      #         shiny::isTruthy(input$errorvars),
      #         "Please define error variables."
      #       )
      #     )
      # 
      #     d <- tryCatch({
      #       d %>%
      #         tidyr::pivot_longer(
      #           cols = input$errorvars,
      #           names_to = "errorvar_name",
      #           values_to = "error"
      #         )
      #     },
      #     error = function(e) {
      #       err_ <- ""
      #       shiny::validate(
      #         shiny::need(
      #           err_ != "",
      #           "Something went wrong. If changing some settings does not solve the problem, re-visit the data tab, re-upload the data and wait for computations and then re-define focus variables. If this error persists, contact the package maintainer."
      #           )
      #       )
      #     })
      # 
      #     bedingung_errorbar <- paste0(
      #       "(OC == '",
      #       input$OC,
      #       "' & errorvar_name == '",
      #       input$errorvars,
      #       "')",
      #       collapse = " | "
      #     )
      # 
      #     d <- subset(
      #       d,
      #       eval(parse(text = bedingung_errorbar))
      #     )
      # 
      #   } else {
      # 
      #     shiny::validate(
      #       shiny::need(
      #         shiny::isTruthy(input$errorvars_upper),
      #         "Please define variable for upper bound/deviation."
      #       )
      #     )
      # 
      #     shiny::validate(
      #       shiny::need(
      #         shiny::isTruthy(input$errorvars_lower),
      #         "Please define variable for upper bound/deviation"
      #       )
      #     )
      # 
      #     d <- tryCatch({
      #       d %>%
      #         tidyr::pivot_longer(
      #           cols = input$errorvars_upper,
      #           names_to = "errorvar_upper_name",
      #           values_to = "error_upper"
      #         )
      #     },
      #     error = function(e) {
      #       err_ <- ""
      #       shiny::validate(
      #         shiny::need(
      #           err_ != "",
      #           "Something went wrong. If changing some settings does not solve the problem, re-visit the data tab, re-upload the data and wait for computations and then re-define focus variables. If this error persists, contact the package maintainer."
      #           )
      #       )
      #     })
      # 
      #     d <- tryCatch({
      #       d %>%
      #         tidyr::pivot_longer(
      #           cols = input$errorvars_lower,
      #           names_to = "errorvar_lower_name",
      #           values_to = "error_lower"
      #         )
      #     },
      #     error = function(e) {
      #       err_ <- ""
      #       shiny::validate(
      #         shiny::need(
      #           err_ != "",
      #           "Something went wrong. If changing some settings does not solve the problem, re-visit the data tab, re-upload the data and wait for computations and then re-define focus variables. If this error persists, contact the package maintainer."
      #           )
      #       )
      #     })
      # 
      #     bedingung_errorbar <- paste0(
      #       "(OC == '",
      #       input$OC,
      #       "' & errorvar_upper_name == '",
      #       input$errorvars_upper,
      #       "' & errorvar_lower_name == '",
      #       input$errorvars_lower,
      #       "')",
      #       collapse = " | "
      #     )
      # 
      #     d <- subset(
      #       d,
      #       eval(parse(text = bedingung_errorbar))
      #     )
      # 
      #   }
      # }
      
    }
    
    return(dfFinal) # return data frame
    
      }
    )
  }
