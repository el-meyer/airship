
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
      
    }
    
    return(dfFinal) # return data frame
    
      }
    )
  }
