# Helper for reading csv files. If data.table=TRUE, data.table::fread is used
fnReadCSV <- function(
    file, 
    header, 
    sep, 
    skip, 
    stringsAsFactors = TRUE, 
    use_data_table = FALSE,
    ...
) {
  
  if (use_data_table) {
    
    ret <- 
      data.table::fread(
        file = file,
        header = header,
        sep = sep,
        skip = skip,
        stringsAsFactors = stringsAsFactors,
        check.names = TRUE,
        data.table = FALSE, 
        ...
      )
    
  } else {
    
    ret <- 
      utils::read.csv(
        file,
        header = header, 
        sep = sep,
        skip = skip, 
        stringsAsFactors = stringsAsFactors,
        ...
      )  
    
  }
  
  return(ret)
  
}

fnReadFacts <- function(
    file,
    bUseFread,
    ...
) {
  
  headrows <- readLines(
    file,
    n = 5
  )
  
  deleterows <- length(
    grep(
      '^ *#', 
      headrows
    )
  ) - 1
  
  dfCandidate <-
    fnReadCSV(
      file,
      header = TRUE,
      sep = ",",
      skip = deleterows,
      stringsAsFactors = TRUE, 
      use_data_table = bUseFread,
      ...
    )
  
  colnames(dfCandidate) <- gsub(
    'X.', 
    '', 
    colnames(dfCandidate)
  )
  
  colnames(dfCandidate) <- gsub(
    '#', 
    '', 
    colnames(dfCandidate)
  )
  
  colnames(dfCandidate) <- gsub(
    ' ', 
    '.', 
    colnames(dfCandidate)
  )
  
  if ("Flags" %in% colnames(dfCandidate)) {
    dfCandidate <- subset(dfCandidate, select = -c(`Flags`))
  }
  
  if ("Random.Number.Seed" %in% colnames(dfCandidate)) {
    dfCandidate <- subset(dfCandidate, select = -c(`Random.Number.Seed`))
  }
  
  if (!'Sim' %in% colnames(dfCandidate)) {
    if ('Number' %in% colnames(dfCandidate)) {
      colnames(dfCandidate)[colnames(dfCandidate) == 'Number'] <- 'Sim'
    } else {
      stop('The dataset contains neither a `Sim` nor a `Number` column')
    }
  }
  
  return(dfCandidate)
  
}

fnPivotLongerTreatmentFacts <- function(
    dfData,
    nDoses
) {
  
  # dfPivotData <- dfData
  
  # Problem that e.g. param.10 interferes with logic and therefore thinks its a dose
  # Hence we need to limit the columns to exclude such input vectors
  
  # For now, use a "hack" which is to ask the user for the number of doses and 
  # remove rows from the final dataset that have a larger dose
  
  dfPivotData <-
    dfData %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("\\.[0-9]+"),
      names_to = "name",
      values_to = "value"
    ) %>% 
    dplyr::mutate(
      # Here we need to add that the first match per column name should correspond to the dose
      Dose = as.integer(gsub(
        ".*?\\.(\\d+).*",
        "\\1",
        name
      )),
      name = gsub(
        "(.*?\\.)\\d+(.*)",
        "\\1\\2",
        name
      )
    ) %>%
    dplyr::filter(Dose <= nDoses) %>%
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    ) 
  
  return(dfPivotData)
  
}
