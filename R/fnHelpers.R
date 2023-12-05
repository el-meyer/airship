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
    airship:::fnReadCSV(
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
