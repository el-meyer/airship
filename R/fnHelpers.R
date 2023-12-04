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
    
    as.data.frame(
      data.table::fread(
        file = file,
        header = header,
        sep = sep,
        skip = skip,
        stringsAsFactors = stringsAsFactors,
        data.table = FALSE, 
        ...
      )
    )
    
  } else {
    
    utils::read.csv(
      file,
      header = header, 
      sep = sep,
      skip = skip, 
      stringsAsFactors = stringsAsFactors,
      ...
    )  
  }
}
