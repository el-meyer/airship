# Helper for reading csv files. If data.table=TRUE, data.table::fread is used
read_csv_ <- function(file, 
                      header, 
                      sep, 
                      skip, 
                      stringsAsFactors = TRUE, 
                      use_data_table = FALSE,
                      ...) {
  
  if (use_data_table) {
    
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
