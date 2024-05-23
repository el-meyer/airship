#' Example Data 2
#'
#' Simulated dataset from Meyer et al. (2022) https://doi.org/10.1002/pst.2194. 
#'
#' @source 
#' https://github.com/el-meyer/airship/blob/master/data/ExampleDataNASH.csv
#' 
#' @examples
#' \dontrun{
#' ExampleData2 <- read.csv("data/ExampleDataNash.csv")
#' for (i in c(3,6,7)) {ExampleData2[,i] <- as.factor(ExampleData2[,i])}
#' usethis::use_data(ExampleData2, overwrite = TRUE)
#' }
"ExampleData2"
