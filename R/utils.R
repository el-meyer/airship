.onAttach <- function(libname, pkgname) {
  
  v <- getNamespaceVersion("airship")
  rule <- paste0(rep("-", getOption("width")), collapse = "")
  
  packageStartupMessage(rule)
  packageStartupMessage(paste0("airship ", v, " loaded."))
  packageStartupMessage("Visit for more information: ")
  packageStartupMessage("https://el-meyer.github.io/airship/")
  packageStartupMessage(rule)
  
}


.onDetach <- function(libpath) {
  
  rule <- paste0(rep("-", getOption("width")), collapse = "")
  packageStartupMessage(rule)
  packageStartupMessage("Thank you for using the airship package!")
  packageStartupMessage("Don't forget to report bugs / request features under:")
  packageStartupMessage("https://github.com/el-meyer/airship/issues")
  packageStartupMessage(rule)
}

utils::globalVariables(c("inputs", "outputs", "data_filtered"))

fnSatisfyCMDCheck <- function() {
  Cairo::Cairo
}
