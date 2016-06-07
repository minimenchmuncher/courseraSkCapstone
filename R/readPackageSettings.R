
#' Read Package Settings
#'
#' @param x a yaml file containing the package settings
#' @param environ a string specifying what environment to use, either "Production" or "Testing"
#'
#' @return a list of the settings to use.
#' @export
#'
readPackageSettings <- function(x = 'settings.yaml', environ = "Production") {
  All_Settings <- yaml::yaml.load_file(x)
  settings <<- All_Settings$Environment[[environ]]
  return(TRUE)
}
