
# Set Global Constants ####
.onAttach <- function(libname, pkgname) {
  locales <<- c("de_DE", "en_US", "fi_FI", "ru_RU")
  textTypes <<- c("blogs", "news", "twitter")

  # Set Hooks ####
  courseraSkCapstone::readPackageSettings()
}
