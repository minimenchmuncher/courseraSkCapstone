#' Read Swiftkey Lines from File given Path
#'
#' This reads lines from a file of swiftkey data, given its file path.
#' It can do so using one (default) or more blocks, which are concatenated.
#' @param x path to file or open connection
#' @param blocksize number of lines to read per block
#' @param blocks total number of blocks
#'
#' @return character vector of lines of swiftkey data
#' @export
#'
#' @examples
#' ## For simple run:
#' readSwiftKeyLines('/path/to/file', blocksize = 100, blocks = 1)
#' ## This is equivalent to:
#' readSwiftKeyLines('/path/to/file', blocksize = 25, blocks = 4)
readSwiftKeyLinesPath <- function(x, blocksize = 100, blocks = 1) {
  if (is.character(x)) {
    con <- file(x, "r")
  } else {
    con <- x
  }
  skText <- sapply(1:blocks, FUN = function(y) {
    readLines(con = con, n = blocksize)
  })
  skTextC <- c(skText)  # skText yields matrix, convert to a vector.
  close(con)
  return(skTextC)
}


#' Read Swiftkey Lines from File
#'
#' Reads lines from a file of swiftkey data, given the provided package settings.
#'
#' @param nrows number of rows to read. Set to -1 for all lines.
#' @param locale which locale to use, can be any of those present. Defaults to "en_US"
#' @param textType which file type to use, can be any of those present. Defaults to "twitter"
#'
#' @return a character vector containing the lines of text
#' @export
#'
readSwiftKeyLines <- function(nrows = 100, locale = "en_US", textType = "twitter") {
  if (!locale %in% locales) {
    stop(sprintf("the locale %s is not available", locale))
  }
  if (!textType %in% textTypes) {
    stop(sprintf("the type %s is not available", textType))
  }

  origFPath <- file.path(settings$skFiles, locale, paste(locale, textType, "txt", sep = "."))
  if (!file.exists(origFPath)) {
    stop("couldn't find the file path provided. See Docs.")
  }
  skText <- c(readLines(origFPath, warn = FALSE, n = nrows))
  return(skText)
}



#' Get a random sampling of swiftkey data
#'
#' Gets a random set of rows given a seed. That seed defaults to today's date
#' but can be set to be any date. Those data are then cached according to
#' the global settings. If the same seed and settings are run multiple times,
#' this program reads from cached file instead of regenerating those
#' samples as those files are quite large.
#'
#' @inheritParams readSwiftKeyLines
#' @param seedDate seed to use, defaults to today's date.
#'
#' @return a character vector of randomly chosen text
#' @export
getSwiftKeyRandomSample <- function(nrows = 100, locale = "en_US", textType = "twitter", seedDate = lubridate::today()) {
  if (!locale %in% locales) {
    stop(sprintf("the locale %s is not available", locale))
  }
  if (!textType %in% textTypes) {
    stop(sprintf("the type %s is not available", textType))
  }
  # establish path to sample cache. If is present, use that file.
  cachePath <- file.path(settings$cacheFiles, paste0(locale, "_", textType, "_", seedDate, "_n", nrows, ".txt"))
  if (file.exists(cachePath)) {
    sampLines <- readLines(cachePath)
  } else {

    # create it, save it to cache, and return the text
    origFPath <- file.path(settings$skFiles, locale, paste(locale, textType, "txt", sep = "."))
    if (!file.exists(origFPath)) {
      stop("couldn't find the file path provided. See Docs.")
    }

    set.seed(seedDate)
    allSkText <- c(readLines(origFPath))
    if (length(allSkText) < nrows) {
      stop("The number of rows selected is greater than the file size. Choose a smaller number.")
    }
    sampLines <- sample(allSkText, size = nrows, replace = FALSE)
    writeLines(sampLines, con = cachePath)
  }

  return(sampLines)
}
