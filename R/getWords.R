#' Get vector of words from character vectors
#'
#' This function splits words up on spaces and deletes punctuation
#' separating words (contractions still contain apostrophes).
#'
#' @details These words are filtered so that
#' \itemize {
#'   \item there are no empty ones.
#'   \item
#' }
#'
#' @param x a character vector containing text
#'
#' @return a character vector containing words
#' @export
#'
#' @examples
#' getWords('foo bar baz.')
#' [1] "foo" "bar" "baz"
getWords <- function(x) {
  words <- unlist(strsplit(x, split = ' '))
  wordsNoPunct <- gsub("[[:punct:]]+$", "", words)
  #! Add more filters in later besides empty words?
  wordsFiltered <- wordsNoPunct[wordsNoPunct != ""]
  return(wordsFiltered)
}
