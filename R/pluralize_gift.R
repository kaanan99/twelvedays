#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift){
  is_special <- (stringr::str_detect(gift, "oo") | stringr::str_detect(gift, "y$"))
  is_special[1] <- TRUE

  gift[is_special] <- stringr::str_replace(gift[is_special], "oo", "ee")
  gift[is_special] <- stringr::str_replace(gift[is_special], "y$", "ies")
  gift[!is_special] <- stringr::str_replace(gift[!is_special], "$", "s")
  return(gift)
}

pluralize_gift(xmas$Gift.Item)
