#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, line, phrase_col){
  day_word <- dataset$Day.in.Words[line]
  #dataset modifications
  dataset <- dplyr::filter(dataset, dataset$Day <= line)
  dataset <- dplyr::mutate(dataset, Gift.Item = pluralize_gift(dataset$Gift.Item))
  dataset <- dplyr::arrange(dataset, dplyr::desc(dataset$Day))
  dataset[[phrase_col]] <- purrr::pmap(dataset,  ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
  #printing
  phrases <- dplyr::pull(dataset, {{phrase_col}})
  #special case for day 1
  if(line == 1){
    phrases [[1]] <- stringr::str_remove(phrases[[1]], "and ")
  }
  #Rest of printing
  cat("On the ")
  cat(day_word)
  cat( " day of Christmas, my true love sent to me,\n")
  list <- purrr::map(phrases, ~cat(.x, sep = "\n"))
  cat("\n")
  return(invisible(""))

}
#sing_day(xmas, 1, "Full.Phrase")
