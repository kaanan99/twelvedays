#' Takes a noun and makes it plural
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#' @import english
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location){
  verb <- stringr::str_replace_na(verb, " ")
  location <- stringr::str_replace_na(location, " ")
  adjective <- stringr::str_replace_na(adjective, " ")
  num_word <- english::as.english(num)
  if(num == 1){
    if(stringr::str_detect(item, "^[aeiou]")){
      num_word <- "and an"
    }
    else{
      num_word <- "and a"
    }
  }
  phrase <- glue::glue("{num_word} {adjective} {item} {verb} {location}")
  phrase <- stringr::str_replace_all(phrase, " +", " ")
  return(phrase)

}

