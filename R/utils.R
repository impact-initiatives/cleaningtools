#' detects variables names in code
#'
#' @param codeintext a string vector to look for variables
#'
#' @return a vector with strings
#' @export
#'
#' @examples
#' string_to_check <- c(
#'   "ki_age> 1",
#'   "noo==\"mar\"",
#'   "sum(var1, var2, na.rm = T)",
#'   "oks != \"mar2\"",
#'   "oknospace!=\"mar2\""
#' )
#' detect_variable(string_to_check)
detect_variable <- function(codeintext) {
  var_vector <- codeintext %>%
    stringr::str_replace_all(pattern = "[:symbol:]", replace = " ") %>% # removes anything | ` =  + ^ ~ < > $
    stringr::str_replace_all(
      pattern = stop_words_replace %>% stringi::stri_c(collapse = "|"),
      replace = " "
    ) %>% # removes anything ! & %

    stringi::stri_split(regex = " ", omit_empty = T) %>%
    do.call(c, .) %>%
    stringr::str_subset(pattern = "\"", negate = T) %>% # removes anything that starts with a quote
    stringr::str_subset(pattern = "^[:digit:]+$", negate = T) %>% # removes anything that is only number
    stringr::str_subset(pattern = "[:alnum:]+(?=\\()", negate = T) %>% # removes functions, i.e. text(
    stringr::str_replace_all(pattern = "\\(|\\)", " ") %>% # removes parenthesis
    stringr::str_replace_all(pattern = "\\(|\\)", " ") %>%
    stringr::str_subset(
      pattern = stop_words_functions %>% stringi::stri_c(collapse = "|"),
      negate = T
    ) %>% # removes stop words from functions
    stringr::str_trim() %>%
    unique()

  var_vector[var_vector != ""]
}
stop_words_replace <- c("!", "&", "%")
stop_words_functions <- c("na.rm", "TRUE", "FALSE", "T", "F")
