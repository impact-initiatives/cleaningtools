#' Checks for duplicated uuid
#'
#' @param .dataset a dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param .col_to_check string character with the name of the uuid column
#'
#' @return return a list with the dataset checked stored as checked_dataset and
#' a dataframe with the duplicate log
#' @export
#'
#' @examples
#'   testdata <- data.frame(
#' uuid = c(letters[1:4], "a", "b", "c"),
#' col_a = runif(7),
#' col_b = runif(7)
#' ) %>%
#'   dplyr::rename(`_uuid` = uuid)
#'
#' check_duplicate(testdata)
#'

check_duplicate <- function(.dataset, .col_to_check = "_uuid") {
  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (!(.col_to_check %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", .col_to_check, " in the names of the dataset")
    stop(msg)
  }

  duplicate_log <- .dataset[["checked_dataset"]] %>%
    dplyr::mutate(duplicate_check = duplicated(!!rlang::sym(.col_to_check))) %>%
    dplyr::filter(duplicate_check) %>%
    dplyr::select(all_of(.col_to_check)) %>%
    dplyr::mutate(
      old_value = !!rlang::sym(.col_to_check),
      question = .col_to_check,
      issue = "duplicated uuid"
    ) %>%
    dplyr::rename(uuid = !!rlang::sym(.col_to_check))

  .dataset[["duplicate_log"]] <- duplicate_log
  return(.dataset)
}
