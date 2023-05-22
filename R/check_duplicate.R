#' Checks for duplicated uuid
#'
#' @param .dataset a dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param uuid_col_name string character for the uuid, default is "_uuid"
#' @param .col_to_check string character with the name of the columns to check. If NULL (default),
#' it will check for the uuid_col_name
#' @param log_name name of the log of flagged value
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
#' testdata2 <- data.frame(
#'               uuid = letters[c(1:7)],
#'               village = paste("village", c(1:3,1:3,4)),
#'               ki_identifier = paste0("xx_", c(1:5,3,4))) %>%
#'               dplyr::rename(`_uuid` = uuid)
#'
#' check_duplicate(testdata2, .col_to_check = "village")
#'
#' check_duplicate(testdata2, .col_to_check = c("village", "ki_identifier"), uuid = "_uuid")
#'

check_duplicate <- function(.dataset,
                            uuid_col_name = "_uuid",
                            .col_to_check = NULL,
                            log_name = "duplicate_log"
                            ) {
  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (!(uuid_col_name %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", uuid_col_name, " in the names of the dataset")
    stop(msg)
  }

  if (is.null(.col_to_check)) {
    .col_to_check <- uuid_col_name
    check_only_uuid <- TRUE
  } else {
    check_only_uuid <- FALSE
  }

  if (!all(.col_to_check %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", {glue::glue_collapse(.col_to_check, sep = ' ~/~ ')}, " in the names of the dataset")
    stop(msg)
  }

  duplicate_log <- .dataset[["checked_dataset"]] %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    tidyr::unite(unique_combination, .col_to_check, remove = F) %>%
    dplyr::mutate(duplicate_check = duplicated(unique_combination)) %>%
    dplyr::filter(duplicate_check) %>%
    dplyr::select(dplyr::all_of(c(uuid_col_name, .col_to_check)))

  if (check_only_uuid) {
    duplicate_log <- duplicate_log %>%
      dplyr::mutate(
        old_value = !!rlang::sym(.col_to_check),
        question = .col_to_check,
        issue = glue::glue("duplicated ", .col_to_check)
      ) %>%
      dplyr::rename(uuid = !!rlang::sym(uuid_col_name))
  } else {
    duplicate_log <- duplicate_log %>%
      tidyr::pivot_longer(-!!rlang::sym(uuid_col_name),
                          names_to = "question",
                          values_to = "old_value") %>%
      dplyr::mutate(
        issue = glue::glue("duplicated ", {glue::glue_collapse(.col_to_check, sep = ' ~/~ ')})
      ) %>%
      dplyr::rename(uuid = !!rlang::sym(uuid_col_name))
  }

  .dataset[[log_name]] <- duplicate_log
  return(.dataset)
}
