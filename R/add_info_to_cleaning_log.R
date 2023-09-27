#' Add information to the cleaning log
#'
#' @param list_of_log A list file containing the cleaning_log and the dataset
#' @param dataset If a list is provided, the element name represents the dataset; otherwise,
#' it should be a dataframe.
#' @param cleaning_log If a list is provided, the element name represents the cleaning log;
#' otherwise, it is should be a dataframe.
#' @param dataset_uuid_column uuid column in the cleaning dataset, it can take a vector of several
#' binding key. Default is uuid.
#' @param cleaning_log_uuid_column uuid column in the cleaning log, it can take a vectore of several
#' binding key. Default is uuid.
#' @param information_to_add Vector of name of the columns to be added.
#'
#' @return A list with cleaning log with added info and the dataset.
#' @export
#'
#' @examples
#' cleaningtools::cleaningtools_raw_data |>
#'   check_pii(uuid_column = "X_uuid") |>
#'   check_duplicate(uuid_column = "X_uuid") |>
#'   check_value(uuid_column = "X_uuid") |>
#'   create_combined_log() |>
#'   add_info_to_cleaning_log(dataset_uuid_column = "X_uuid")
add_info_to_cleaning_log <- function(list_of_log = NULL,
                                     dataset = "checked_dataset",
                                     cleaning_log = "cleaning_log",
                                     dataset_uuid_column = "uuid",
                                     cleaning_log_uuid_column = "uuid",
                                     information_to_add = c("enumerator_num", "date_assessment")) {
  if (is.data.frame(list_of_log) & !is.null(list_of_log)) {
    stop("The variable `list_of_log` represents a dataframe and can only take the values of either a list or NULL.")
  }

  if (!is.null(list_of_log)) {
    if (!dataset %in% names(list_of_log)) {
      stop(glue::glue("The element ", dataset, " is not present within the list_of_log."))
    }
    if (!cleaning_log %in% names(list_of_log)) {
      stop(glue::glue("The element ", cleaning_log, " is not present within the list_of_log."))
    }
    dataset <- list_of_log[[dataset]]
    cleaning_log <- list_of_log[[cleaning_log]]
  }

  if (is.null(list_of_log)) {
    if (!is.data.frame(dataset)) {
      stop(glue::glue("As the variable `list_of_log` is set to NULL, `dataset` must be a dataframe."))
    }

    if (!is.data.frame(cleaning_log)) {
      stop(glue::glue("As the variable `list_of_log` is set to NULL, `cleaning_log` must be a dataframe."))
    }
  }

  ## cleaning_log_info_check

  if (any(!cleaning_log_uuid_column %in% names(cleaning_log))) {
    stop(
      cleaning_log_uuid_column[!cleaning_log_uuid_column %in% names(cleaning_log)] |> glue::glue_collapse(", ") %>%
        glue::glue(., " can not be found in the cleaning log.")
    )
  }

  ## dataset info check
  if (any(!information_to_add %in% names(dataset))) {
    stop(
      information_to_add[!c(information_to_add, dataset_uuid_column) %in% names(dataset)] |> glue::glue_collapse(", ") %>%
        glue::glue(., " can not be found in the dataset.")
    )
  }

  dataset_raw <- dataset

  dataset <- dataset |> dplyr::select(dplyr::all_of(information_to_add), dplyr::all_of(dataset_uuid_column))
  cleaning_log <- cleaning_log |> dplyr::select(-dplyr::any_of(information_to_add))

  cleaning_log <- merge(cleaning_log, dataset,
    by.x = c(cleaning_log_uuid_column),
    by.y = c(dataset_uuid_column),
    all.x = TRUE
  )

  output <- list()

  output[["checked_dataset"]] <- dataset_raw
  output[["cleaning_log"]] <- cleaning_log

  output
}
