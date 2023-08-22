#' Check if the input passed to the check_others function is correct
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is uuid.
#' @param columns_to_check Columns to include in the checks
#'
#' @return nothing
#' @export
#'

check_others_checks <- function(dataset, uuid_column, columns_to_check) {
  if (!(uuid_column %in% names(dataset))) {
    stop("uuid column is missing")
  }

  if (is.null(columns_to_check)) {
    stop("provide the list of follow up questions")
  }

  if (!is.null(columns_to_check) & any(!columns_to_check %in% names(dataset))) {
    stop("at least one variable is missing from the dataset")
  }

  if (!is.null(columns_to_check) & any(!columns_to_check %in% names(dataset))) {
    stop("at least one variable is missing from the dataset")
  }
}



#' Generate a log for other follow up questions
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param columns_to_check Columns to include in the checks
#'
#' @return a log with all the values that may require recoding
#' @export
#'
#' @examples
#' check_others(
#'   dataset = cleaningtools::cleaningtools_clean_data,
#'   uuid_column = "X_uuid",
#'   columns_to_check = names(cleaningtools::cleaningtools_clean_data |>
#'     dplyr::select(ends_with("_other")) |>
#'     dplyr::select(-contains(".")))
#' )
#'
check_others <- function(dataset,
                         uuid_column = "uuid",
                         columns_to_check = NULL) {
  input_is_list <- F

  if (is.list(dataset) & !is.data.frame(dataset)) {
    if (!"checked_dataset" %in% names(dataset)) {
      stop("the dataset in the list should be named 'checked_dataset'")
    }

    input_is_list <- T
    input_list <- dataset
    dataset <- dataset$checked_dataset
  }

  check_others_checks(dataset, uuid_column, columns_to_check)


  other_log <- dataset %>%
    dplyr::select(uuid := !!rlang::sym(uuid_column), dplyr::all_of(columns_to_check)) %>%
    tidyr::pivot_longer(cols = -c("uuid"), names_to = "question", values_to = "old_value") %>%
    dplyr::filter(!is.na(old_value) & old_value != "") %>%
    dplyr::mutate(
      issue = "recode other"
    )

  if (input_is_list) {
    input_list$other_log <- other_log
    return(input_list)
  } else {
    return(list(
      checked_dataset = dataset,
      other_log = other_log
    ))
  }
}
