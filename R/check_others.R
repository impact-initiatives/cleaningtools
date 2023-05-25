

#' Check if the input passed to the check_others function is correct
#'
#' @param dataset dataset
#' @param uuid uuid column
#' @param var_list list of questions to generate the logs for
#'
#' @return nothing
#' @export
#'

check_others_checks <- function(dataset,uuid,var_list) {

  if (!(uuid %in% names(dataset))) {
    stop("uuid column is missing")
  }

  if (is.null(var_list)) {
    stop("provide the list of follow up questions")
  }

  if (!is.null(var_list) & any(!var_list %in% names(dataset))) {
    stop("at least one variable is missing from the dataset")
  }

  if (!is.null(var_list) & any(!var_list %in% names(dataset))) {
    stop("at least one variable is missing from the dataset")
  }


}



#' Generate a log for other follow up questions
#'
#' @param dataset dataset
#' @param uuid uuid column
#' @param var_list list of questions to generate the logs for
#'
#' @return a log with all the values that may require recoding
#' @export
#'
#' @examples
#' check_others(dataset = cleaningtools::cleaningtools_clean_data,
#' uuid = "X_uuid",
#' var_list = names(cleaningtools::cleaningtools_clean_data |>
#' dplyr::select(ends_with("_other")) |>
#' dplyr::select(-contains("."))))
#'

check_others <- function(dataset,
                         uuid = "uuid",
                         var_list = NULL
) {


  input_is_list = F

  if(is.list(dataset) & !is.data.frame(dataset)) {

    if(!"checked_dataset" %in% names(dataset)) {
      stop("the dataset in the list should be named 'checked_dataset'")
    }

    input_is_list = T
    input_list = dataset
    dataset = dataset$checked_dataset
  }

  check_others_checks(dataset,uuid,var_list)


  other_log <- dataset %>%
    dplyr::select(uuid := !!rlang::sym(uuid), dplyr::all_of(var_list)) %>%
    tidyr::pivot_longer(cols= -c("uuid"), names_to = "question", values_to = "old_value") %>%
    dplyr::filter(!is.na(old_value) & old_value != "") %>% dplyr::mutate(
      issue = "recode other"
    )

  if(input_is_list) {
    input_list$other_log = other_log
    return(input_list)
  } else {
    return(list(checked_dataset = dataset,
                other_log = other_log))
  }


}
