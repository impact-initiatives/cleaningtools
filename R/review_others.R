#' Review discrepancy between kobo relevancies and the dataset.
#'
#' @param dataset dataset to be check
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param kobo_survey Kobo survey sheet.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @param information_to_add string vector optional, if to add some information to the log (today,village etc.)
#' @param columns_not_to_check Columns to exclude from the checks
#' @return Any discrepancy between kobo relevancies and the dataset.
#' @export
#'
#' @examples
#' review_others(
#'   dataset = cleaningtools::cleaningtools_clean_data,
#'   uuid_column = "X_uuid", kobo_survey = cleaningtools_survey
#' )
#'
review_others <- function(dataset,
                          uuid_column = "uuid",
                          kobo_survey,
                          sm_separator = ".",
                          information_to_add = NULL,
                          columns_not_to_check = NULL) {
  ### variables to add not found
  if (any(!information_to_add %in% names(dataset))) {
    not_found <- information_to_add[!information_to_add %in% names(dataset)]

    msg <- not_found |>
      glue::glue_collapse(sep = ", ") %>%
      glue::glue("Following variables: ", ., " cannot be found in the dataset.")
    stop(msg)
  }

  ### columns_not_to_check not found [warning]

  if (any(!columns_not_to_check %in% names(dataset))) {
    not_found <- columns_not_to_check[!columns_not_to_check %in% names(dataset)]

    msg <- not_found |>
      glue::glue_collapse(sep = ", ") %>%
      glue::glue("Following variables: ", ., " cannot be found in the dataset.")
    warning(msg)
  }


  ### UUID checkk error

  if (!uuid_column %in% names(dataset)) {
    stop(paste0(uuid_column, " not found in the dataset."))
  }


  all_logic <- create_logic_for_other(
    kobo_survey = kobo_survey,
    compare_with_dataset = TRUE,
    dataset = dataset,
    sm_separator = sm_separator
  )



  check <- check_logical_with_list(
    dataset = dataset,
    uuid_column = uuid_column,
    list_of_check = all_logic,
    check_id_column = "id",
    check_to_perform_column = "logic",
    columns_to_clean_column = "variables_to_clean_column",
    information_to_add = information_to_add,
    description_column = "description",
    bind_checks = T
  )

  check$logical_all <- check$logical_all |> dplyr::filter(!check$logical_all$question %in% columns_not_to_check)
  check$logical_all |> as.data.frame()
}
