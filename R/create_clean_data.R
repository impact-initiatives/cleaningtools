#' implement cleaning log on raw data set.
#' @param raw_dataset Raw dataset
#' @param raw_data_uuid_column uuid column in the raw dataset. Default is uuid.
#' @param cleaning_log cleaning + deletion log (data.frame).
#' @param cleaning_log_uuid_column uuid column in the cleaning log. Default is "uuid".
#' @param cleaning_log_question_column column in cleaning log which specifies data set column to change
#' @param cleaning_log_new_value_column cleaning log column specifying the new correct value
#' @param cleaning_log_change_type_column column in cleaning log which specifies change type to be made
#' @param change_response_value values in change type column which should be changed to a new value.
#' @param NA_response_value values in change type column which should be blank (NA).
#' @param no_change_value values in change type column which should NOT be changed to a new value.
#' @param remove_survey_value values in change type column which should be deleted from the data.
#'
#' @return clean data set
#' @export
#' @examples
#' cleaning_log_test <- data.frame(
#'   uuid = paste0("uuid", 1:4),
#'   question = c("age", "gender", "pop_group", "strata"),
#'   change_type = c("blank_response", "no_change", "Delete", "change_res"),
#'   new_value = c(NA_character_, NA_character_, NA_character_, "st-a")
#' )
#' test_data <- data.frame(
#'   uuid = paste0("uuid", 1:4),
#'   age = c(180, 23, 45, 67),
#'   gender = c("male", "female", "male", "female"),
#'   pop_group = c("idp", "refugee", "host", "idp"),
#'   strata = c("a", "b", "c", "d")
#' )
#'
#'
#' review_cleaning_log(
#'   raw_dataset = test_data,
#'   raw_data_uuid_column = "uuid",
#'   cleaning_log = cleaning_log_test,
#'   cleaning_log_change_type_column = "change_type",
#'   change_response_value = "change_res",
#'   cleaning_log_question_column = "question",
#'   cleaning_log_uuid_column = "uuid",
#'   cleaning_log_new_value_column = "new_value"
#' )
#'
#' create_clean_data(
#'   raw_dataset = test_data, raw_data_uuid_column = "uuid", cleaning_log = cleaning_log_test,
#'   cleaning_log_change_type_column = "change_type",
#'   change_response_value = "change_res",
#'   NA_response_value = "blank_response",
#'   no_change_value = "no_change",
#'   remove_survey_value = "Delete",
#'   cleaning_log_question_column = "question",
#'   cleaning_log_uuid_column = "uuid",
#'   cleaning_log_new_value_column = "new_value"
#' )
create_clean_data <- function(raw_dataset,
                              raw_data_uuid_column = "uuid",
                              cleaning_log,
                              cleaning_log_uuid_column = "uuid",
                              cleaning_log_question_column,
                              cleaning_log_new_value_column,
                              cleaning_log_change_type_column,
                              change_response_value = "change_response",
                              NA_response_value = "blank_response",
                              no_change_value = "no_action",
                              remove_survey_value = "remove_survey") {
  raw_dataset <- raw_dataset %>% dplyr::mutate_all(as.character)
  cleaning_log <- cleaning_log %>% dplyr::mutate_all(as.character)



  assertthat::assert_that(cleaning_log_change_type_column %in% names(cleaning_log),
                          msg = "cleaning_log_change_type_column column not found in cleaning log")

  all_type <-
    c(change_response_value,
      NA_response_value,
      no_change_value,
      remove_survey_value)

  cleaning_log <- cleaning_log |> dplyr::mutate(
    change_type_created_f = dplyr::case_when(
      !!rlang::sym(cleaning_log_change_type_column) %in% change_response_value ~ "change_response",
      !!rlang::sym(cleaning_log_change_type_column) %in% NA_response_value ~ "blank_response",
      !!rlang::sym(cleaning_log_change_type_column) %in% no_change_value ~ "no_action",
      !!rlang::sym(cleaning_log_change_type_column) %in% remove_survey_value ~ "remove_survey"
    )
  )

  assertthat::assert_that(any(!is.na(cleaning_log[[cleaning_log_change_type_column]])),
                          msg = "You have NAs in change_type option(s)")


  check_not_entry_value_in_change_type <-
    unique(cleaning_log[[cleaning_log_change_type_column]])[!unique(cleaning_log[[cleaning_log_change_type_column]]) %in% all_type]
  if (length(check_not_entry_value_in_change_type) > 0) {
    print(check_not_entry_value_in_change_type)
    stop("Missing values in change_type")
  }

  assertthat::assert_that(all(cleaning_log[[cleaning_log_change_type_column]] %in% all_type),
                          msg = "You have missing change_type option(s)")

  cleaning_log[[cleaning_log_question_column]] <-
    cleaning_log[[cleaning_log_question_column]] %>% trimws()
  cleaning_log[[cleaning_log_new_value_column]] <-
    cleaning_log[[cleaning_log_new_value_column]] %>% trimws()
  cl_change_type_options <-
    c("change_response",
      "remove_survey",
      "blank_response",
      "no_action")
  cl_change_response <-
    cleaning_log %>% dplyr::filter(change_type_created_f %in% c(cl_change_type_options[1], cl_change_type_options[3]))
  cl_change_response <- cl_change_response %>%
    dplyr::mutate(
      !!cleaning_log_new_value_column := ifelse(
        change_type_created_f == cl_change_type_options[3],
        NA,
        !!rlang::sym(cleaning_log_new_value_column)
      )
    )
  cl_remove_survey <-
    cleaning_log %>% dplyr::filter(change_type_created_f == cl_change_type_options[2])

  if (all(cl_change_response[[cleaning_log_question_column]] %in% colnames(raw_dataset)) == F) {
    problem_question_in_cl <-
      cl_change_response[[cleaning_log_question_column]][cl_change_response[[cleaning_log_question_column]] %in% colnames(raw_dataset) == FALSE]
    print(paste0(problem_question_in_cl, ": Not found in the dataset"))
  }

  if (all(cleaning_log[[cleaning_log_uuid_column]] %in% raw_dataset[[raw_data_uuid_column]] == F)) {
    problem_uuid_in_cl <-
      cleaning_log[[cleaning_log_uuid_column]][cleaning_log[[cleaning_log_uuid_column]] %in% c(raw_dataset[[raw_data_uuid_column]], "all_data") == FALSE]
    print(problem_uuid_in_cl)
    print("Not found in the datase")
  }
  assertthat::assert_that(all(cl_change_response[[cleaning_log_question_column]] %in% colnames(raw_dataset)),
                          msg = "Make sure all names in cleaning_log_question_column values in the cleaning log are in dataset")
  assertthat::assert_that(all(cleaning_log[[cleaning_log_uuid_column]] %in% raw_dataset[[raw_data_uuid_column]]),
                          msg = "Make sure all uuids in cleaing log are in data set")

  if (nrow(cl_change_response) > 0) {
    for (i in 1:nrow(cl_change_response)) {
      print(cl_change_response[[cleaning_log_question_column]][i])
      cl_uuid_temp <-
        cl_change_response[[cleaning_log_uuid_column]][i]
      cl_question_temp <-
        cl_change_response[[cleaning_log_question_column]][i]
      cl_new_val_temp <-
        cl_change_response[[cleaning_log_new_value_column]][i]

      if (cl_uuid_temp != "all_data") {
        raw_dataset[raw_dataset[[raw_data_uuid_column]] == cl_uuid_temp, cl_question_temp] <-
          cl_new_val_temp
      }

      if (cl_uuid_temp == "all_data") {
        raw_dataset[, cl_question_temp] <- cl_new_val_temp
      }
    }
  } else {
    print("no change_response in log")
  }
  if (nrow(cl_remove_survey) > 0) {
    raw_dataset <-
      raw_dataset %>% dplyr::filter(!!rlang::sym(raw_data_uuid_column) %in% cl_remove_survey[[cleaning_log_uuid_column]] == FALSE)
  } else {
    print("no surveys to remove in log")
  }

  return(raw_dataset %>% utils::type.convert(as.is = T))
}


#' Review cleaning log
#'
#' Review the cleaning log. It can be used before create_clean_data to check cleaning log. Possible
#' flags are:
#' - Value in change_response_value not found
#' - question_does_not_exist: question does not exist
#' - uuid_does_not_exist: cannot find a UUID
#' - na_in_change_type: change_type is NA
#'
#' @param raw_dataset Raw dataset
#' @param raw_data_uuid_column uuid column in the raw dataset. Default is "uuid".
#' @param cleaning_log cleaning log (data.frame)
#' @param cleaning_log_uuid_column uuid column in the cleaning log. Default is "uuid".
#' @param cleaning_log_question_column column in cleaning log which specifies data set column to change
#' @param cleaning_log_new_value_column cleaning log column specifying the new correct value
#' @param cleaning_log_change_type_column column in cleaning log which specifies change type to be made
#' @param change_response_value values in change type column which should be changed to a new value.
#' @return review of the cleaning log with flagged entries and note explaining the flags.
#' @export


review_cleaning_log <- function(raw_dataset,
                               raw_data_uuid_column = "uuid",
                               cleaning_log,
                               cleaning_log_uuid_column = "uuid",
                               cleaning_log_question_column,
                               cleaning_log_new_value_column,
                               cleaning_log_change_type_column,
                               change_response_value = "change_response") {
  cleaning_log[[cleaning_log_question_column]] <-
    cleaning_log[[cleaning_log_question_column]] %>% trimws()
  cleaning_log[[cleaning_log_new_value_column]] <-
    cleaning_log[[cleaning_log_new_value_column]] %>% trimws()


  assertthat::assert_that(cleaning_log_change_type_column %in% names(cleaning_log),
                          msg = "cleaning_log_change_type_column column not found in cleaning log")

  assertthat::assert_that(all(change_response_value %in% cleaning_log[[cleaning_log_change_type_column]]),
                          msg = "Value in change_response_value not found")

  cl_change_col_prob_df <- cleaning_log %>%
    dplyr::filter(!!rlang::sym(cleaning_log_change_type_column) %in% change_response_value) %>%
    dplyr::mutate(cl_prob = "question_does_not_exist") %>%
    dplyr::filter(!!rlang::sym(cleaning_log_question_column) %in% colnames(raw_dataset) == FALSE) %>%
    dplyr::select(cl_prob, dplyr::everything())

  cl_uuid_prob_df <- cleaning_log %>%
    dplyr::filter(!!rlang::sym(cleaning_log_uuid_column) %in% c(raw_dataset[[raw_data_uuid_column]], "all_data") == FALSE) %>%
    dplyr::mutate(cl_prob = "uuid_does_not_exist") %>%
    dplyr::filter(!!rlang::sym(cleaning_log_uuid_column) %in% raw_dataset[[raw_data_uuid_column]] == FALSE) %>%
    dplyr::select(cl_prob, dplyr::everything())

  na_change_type_prob_df <- cleaning_log %>%
    dplyr::filter(is.na(!!rlang::sym(cleaning_log_change_type_column))) %>%
    dplyr::mutate(cl_prob = "na_in_change_type") %>%
    dplyr::select(cl_prob, dplyr::everything())

  cl_problems_df <-
    dplyr::bind_rows(get0("cl_change_col_prob_df"), get0("cl_uuid_prob_df")) |>
    dplyr::bind_rows(get0("na_change_type_prob_df"))

  if (nrow(cl_problems_df) > 0) {
    print("cleaning log has issues, see output table")
  } else {
    cl_problems_df <- "no issues in cleaning log found"
  }
  return(cl_problems_df)
}
