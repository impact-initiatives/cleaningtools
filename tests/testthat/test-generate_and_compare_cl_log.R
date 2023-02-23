library(tidyverse)
library(testthat)

# generate cleaning log ---------------------------------------------------

testthat::test_that("generating cleaning log", {
  expect_no_error(create_cleaning_log(
    raw_data = raw_data, raw_data_uuid = "X_uuid",
    clean_data = clean_data, clean_data_uuid = "X_uuid",
    check_for_deletion_log = T, check_for_variable_name = T
  ))

  deletaion_log <- cleaning_log |> filter(change_type == "remove_survey")
  cleaning_log2 <- cleaning_log |> filter(change_type != "remove_survey")

  expect_no_error(compare_cl_with_datasets(raw_data = raw_data,raw_data_uuid = "X_uuid",
                                           clean_data = clean_data,clean_data_uuid = "X_uuid",
                                           cleaning_log = cleaning_log2,cleaning_log_uuid = "X_uuid",
                                           cleaning_log_question_name = "questions",
                                           cleaning_log_new_value =  "new_value",
                                           cleaning_log_old_value = "old_value",
                                           deletion_log = deletaion_log,
                                           deletion_log_uuid = "X_uuid",
                                           check_for_deletion_log =T,
                                           check_for_variable_name = T))


})
