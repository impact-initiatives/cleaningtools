testthat::test_that("implement cleaning log", {
  cleaningtools_clean_data_fi <- create_clean_data(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaningtools::cleaningtools_cleaning_log,
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  )
  expect_equal(nrow(cleaningtools_clean_data_fi), nrow(cleaningtools::cleaningtools_clean_data))


  ## check_multiple input
  cleaning_log_1 <- cleaningtools::cleaningtools_cleaning_log
  cleaning_log_1$change_type[[2]] <- "change_res"
  cleaning_log_1$change_type[[3]] <- "Chane_res"
  cleaning_log_1$change_type[[30]] <- "no_change"
  cleaning_log_1$change_type[[724]] <- "blank"
  cleaning_log_1$change_type[[646]] <- "delete"




  cleaningtools_clean_data_multiple <- create_clean_data(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_1,
    cleaning_log_change_type_column = "change_type",
    change_response_value = c("change_res", "Chane_res", "change_response"),
    NA_response_value = c("blank_response", "blank"),
    no_change_value = c("no_action", "no_change"),
    remove_survey_value = c("remove_survey", "delete"),
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  )

  cleaning_log_1_log_only <- cleaning_log_1 |> dplyr::filter(!change_type %in% c("remove_survey", "delete"))
  deletation_log <- cleaning_log_1 |> dplyr::filter(change_type %in% c("remove_survey", "delete"))


  expect_equal(nrow(review_cleaning(
    raw_dataset = cleaningtools::cleaningtools_raw_data,
    raw_dataset_uuid_column = "X_uuid",
    clean_dataset = cleaningtools_clean_data_multiple,
    clean_dataset_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_1_log_only,
    cleaning_log_uuid_column = "X_uuid",
    cleaning_log_question_column = "questions",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = deletation_log,
    deletion_log_uuid_column = "X_uuid",
    check_for_deletion_log = T
  ) |> dplyr::filter(df.change_type != "no_action")), 3)


  #####

  ## check change response
  # change_response
  expect_equal(cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "728e4de0-7356-4bb5-9db4-9479b3ffe098", ]$treat_drink_water, "never_treat")
  # change_res
  expect_equal(cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "fcc6b9e8-c72a-4228-8dc0-6e803114b6d6", ]$water_tank_litres_nb, 7000)
  # remove
  expect_false("248f7774-319a-4368-91d2-a1f8dec9972d" %in% cleaningtools_clean_data_multiple$X_uuid)
  # delete
  expect_false("683e97a1-a6df-4442-b1ba-effe04612b86" %in% cleaningtools_clean_data_multiple$X_uuid)

  # no_action
  expect_equal(
    cleaningtools::cleaningtools_raw_data[cleaningtools::cleaningtools_raw_data$X_uuid == "bc6df6b9-c888-4cd3-9198-a52f3a1eb677", "air_coolers_hours"],
    cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "bc6df6b9-c888-4cd3-9198-a52f3a1eb677", "air_coolers_hours"]
  )

  # no change
  expect_equal(
    cleaningtools::cleaningtools_raw_data[cleaningtools::cleaningtools_raw_data$X_uuid == "f949f8ee-6536-4627-a850-dc0b092ba379", "air_coolers_nb"],
    cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "f949f8ee-6536-4627-a850-dc0b092ba379", "air_coolers_nb"]
  )


  #### NA check
  cleaning_log_na <- cleaningtools::cleaningtools_cleaning_log
  cleaning_log_na$change_type[[1]] <- NA

  expect_error(create_clean_data(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_na,
    cleaning_log_change_type_column = "change_type",
    no_change_value = c("no_action", ""),
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  ), "Missing values in change_type")

  expect_equal(nrow(review_cleaning_log(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_na,
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  )), 1)

  ### wrong question
  cleaning_log_wrong <- cleaningtools::cleaningtools_cleaning_log %>% dplyr::mutate(
    questions = cleaningtools::cleaningtools_cleaning_log$questions %>% stringr::str_replace_all("treat_drink_water", "treat_drink_water2")
  )


  expect_error(create_clean_data(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_wrong,
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  ), "Make sure all names in cleaning_log_question_column values in the cleaning log are in dataset")


  expect_equal(nrow(review_cleaning_log(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_wrong,
    change_response_value = "change_response",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  )), 4)


  expect_output(review_cleaning_log(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaning_log_wrong,
    change_response_value = "change_response",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  ), "cleaning log has issues, see output table")


  expect_error(create_clean_data(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaningtools::cleaningtools_cleaning_log,
    cleaning_log_change_type_column = "change_type",
    change_type_for_blank_response = "c",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  ))

  expect_error(create_clean_data(
    raw_dataset = cleaningtools::cleaningtools_raw_data, raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaningtools::cleaningtools_cleaning_log,
    cleaning_log_change_type_column = "change_ype",
    cleaning_log_question_column = "questions", cleaning_log_new_value_column = "new_value",
    cleaning_log_uuid_column = "X_uuid"
  ), "cleaning_log_change_type_column column not found in cleaning log")
})


testthat::test_that("check cleaning log", {
  expect_no_error(review_cleaning_log(
    raw_dataset = cleaningtools::cleaningtools_raw_data,
    raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaningtools::cleaningtools_cleaning_log,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "questions",
    cleaning_log_uuid_column = "X_uuid",
    cleaning_log_new_value_column = "new_value"
  ))

  expect_error(review_cleaning_log(
    raw_dataset = cleaningtools::cleaningtools_raw_data,
    raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaningtools::cleaningtools_cleaning_log,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_esponse", cleaning_log_question_column = "questions",
    cleaning_log_uuid_column = "X_uuid", cleaning_log_new_value_column = "new_value"
  ), "Value in change_response_value not found")

  expect_error(review_cleaning_log(
    raw_dataset = cleaningtools::cleaningtools_raw_data,
    raw_data_uuid_column = "X_uuid",
    cleaning_log = cleaningtools::cleaningtools_cleaning_log,
    cleaning_log_change_type_column = "chage_type",
    change_response_value = "change_response", cleaning_log_question_column = "questions",
    cleaning_log_uuid_column = "X_uuid", cleaning_log_new_value_column = "new_value"
  ), "cleaning_log_change_type_column column not found in cleaning log")
})
