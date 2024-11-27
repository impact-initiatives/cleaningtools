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

#
# testthat::test_that("check cleaning log", {
#   expect_no_error(review_cleaning_log(
#     raw_dataset = cleaningtools::cleaningtools_raw_data,
#     raw_data_uuid_column = "X_uuid",
#     cleaning_log = cleaningtools::cleaningtools_cleaning_log,
#     cleaning_log_change_type_column = "change_type",
#     change_response_value = "change_response",
#     cleaning_log_question_column = "questions",
#     cleaning_log_uuid_column = "X_uuid",
#     cleaning_log_new_value_column = "new_value"
#   ))
#
#   expect_error(review_cleaning_log(
#     raw_dataset = cleaningtools::cleaningtools_raw_data,
#     raw_data_uuid_column = "X_uuid",
#     cleaning_log = cleaningtools::cleaningtools_cleaning_log,
#     cleaning_log_change_type_column = "change_type",
#     change_response_value = "change_esponse", cleaning_log_question_column = "questions",
#     cleaning_log_uuid_column = "X_uuid", cleaning_log_new_value_column = "new_value"
#   ), "Value in change_response_value not found")
#
#   expect_error(review_cleaning_log(
#     raw_dataset = cleaningtools::cleaningtools_raw_data,
#     raw_data_uuid_column = "X_uuid",
#     cleaning_log = cleaningtools::cleaningtools_cleaning_log,
#     cleaning_log_change_type_column = "chage_type",
#     change_response_value = "change_response", cleaning_log_question_column = "questions",
#     cleaning_log_uuid_column = "X_uuid", cleaning_log_new_value_column = "new_value"
#   ), "cleaning_log_change_type_column column not found in cleaning log")
# })
#
# testthat::test_that("recode_other recode correctly", {
#
#   # select one
#
#   so_raw_dataset <- data.frame(
#     uuid = letters[1:11],
#     main_source_water = c(
#       rep("other", 4),
#       rep("tab_water_at_home", 7)
#     ),
#     main_source_water_other = c(
#       "we have water at home",
#       "we have rain collector",
#       "We buy bottled water",
#       "Nothing to say",
#       rep(NA, 7)
#     )
#   )
#
#   so_cleaning_log <- data.frame(
#     uuid = c("a", "b", "c", "d"),
#     question = "main_source_water_other",
#     old_value = c(
#       "we have water at home",
#       "we have rain collector",
#       "We buy bollted water",
#       "Nothing to say"
#     ),
#     issue = "recode other",
#     change_type = c(
#       "recode_other", # recode to tab_water_at_home
#       "no_action",
#       "recode_other", # create new option bottled_water
#       "blank_response"
#     )
#   )
#
#   so_expected_output <- data.frame(
#     uuid = letters[1:11],
#     main_source_water = c(
#       "tab_water_at_home",
#       "other",
#       "bottled_water",
#       NA,
#       rep("tab_water_at_home", 7)
#     ),
#     main_source_water_other = c(
#       NA,
#       "we have rain collector",
#       NA,
#       NA,
#       rep(NA, 7)
#     )
#   )
#
#   so_actual_output <- create_clean_data(
#     so_raw_dataset,
#     so_cleaning_log
#   )
#
#   expect_equal(so_actual_output, so_expected_output)
#
#   # select multiple
#
#   sm_raw_dataset <- data.frame(
#     uuid = letters[1:11],
#     income_source = c(
#       rep("work temporary_work", 3),
#       rep("work", 4),
#       rep("work other", 2),
#       rep("other", 2)
#     ),
#     income_source.work = c(rep(TRUE, 9), rep(FALSE, 2)),
#     income_source.temporary_work = c(rep(TRUE, 3), rep(FALSE, 8)),
#     income_source.other = c(rep(FALSE, 7), rep(TRUE, 4)),
#     income_source_other = c(
#       rep(NA, 7),
#       "Nothing to say",
#       "I get my pension",
#       "I rent my lands",
#       "I work sometimes at the market"
#     )
#   )
#
#   sm_cleaning_log <- data.frame(
#     uuid = c("h", "i", "j", "k"),
#     question = "income_source_other",
#     old_value = c(
#       "Nothing to say",
#       "I get my pension",
#       "I rent my lands",
#       "I work sometimes at the market"
#     ),
#     issue = "recode other",
#     change_type = c(
#       "blank_response",
#       "recode_other", # recode to pension
#       "no_action",
#       "recode_other" # recode to temporary_work
#     )
#   )
#
#   sm_expected_output <- data.frame(
#     uuid = letters[1:11],
#     income_source = c(
#       rep("work temporary_work", 3),
#       rep("work", 4),
#       rep("work other", 2), #not mandatory to change, it should change with recreate_parent
#       rep("other", 2) #not mandatory to change, it should change with recreate_parent
#     ),
#     income_source.work = c(rep(TRUE, 9), rep(FALSE, 2)),
#     income_source.temporary_work = c(rep(TRUE, 3), rep(FALSE, 7), TRUE),
#     income_source.pension = c(rep(FALSE, 7), TRUE, rep(FALSE, 3)),
#     income_source.other = c(
#       rep(FALSE, 7),
#       FALSE,
#       FALSE,
#       TRUE,
#       FALSE
#     ),
#     income_source_other = c(
#       rep(NA, 7),
#       NA,
#       NA,
#       "I rent my lands",
#       NA
#     )
#   )
#
#   sm_actual_output <- create_clean_data(
#     sm_raw_dataset,
#     sm_cleaning_log
#   )
#
#   expect_equal(sm_actual_output, sm_expected_output)
#
#   # open text
#
#   open_raw_dataset <- data.frame(
#     uuid = letters[1:11],
#     describe_open = c(
#       "calm area",
#       "the area is quite",
#       "nothing to add",
#       "nothing to add",
#       rep(NA, 7)
#     )
#   )
#
#   open_text_cleaning <- data.frame(
#     uuid = c("a", "b", "c", "d"),
#     question = "describe_open",
#     old_value = c(
#       "calm area",
#       "the area is quite",
#       "nothing to add",
#       "nothing to add"
#     ),
#     issue = "recode other",
#     change_type = c(
#       "no_action",
#       "change_response",
#       "blank_response",
#       "blank_response"
#     ),
#     new_value = c(
#       "calm area",
#       "calm area",
#       "",
#       ""
#     )
#   )
#
#   open_text_expected_output <- data.frame(
#     uuid = letters[1:11],
#     describe_open = c(
#       "calm area",
#       "calm area",
#       rep(NA, 9)
#     )
#   )
#
#   open_text_actual_output <- create_clean_data(
#     open_raw_dataset,
#     open_text_cleaning
#   )
#
#   expect_equal(open_text_actual_output, open_text_expected_output)
# })

