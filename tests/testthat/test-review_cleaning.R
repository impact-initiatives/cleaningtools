# creating and reviewing cleaning log ---------------------------------------------------

testthat::test_that("Checking with test data", {
  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:6),
    gender = rep(c("male", "female"), 3),
    expenditure = c(200, 300, 240, 44444, 300, 280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid1", "uuid4"),
    question = c("gender", "expenditure"),
    change_type = c("change_response", "blank_response"),
    old_value = c("male", "44444"),
    new_value = c("female", NA_character_),
    comment = c("An alteration was performed", "changed to NA")
  )

  test_deletion_log <- tibble::tibble(
    uuid = "uuid6",
    change_type = "remove_survey",
    comment = "No matching uuid in the cleaned dataset"
  )

  expected_cleaning_log <- test_deletion_log |>
    dplyr::bind_rows(test_cleaning_log) |>
    as.data.frame()

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c("female", "female", "male", "female", "male"),
    expenditure = c(200, 300, 240, NA_real_, 300)
  )

  cleaning_log_actual <- create_cleaning_log(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    check_for_deletion_log = T,
    check_for_variable_name = F
  ) |> dplyr::select(dplyr::all_of(names(expected_cleaning_log)))
  ### check create cleaning log
  testthat::expect_equal(cleaning_log_actual, expected_cleaning_log)

  ## check review cleaning log

  testthat::expect_equal(nrow(
    review_cleaning(
      raw_dataset = test_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = test_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = test_cleaning_log,
      cleaning_log_uuid_column = "uuid",
      cleaning_log_question_column = "question",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      deletion_log = test_deletion_log,
      deletion_log_uuid_column = "uuid",
      check_for_deletion_log = T
    ) %>%
      suppressWarnings()
  ), 0)

  test_clean_data_faulty <- test_clean_data
  test_clean_data_faulty[3, "expenditure"] <- 444
  test_clean_data_faulty[1, "gender"] <- "male"

  actual <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data_faulty,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
  ) |> as.data.frame()


  expected_output <- data.frame(
    uuid = c("uuid1", "uuid3"),
    df.question = c("gender", "expenditure"),
    df.change_type = c("change_response", "change_response"),
    df.new_value = c("male", "444"),
    cl.new_value = c("female", NA_character_),
    df.old_value = c("male", "240"),
    cl.old_value = c("male", NA_character_),
    comment = c("Changes were not applied", "Entry missing in cleaning log")
  )

  testthat::expect_equal(actual, expected_output)

  ########## check with different uuid name/no issue

  test_raw_data <- tibble::tibble(
    X_uuid = paste0("uuid", 1:6),
    gender = rep(c("male", "female"), 3),
    expenditure = c(200, 300, 240, 44444, 300, 280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    `_uuid` = c("uuid1", "uuid3"),
    question = c("gender", "expenditure"),
    change_type_col = c("change_response", "blank_response"),
    old_value = c("male", "240"),
    new.value = c("female", NA_character_),
    comment = c("An alteration was performed", "changed to NA")
  )

  test_deletion_log <- tibble::tibble(
    `_uuid` = "uuid6",
    change_type_col = "remove_survey",
    comment = "No matching uuid in the cleaned dataset"
  )

  expected_cleaning_log <- test_deletion_log |>
    dplyr::bind_rows(test_cleaning_log) |>
    as.data.frame() |>
    dplyr::rename(
      uuid = `_uuid`,
      change_type = change_type_col,
      new_value = new.value
    )

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c("female", "female", "male", "female", "male"),
    expenditure = c(200, 300, NA_real_, 44444, 300)
  )

  cleaning_log_actual <- create_cleaning_log(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "X_uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    check_for_deletion_log = T,
    check_for_variable_name = F
  ) |> dplyr::select(dplyr::all_of(names(expected_cleaning_log)))
  ### check create cleaning log
  testthat::expect_equal(cleaning_log_actual, expected_cleaning_log)

  ## check review cleaning log

  testthat::expect_equal(nrow(
    review_cleaning(
      raw_dataset = test_raw_data,
      raw_dataset_uuid_column = "X_uuid",
      clean_dataset = test_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = test_cleaning_log,
      cleaning_log_uuid_column = "_uuid",
      cleaning_log_question_column = "question",
      cleaning_log_change_type_column = "change_type_col",
      cleaning_log_new_value_column = "new.value",
      cleaning_log_old_value_column = "old_value",
      deletion_log = test_deletion_log,
      deletion_log_uuid_column = "_uuid",
      check_for_deletion_log = T
    ) %>%
      suppressWarnings()
  ), 0)



  ########## check with different uuid name/with issue

  test_clean_data_faulty <- test_clean_data
  test_clean_data_faulty[4, "expenditure"] <- 444
  test_clean_data_faulty[1, "gender"] <- "male"
  test_cleaning_log <-
    test_cleaning_log |> dplyr::rename(question_name = question)

  actual_output <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "X_uuid",
    clean_dataset = test_clean_data_faulty,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "_uuid",
    cleaning_log_change_type_column = "change_type_col",
    cleaning_log_question_column = "question_name",
    cleaning_log_new_value_column = "new.value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "_uuid",
    check_for_deletion_log = T
  ) |> as.data.frame()
  expected_output <- data.frame(
    uuid = c("uuid1", "uuid4"),
    df.question = c("gender", "expenditure"),
    df.change_type = c("change_response", "change_response"),
    df.new_value = c("male", "444"),
    cl.new_value = c("female", NA_character_),
    df.old_value = c("male", "44444"),
    cl.old_value = c("male", NA_character_),
    comment = c("Changes were not applied", "Entry missing in cleaning log")
  )

  testthat::expect_equal(actual_output, expected_output)
})



testthat::test_that("Checking with real data", {
  expect_no_error(
    create_cleaning_log(
      raw_dataset = cleaningtools::cleaningtools_raw_data,
      raw_dataset_uuid_column = "X_uuid",
      clean_dataset = cleaningtools::cleaningtools_clean_data,
      clean_dataset_uuid_column = "X_uuid",
      check_for_deletion_log = T,
      check_for_variable_name = T
    )
  )

  deletion_log <-
    cleaningtools::cleaningtools_cleaning_log |> dplyr::filter(change_type == "remove_survey")
  cleaning_log2 <-
    cleaningtools::cleaningtools_cleaning_log |> dplyr::filter(change_type != "remove_survey")

  expect_no_error(
    review_cleaning(
      raw_dataset = cleaningtools::cleaningtools_raw_data,
      raw_dataset_uuid_column = "X_uuid",
      clean_dataset = cleaningtools::cleaningtools_clean_data,
      clean_dataset_uuid_column = "X_uuid",
      cleaning_log = cleaning_log2,
      cleaning_log_uuid_column = "X_uuid",
      cleaning_log_question_column = "questions",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      deletion_log = deletion_log,
      deletion_log_uuid_column = "X_uuid",
      check_for_deletion_log = T
    ) %>%
      suppressWarnings()
  )
})




## create cleaning log with no deletion survey

testthat::test_that("create cleaning log with no deletion survey", {
  me_raw_data <- data.frame(
    uuid = 1:5,
    expenditure = c(500, 600, 700, NA, NA),
    gender = c("male", "female", "male", NA, NA),
    my_old_var = letters[26:22]
  )


  me_clean_data <- data.frame(
    uuid = 1:5,
    expenditure = c(500, 666, NA, 800, NA),
    gender = c("male", "male", NA, "male", NA),
    my_new_var = letters[1:5]
  )
  expected <- structure(
    list(
      uuid = c(
        "all", "all", "2", "2", "4", "4", "3",
        "3"
      ),
      question = c(
        "my_old_var",
        "my_new_var",
        "expenditure",
        "gender",
        "expenditure",
        "gender",
        "expenditure",
        "gender"
      ),
      change_type = c(
        "variable_removed",
        "variable_added",
        "change_response",
        "change_response",
        "change_response",
        "change_response",
        "blank_response",
        "blank_response"
      ),
      new_value = c(
        NA, NA, "666", "male", "800",
        "male", NA, NA
      ),
      old_value = c(
        NA, NA, "600", "female", NA,
        NA, "700", "male"
      ),
      comment = c(
        "variable removed from the clean dataset",
        "variable added to the clean dataset",
        "An alteration was performed",
        "An alteration was performed",
        "NA changed to value",
        "NA changed to value",
        "changed to NA",
        "changed to NA"
      )
    ),
    class = "data.frame",
    row.names = c(
      "...1",
      "...2", "2", "21", "4", "41", "3", "31"
    )
  )


  actual <- create_cleaning_log(me_raw_data,
    "uuid",
    me_clean_data,
    "uuid",
    check_for_deletion_log = T
  )

  testthat::expect_equal(actual, expected)

  ### check added variable
  expected_v2 <- structure(
    list(
      uuid = c(
        "5", "all", "all", "2", "2", "4", "4",
        "3", "3"
      ),
      question = c(
        NA,
        "my_old_var",
        "my_new_var",
        "expenditure",
        "gender",
        "expenditure",
        "gender",
        "expenditure",
        "gender"
      ),
      change_type = c(
        "added_survey",
        "variable_removed",
        "variable_added",
        "change_response",
        "change_response",
        "change_response",
        "change_response",
        "blank_response",
        "blank_response"
      ),
      new_value = c(
        NA,
        NA, NA, "666", "male", "800", "male", NA, NA
      ),
      old_value = c(
        NA,
        NA, NA, "600", "female", NA, NA, "700", "male"
      ),
      comment = c(
        "Survey added to the clean dataset.",
        "variable removed from the clean dataset",
        "variable added to the clean dataset",
        "An alteration was performed",
        "An alteration was performed",
        "NA changed to value",
        "NA changed to value",
        "changed to NA",
        "changed to NA"
      )
    ),
    class = "data.frame",
    row.names = c(
      "...1",
      "...2", "...3", "2", "21", "4", "41", "3", "31"
    )
  )
  me_raw_data <- data.frame(
    uuid = 1:4,
    expenditure = c(500, 600, 700, NA),
    gender = c("male", "female", "male", NA),
    my_old_var = letters[26:23]
  )
  actual_v2 <- create_cleaning_log(me_raw_data,
    "uuid",
    me_clean_data,
    "uuid",
    check_for_deletion_log = T
  )

  testthat::expect_equal(actual_v2, expected_v2)




  ##
  # 11: deletion: in deleted log, not in clean data

  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:6),
    gender = rep(c("male", "female"), 3),
    expenditure = c(200, 300, 240, 44444, 300, 280),
    to_remove = NA_character_
  )

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:6),
    gender = rep(c("male", "female"), 3),
    expenditure = c(200, 300, 240, 44444, 300, 280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    uuid = character(),
    question_name = character(),
    change_type = character(),
    old_value = character(),
    new_value = character(),
    comment = character()
  )

  test_deletion_log <- tibble::tibble(
    uuid = c("uuid6", "uuid2"),
    change_type = c("remove_survey", "remove_survey"),
    comment = c("No matching uuid in the cleaned dataset", "c")
  )

  output <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_raw_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question_name",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
  )

  expected <- structure(
    list(
      uuid = c("uuid6", "uuid2"),
      df.question = c(NA_character_, NA_character_),
      df.change_type = c("remove_survey", "remove_survey"),
      df.new_value = c(NA_character_, NA_character_),
      cl.new_value = c(NA_character_, NA_character_),
      df.old_value = c(NA_character_, NA_character_),
      cl.old_value = c(NA_character_, NA_character_),
      comment = c(
        "This survey should be deleted from the clean dataset but it was not deleted",
        "This survey should be deleted from the clean dataset but it was not deleted"
      )
    ),
    row.names = c(NA, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  testthat::expect_equal(expected, output)
})




testthat::test_that("Test related to deletion and added surveys", {
  ## create cleaning log with no deletion survey
  me_raw_data <- data.frame(
    uuid = 1:5,
    expenditure = c(500, 600, 700, NA, NA),
    gender = c("male", "female", "male", NA, NA),
    my_old_var = letters[26:22]
  )


  me_clean_data <- data.frame(
    uuid = 1:5,
    expenditure = c(500, 666, NA, 800, NA),
    gender = c("male", "male", NA, "male", NA),
    my_new_var = letters[1:5]
  )

  testthat::expect_no_error(
    create_cleaning_log(
      raw_dataset = me_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = me_clean_data,
      clean_dataset_uuid_column = "uuid"
    )
  )


  ### test added survey
  me_clean_data <- data.frame(
    uuid = 1:6,
    expenditure = c(500, 666, NA, 800, NA, 12),
    gender = c("male", "male", NA, "male", NA, 56),
    my_new_var = letters[1:6]
  )

  testthat::expect_no_error(
    create_cleaning_log(
      raw_dataset = me_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = me_clean_data,
      clean_dataset_uuid_column = "uuid"
    )
  )


  ### [expect no error and no observation] check review cleaning log with added survey
  cleaning_log_test <- create_cleaning_log(
    raw_dataset = me_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = me_clean_data,
    check_for_variable_name = F,
    clean_dataset_uuid_column = "uuid",
    check_for_deletion_log = T
  )

  cl_test <-
    cleaning_log_test |> dplyr::filter(change_type != "remove_survey")
  dl_test <-
    cleaning_log_test |> dplyr::filter(change_type == "remove_survey")

  testthat::expect_equal(nrow(
    review_cleaning(
      raw_dataset = me_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = me_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = cl_test,
      cleaning_log_uuid_column = "uuid",
      cleaning_log_change_type_column = "change_type",
      cleaning_log_question_column = "question",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      deletion_log = dl_test,
      deletion_log_uuid_column = "uuid",
      cleaning_log_added_survey_value = "added_survey",
      check_for_deletion_log = T
    ) %>%
      suppressWarnings()
  ), 0)

  testthat::expect_no_error(
    review_cleaning(
      raw_dataset = me_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = me_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = cl_test,
      cleaning_log_uuid_column = "uuid",
      cleaning_log_change_type_column = "change_type",
      cleaning_log_question_column = "question",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      deletion_log = dl_test,
      deletion_log_uuid_column = "uuid",
      cleaning_log_added_survey_value = "added_survey",
      check_for_deletion_log = T
    ) %>%
      suppressWarnings()
  )

  ### check review cleaning log with added survey [expected - added_survey is missing in the cleaning log]
  cleaning_log_test_2 <-
    cleaning_log_test |> dplyr::filter(change_type != "added_survey")
  cl_test <-
    cleaning_log_test_2 |> dplyr::filter(change_type != "remove_survey")
  dl_test <-
    cleaning_log_test_2 |> dplyr::filter(change_type == "remove_survey")


  expected <- structure(
    list(
      uuid = "6",
      df.question = NA_character_,
      df.change_type = "added_survey",
      df.new_value = NA_character_,
      cl.new_value = NA_character_,
      df.old_value = NA_character_,
      cl.old_value = NA_character_,
      comment = "Survey missing in the raw data"
    ),
    row.names = c(NA, -1L),
    class = "data.frame"
  )

  actual <- review_cleaning(
    raw_dataset = me_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = me_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = cl_test,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = dl_test,
    deletion_log_uuid_column = "uuid",
    cleaning_log_added_survey_value = "added_survey",
    check_for_deletion_log = T
  ) %>%
    suppressWarnings()


  testthat::expect_equal(actual, expected)

  ### check review cleaning log with added survey [expected - added surveys are missing in the clean data]

  me_raw_data <- data.frame(
    uuid = 1:5,
    expenditure = c(500, 666, NA, 800, NA),
    gender = c("male", "male", NA, "male", NA),
    my_new_var = letters[1:5]
  )

  testthat::expect_equal(nrow(
    create_cleaning_log(
      raw_dataset = me_raw_data,
      raw_dataset_uuid_column = "uuid",
      check_for_deletion_log = F,
      clean_dataset = me_raw_data,
      clean_dataset_uuid_column = "uuid"
    )
  ), 0) ### expect nothing. comparing raw data with raw data


  # check if added survey is missing in the clean data
  cl_test <- structure(
    list(
      uuid = "6",
      question_name = NA_character_,
      change_type = "added_survey",
      new_value = NA_character_,
      old_value = NA_character_,
      comment = "Survey added to the clean dataset."
    ),
    class = "data.frame",
    row.names = "...1"
  )

  #
  actual <- review_cleaning(
    raw_dataset = me_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = me_raw_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = cl_test,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question_name",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    cleaning_log_added_survey_value = "added_survey",
    check_for_deletion_log = F
  )

  expected <- structure(
    list(
      uuid = "6",
      df.question = NA_character_,
      df.change_type = "added_survey",
      df.new_value = NA_character_,
      cl.new_value = NA_character_,
      df.old_value = NA_character_,
      cl.old_value = NA_character_,
      comment = "This survey should be added in the clean dataset but its missing now"
    ),
    row.names = c(
      NA,
      -1L
    ),
    class = "data.frame"
  )

  testthat::expect_equal(actual, expected)
})





#### check deletion log
testthat::test_that("Test related to deletion and added surveys", {
  ################ Start::11: deletion: in deleted log, not in clean data ####################

  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:6),
    gender = rep(c("male", "female"), 3),
    expenditure = c(200, 300, 240, 44444, 300, 280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid1", "uuid3"),
    question_name = c("gender", "expenditure"),
    change_type = c("change_response", "blank_response"),
    old_value = c("male", "240"),
    new_value = c("female", NA_character_),
    comment = c("An alteration was performed", "changed to NA")
  )

  test_deletion_log <- tibble::tibble(
    uuid = c("uuid6", "uuid2"),
    change_type = c("remove_survey", "remove_survey"),
    comment = c("No matching uuid in the cleaned dataset", "c")
  )

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c("female", "female", "male", "female", "male"),
    expenditure = c(200, 300, NA_real_, 44444, 300)
  )

  output <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question_name",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
  ) %>%
    suppressWarnings()
  testthat::expect_equal(
    output$comment,
    "This survey should be deleted from the clean dataset but it was not deleted"
  )
  ################ END::11: deletion: in deleted log, not in clean data ####################


  ################ Start::deletion: in raw data, not in clean data, not in deleted log ####################


  test_deletion_log <- tibble::tibble(
    uuid = c("uuid6"),
    change_type = c("remove_survey"),
    comment = c("No matching uuid in the cleaned dataset")
  )



  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:4),
    gender = c("female", "female", "male", "female"),
    expenditure = c(200, 300, NA_real_, 44444)
  )


  output2 <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question_name",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
  ) %>%
    suppressWarnings()

  testthat::expect_equal(
    output2$comment,
    "This survey was removed but currently missing in cleaning log"
  )


  ################ END::deletion: in raw data, not in clean data, not in deleted log ####################


  ################ Start::12: deletion: in deleted log, in clean data ####################

  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "male"),
    expenditure = c(200, 300, 240, 44444, 300),
    to_remove = NA_character_
  )

  ## added a new survey
  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:6),
    gender = c("female", "female", "male", "male", "female", "male"),
    expenditure = c(200, 300, NA_real_, 444, 88, 44)
  )

  test_deletion_log <- tibble::tibble(uuid = "uuid5")


  expected <- structure(
    list(
      uuid = c(
        "uuid5",
        "uuid6",
        "uuid4",
        "uuid5",
        "uuid4",
        "uuid5",
        "uuid6",
        "uuid6"
      ),
      df.question = c(
        NA,
        NA,
        "gender",
        "gender",
        "expenditure",
        "expenditure",
        "gender",
        "expenditure"
      ),
      df.change_type = c(
        "remove_survey",
        "added_survey",
        "change_response",
        "change_response",
        "change_response",
        "change_response",
        "change_response",
        "change_response"
      ),
      df.new_value = c(
        NA, NA, "male", "female",
        "444", "88", "male", "44"
      ),
      cl.new_value = c(
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
      ),
      df.old_value = c(
        NA, NA, "female", "male", "44444",
        "300", NA, NA
      ),
      cl.old_value = c(
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
      ),
      comment = c(
        "This survey should be deleted from the clean dataset but it was not deleted",
        "Survey missing in the raw data",
        "Entry missing in cleaning log",
        "Entry missing in cleaning log",
        "Entry missing in cleaning log",
        "Entry missing in cleaning log",
        "Entry missing in cleaning log",
        "Entry missing in cleaning log"
      )
    ),
    row.names = c(NA, -8L),
    class = c("tbl_df", "tbl", "data.frame")
  )


  output3 <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question_name",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
  ) %>%
    suppressWarnings()

  testthat::expect_equal(output3, expected)

  ################ END::12: deletion: in deleted log, in clean data ####################
  ## error msg check
  testthat::expect_error(
    review_cleaning(
      raw_dataset = test_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = test_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = test_cleaning_log,
      cleaning_log_uuid_column = "uuid",
      cleaning_log_change_type_column = "change_type",
      cleaning_log_question_column = "question_name",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      deletion_log_uuid_column = "uuid",
      check_for_deletion_log = T
    ),
    "Please provide deletion log"
  )

  testthat::expect_error(
    review_cleaning(
      raw_dataset = test_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = test_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = test_cleaning_log,
      cleaning_log_uuid_column = "uuid",
      cleaning_log_change_type_column = "change_type",
      cleaning_log_question_column = "question_name",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      deletion_log = test_deletion_log,
      deletion_log_uuid_column = "X_uuid",
      check_for_deletion_log = T
    ),
    "Deletion log uuid not found"
  )
})


#### Change not applied
testthat::test_that("Change not applied", {
  # 5: change not applied value to value,
  # 6: change not applied Na to value,
  # 7: changed not applied value to NA.


  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "male"),
    expenditure = c(NA_character_, 300, 240, 44444, 300)
  )

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "male"),
    expenditure = c(NA_character_, 300, 240, 444, 300)
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid5", "uuid4", "uuid1", "uuid5"),
    change_type = c(
      "change_response",
      "change_response",
      "change_response",
      "change_response"
    ),
    question = c("gender", "expenditure", "expenditure", "expenditure"),
    old_value = c("male", "44444", NA_character_, 300),
    new_value = c("female", "444", "250", NA_character_)
  )
  output4 <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    check_for_deletion_log = F
  )

  expected <- structure(
    list(
      uuid = c("uuid5", "uuid1", "uuid5"),
      df.question = c("gender", "expenditure", "expenditure"),
      df.change_type = c("change_response", "change_response", "change_response"),
      df.new_value = c("male", NA, "300"),
      cl.new_value = c("female", "250", NA),
      df.old_value = c("male", NA, "300"),
      cl.old_value = c("male", NA, "300"),
      comment = c(
        "Changes were not applied",
        "Changes were not applied",
        "Changes were not applied"
      )
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  testthat::expect_equal(output4, expected)
})



#### Change applied
testthat::test_that("Change applied", {
  # 2: change applied - value to value,
  # 3: change applied NA to value,
  # 4: change applied value to NA

  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "male"),
    expenditure = c(NA_character_, 300, NA_character_, 44444, 300)
  )


  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "female"),
    expenditure = c(NA_character_, 300, 240, NA_character_, 300)
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid5", "uuid3", "uuid4"),
    change_type = c("change_response", "change_response", "change_response"),
    question = c("gender", "expenditure", "expenditure"),
    old_value = c("male", NA_character_, "44444"),
    new_value = c("female", "240", NA_character_),
    comment = c(
      "change_applied-value to value",
      "change applied NA to value",
      "change applied value to NA"
    )
  )


  testthat::expect_equal(nrow(
    review_cleaning(
      raw_dataset = test_raw_data,
      raw_dataset_uuid_column = "uuid",
      clean_dataset = test_clean_data,
      clean_dataset_uuid_column = "uuid",
      cleaning_log = test_cleaning_log,
      cleaning_log_uuid_column = "uuid",
      cleaning_log_change_type_column = "change_type",
      cleaning_log_question_column = "question",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_old_value_column = "old_value",
      check_for_deletion_log = F
    ) %>%
      suppressWarnings()
  ), 0)
})



### Test entry both in deletion log and cleaning log

testthat::test_that("entry both in deletion log and cleaning log", {
  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "male"),
    expenditure = c(NA_character_, 300, NA_character_, 44444, 300)
  )


  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:4),
    gender = c(rep(c("male", "female"), 2)),
    expenditure = c(NA_character_, 300, 240, NA_character_)
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid5", "uuid3", "uuid4"),
    change_type = c("change_response", "change_response", "change_response"),
    question = c("gender", "expenditure", "expenditure"),
    old_value = c("male", NA_character_, "44444"),
    new_value = c("female", "240", NA_character_),
    comment = c(
      "change_applied-value to value/check deletion ",
      "change applied NA to value",
      "change applied value to NA"
    )
  )

  test_deletion_log <- tibble::tibble(uuid = "uuid5")


  output6 <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = T
  )
  testthat::expect_equal(
    output6$comment,
    "UUID found in deletion log. Please remove the entry."
  )
})


testthat::test_that("Test no change", {
  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c(rep(c("male", "female"), 2), "male"),
    expenditure = c(NA_character_, 300, NA_character_, 44444, 300)
  )

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid", 1:5),
    gender = c("female", "female", "male", "female", "male"),
    expenditure = c(NA_character_, 300, NA_character_, 44444, 300)
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid5", "uuid3", "uuid4", "uuid2", "uuid1"),
    change_type = c(
      "no_action",
      "no_change",
      "do_nothing",
      "no_action",
      "change_response"
    ),
    question = c("gender", "expenditure", "expenditure", "gender", "gender"),
    old_value = c("male", NA_character_, "44444", "female", "male"),
    new_value = c("female", "240", NA_character_, "female", "female"),
    comment = c(
      "No action with new value,old value value",
      "No action with new value, old value NA",
      "NO action VALUE TO NA",
      "No Issue",
      "Added change response"
    )
  )


  output7 <- review_cleaning(
    raw_dataset = test_raw_data,
    raw_dataset_uuid_column = "uuid",
    clean_dataset = test_clean_data,
    clean_dataset_uuid_column = "uuid",
    cleaning_log = test_cleaning_log,
    cleaning_log_uuid_column = "uuid",
    cleaning_log_no_change_value = c("no_action", "no_change", "do_nothing"),
    cleaning_log_change_type_column = "change_type",
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_old_value_column = "old_value",
    deletion_log = test_deletion_log,
    deletion_log_uuid_column = "uuid",
    check_for_deletion_log = F
  ) %>%
    suppressWarnings()

  expected <- structure(
    list(
      uuid = c("uuid5", "uuid3", "uuid4"),
      df.question = c("gender", "expenditure", "expenditure"),
      df.change_type = c("no_action", "no_change", "do_nothing"),
      df.new_value = c(NA_character_, NA_character_, NA_character_),
      cl.new_value = c("female", "240", NA),
      df.old_value = c(NA_character_, NA_character_, NA_character_),
      cl.old_value = c("male", NA, "44444"),
      comment = c(
        "No action with different value in new value column.",
        "No action with different value in new value column.",
        "No action with different value in new value column."
      )
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  testthat::expect_equal(output7, expected)
})


testthat::test_that("Numeric and boolean are equals after changing to text", {
  # numeric values
  numeric_raw_data <- data.frame(
    uuid = letters[1:6],
    score_x = c(1, 2, 3, NA, 5, 6)
  )
  numeric_clean_data <- numeric_raw_data %>%
    dplyr::mutate(score_x = c(1.5, 2, 3, NA, 5, 6))

  numeric_cleaning_log <- data.frame(
    uuid = "a",
    question = "score_x",
    change_type = "change_value",
    new_value = "1.5",
    old_value = "1"
  )

  numeric_expected_review_log <- data.frame(
    uuid = character(),
    df.question = character(),
    df.change_type = character(),
    df.new_value = character(),
    cl.new_value = character(),
    df.old_value = character(),
    cl.old_value = character(),
    comment = character()
  )

  numeric_actual_review_log <- review_cleaning(
    raw_dataset = numeric_raw_data,
    clean_dataset = numeric_clean_data,
    cleaning_log = numeric_cleaning_log,
    check_for_deletion_log = FALSE
  )
  expect_equal(numeric_actual_review_log, numeric_expected_review_log)

  # logical TRUE/FALSE to 1/0
  logical_test_raw_data <- data.frame(
    uuid = letters[1:6],
    variable.choicea = rep(c(TRUE, FALSE), 3)
  )
  logical_test_clean_data <- data.frame(
    uuid = letters[1:6],
    variable.choicea = c(0, 0, 1, 0, 1, 0)
  )

  logical_test_cleaning_log <- data.frame(
    uuid = "a",
    question = "variable.choicea",
    change_type = "change_value",
    new_value = "0",
    old_value = "1"
  )

  logical_expected_review_log <- data.frame(
    uuid = character(),
    df.question = character(),
    df.change_type = character(),
    df.new_value = character(),
    cl.new_value = character(),
    df.old_value = character(),
    cl.old_value = character(),
    comment = character()
  )

  logical_actual_review_log <- review_cleaning(
    raw_dataset = logical_test_raw_data,
    clean_dataset = logical_test_clean_data,
    cleaning_log = logical_test_cleaning_log,
    check_for_deletion_log = FALSE
  ) %>%
    suppressWarnings()
  expect_equal(logical_actual_review_log, logical_expected_review_log)

  # logical 1/0 to TRUE/FALSE
  logical_test_raw_data2 <- data.frame(
    uuid = letters[1:6],
    variable.choicea = rep(c(1, 0), 3)
  )
  logical_test_clean_data2 <- data.frame(
    uuid = letters[1:6],
    variable.choicea = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  logical_test_cleaning_log2 <- data.frame(
    uuid = "a",
    question = "variable.choicea",
    change_type = "change_value",
    new_value = "FALSE",
    old_value = "TRUE"
  )

  logical_expected_review_log2 <- data.frame(
    uuid = character(),
    df.question = character(),
    df.change_type = character(),
    df.new_value = character(),
    cl.new_value = character(),
    df.old_value = character(),
    cl.old_value = character(),
    comment = character()
  )

  logical_actual_review_log2 <- review_cleaning(
    raw_dataset = logical_test_raw_data2,
    clean_dataset = logical_test_clean_data2,
    cleaning_log = logical_test_cleaning_log2,
    check_for_deletion_log = FALSE
  ) %>%
    suppressWarnings()
  expect_equal(logical_actual_review_log2, logical_expected_review_log2)
})

test_that("TRUE/FALSE and 1/0 in the cleaning log will not be flagged as \"New value in cleaning log
and value in clean dataset not matching\" or \"Changes were not applied\"", {

  #from TRUE/FALSE to 1/0
  logical_test_raw_data <- data.frame(
    uuid = letters[1:6],
    variable.choicea = rep(c(T, F), 3)
  )
  logical_test_clean_data <- data.frame(
    uuid = letters[1:6],
    variable.choicea = c(0, 0, 1, 0, 1, 0)
  )

  logical_TF_test_cleaning_log <- data.frame(
    uuid = "a",
    question = "variable.choicea",
    change_type = "change_value",
    new_value = "FALSE",
    old_value = "TRUE"
  )

  logical_10_test_cleaning_log <- data.frame(
    uuid = "a",
    question = "variable.choicea",
    change_type = "change_value",
    new_value = "0",
    old_value = "1"
  )

  logical_expected_review_log <- data.frame(
    uuid = character(),
    df.question = character(),
    df.change_type = character(),
    df.new_value = character(),
    cl.new_value = character(),
    df.old_value = character(),
    cl.old_value = character(),
    comment = character()
  )

  logical_TF_actual_review_log <- review_cleaning(
    raw_dataset = logical_test_raw_data,
    clean_dataset = logical_test_clean_data,
    cleaning_log = logical_TF_test_cleaning_log,
    check_for_deletion_log = FALSE
  ) %>%
    suppressWarnings()
  expect_equal(logical_TF_actual_review_log, logical_expected_review_log)

  logical_10_actual_review_log <- review_cleaning(
    raw_dataset = logical_test_raw_data,
    clean_dataset = logical_test_clean_data,
    cleaning_log = logical_10_test_cleaning_log,
    check_for_deletion_log = FALSE
  ) %>%
    suppressWarnings()
  expect_equal(logical_10_actual_review_log, logical_expected_review_log)


  #from 1/0 to FALSE/TRUE

  logical_test_raw_data2 <- data.frame(
    uuid = letters[1:6],
    variable.choicea = rep(c(1, 0), 3)
  )
  logical_test_clean_data2 <- data.frame(
    uuid = letters[1:6],
    variable.choicea = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  logical_TF_test_cleaning_log2 <- data.frame(
    uuid = "a",
    question = "variable.choicea",
    change_type = "change_value",
    new_value = "0",
    old_value = "1"
  )

  logical_10_test_cleaning_log2 <- data.frame(
    uuid = "a",
    question = "variable.choicea",
    change_type = "change_value",
    new_value = "0",
    old_value = "1"
  )

  logical_expected_review_log2 <- data.frame(
    uuid = character(),
    df.question = character(),
    df.change_type = character(),
    df.new_value = character(),
    cl.new_value = character(),
    df.old_value = character(),
    cl.old_value = character(),
    comment = character()
  )

  logical_TF_actual_review_log2 <- review_cleaning(
    raw_dataset = logical_test_raw_data2,
    clean_dataset = logical_test_clean_data2,
    cleaning_log = logical_TF_test_cleaning_log2,
    check_for_deletion_log = FALSE
  ) %>%
    suppressWarnings()
  expect_equal(logical_TF_actual_review_log2, logical_expected_review_log2)

  logical_10_actual_review_log2 <- review_cleaning(
    raw_dataset = logical_test_raw_data2,
    clean_dataset = logical_test_clean_data2,
    cleaning_log = logical_10_test_cleaning_log2,
    check_for_deletion_log = FALSE
  ) %>%
    suppressWarnings()
  expect_equal(logical_10_actual_review_log2, logical_expected_review_log2)
})
