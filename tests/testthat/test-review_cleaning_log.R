library(dplyr)
library(testthat)

# creating and reviewing cleaning log ---------------------------------------------------

testthat::test_that("Checking with test data", {

  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid",1:6),
    gender = rep(c("male","female"),3),
    expenditure= c(200,300,240,44444,300,280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid1","uuid3"),
    question_name = c("gender","expenditure"),
    change_type = c("change_response","blank_response"),
    old_value = c("male","240"),
    new_value = c("female",NA_character_),
    comment = c("An alteration was performed","changed to NA")
  )

  test_deletion_log <- tibble::tibble(
    uuid = "uuid6",
    change_type = "remove_survey",
    comment = "No matching uuid in the cleaned dataset")

  expected_cleaning_log <- test_deletion_log |> dplyr::bind_rows(test_cleaning_log) |> as.data.frame()

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid",1:5),
    gender = c("female","female","male","female","male"),
    expenditure= c(200,300,NA_real_,44444,300)
    )

  cleaing_log_actual <- create_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "uuid",
                      clean_data = test_clean_data,clean_data_uuid = "uuid",
                      check_for_deletion_log = T,
                      check_for_variable_name = F) |> dplyr::select(dplyr::all_of(names(expected_cleaning_log)))
  ### check create cleaning log
  testthat::expect_equal(cleaing_log_actual,expected_cleaning_log)

  ## check review cleaning log

  testthat::expect_equal(nrow(review_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "uuid",
                                                  clean_data = test_clean_data,clean_data_uuid = "uuid",
                      cleaning_log = test_cleaning_log,cleaning_log_uuid = "uuid",
                      cleaning_log_question_name = "question_name",cleaning_log_new_value = "new_value",
                      cleaning_log_old_value = "old_value",deletion_log = test_deletion_log,deletion_log_uuid = "uuid",
                      check_for_deletion_log =T)),0)

  test_clean_data_faulty <- test_clean_data
  test_clean_data_faulty[4,"expenditure"] <- 444
  test_clean_data_faulty[1,"gender"] <- "male"

 actual <-  review_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "uuid",
                      clean_data = test_clean_data_faulty,
                      clean_data_uuid = "uuid",
                      cleaning_log = test_cleaning_log,
                      cleaning_log_uuid = "uuid",
                      cleaning_log_question_name = "question_name",
                      cleaning_log_new_value = "new_value",
                      cleaning_log_old_value = "old_value",
                      deletion_log = test_deletion_log,
                      deletion_log_uuid = "uuid",
                      check_for_deletion_log =T) |> as.data.frame()


  expected_output <- data.frame(
    uuid = c("uuid1","uuid4"),
    df.question_name = c("gender","expenditure"),
    df.change_type = c("change_response","change_response"),
    df.new_value = c("male","444"),
    cl.new_value = c("female",NA_character_),
    df.old_value = c("male","44444"),
    cl.old_value = c("male",NA_character_),
    comment = c("Changes were not applied","Entry missing in cleaning log")
    )

  testthat::expect_equal(actual,expected_output)



  ########## check with different uuid name/no issue

  test_raw_data <- tibble::tibble(
    X_uuid = paste0("uuid",1:6),
    gender = rep(c("male","female"),3),
    expenditure= c(200,300,240,44444,300,280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    `_uuid`= c("uuid1","uuid3"),
    question_name = c("gender","expenditure"),
    change_type_col = c("change_response","blank_response"),
    old_value = c("male","240"),
    new.value = c("female",NA_character_),
    comment = c("An alteration was performed","changed to NA")
  )

  test_deletion_log <- tibble::tibble(
    `_uuid` = "uuid6",
    change_type_col = "remove_survey",
    comment = "No matching uuid in the cleaned dataset")

  expected_cleaning_log <- test_deletion_log |> dplyr::bind_rows(test_cleaning_log) |> as.data.frame() |> dplyr::rename(
    uuid = `_uuid`,
    change_type = change_type_col,
    new_value =new.value
  )

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid",1:5),
    gender = c("female","female","male","female","male"),
    expenditure= c(200,300,NA_real_,44444,300)
  )

  cleaing_log_actual <- create_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "X_uuid",
                                            clean_data = test_clean_data,clean_data_uuid = "uuid",
                                            check_for_deletion_log = T,
                                            check_for_variable_name = F) |> dplyr::select(dplyr::all_of(names(expected_cleaning_log)))
  ### check create cleaning log
  testthat::expect_equal(cleaing_log_actual,expected_cleaning_log)

  ## check review cleaning log

  testthat::expect_equal(nrow(review_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "X_uuid",
                                                  clean_data = test_clean_data,clean_data_uuid = "uuid",
                                                  cleaning_log = test_cleaning_log,
                                                  cleaning_log_uuid = "_uuid",
                                                  cleaning_log_question_name = "question_name",
                                                  cleaning_log_change_type_column = "change_type_col",
                                                  cleaning_log_new_value = "new.value",
                                                  cleaning_log_old_value = "old_value",
                                                  deletion_log = test_deletion_log,
                                                  deletion_log_uuid = "_uuid",
                                                  check_for_deletion_log =T)),0)



  ########## check with different uuid name/with issue

  test_clean_data_faulty <- test_clean_data
  test_clean_data_faulty[4,"expenditure"] <- 444
  test_clean_data_faulty[1,"gender"] <- "male"
  test_cleaning_log <- test_cleaning_log |> dplyr::rename(
    question = question_name
  )

  actual_output <- review_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "X_uuid",
                      clean_data = test_clean_data_faulty,
                      clean_data_uuid = "uuid",
                      cleaning_log = test_cleaning_log,
                      cleaning_log_uuid = "_uuid",
                      cleaning_log_change_type_column = "change_type_col",
                      cleaning_log_question_name = "question",
                      cleaning_log_new_value = "new.value",
                      cleaning_log_old_value = "old_value",
                      deletion_log = test_deletion_log,
                      deletion_log_uuid = "_uuid",
                      check_for_deletion_log =T) |> as.data.frame()
  expected_output <- data.frame(
    uuid = c("uuid1","uuid4"),
    df.question_name = c("gender","expenditure"),
    df.change_type = c("change_response","change_response"),
    df.new_value = c("male","444"),
    cl.new_value = c("female",NA_character_),
    df.old_value = c("male","44444"),
    cl.old_value = c("male",NA_character_),
    comment = c("Changes were not applied","Entry missing in cleaning log")

  )

  testthat::expect_equal(actual_output,expected_output)



  })










testthat::test_that("Checking with real data", {
  expect_no_error(create_cleaning_log(
    raw_data = cleaningtools::raw_data, raw_data_uuid = "X_uuid",
    clean_data = cleaningtools::clean_data, clean_data_uuid = "X_uuid",
    check_for_deletion_log = T, check_for_variable_name = T
  ))

  deletaion_log <- cleaningtools::cleaning_log |> dplyr::filter(change_type == "remove_survey")
  cleaning_log2 <- cleaningtools::cleaning_log |> dplyr::filter(change_type != "remove_survey")

  expect_no_error(review_cleaning_log(raw_data = cleaningtools::raw_data,raw_data_uuid = "X_uuid",
                                           clean_data = cleaningtools::clean_data,clean_data_uuid = "X_uuid",
                                           cleaning_log = cleaning_log2,cleaning_log_uuid = "X_uuid",
                                           cleaning_log_question_name = "questions",
                                           cleaning_log_new_value =  "new_value",
                                           cleaning_log_old_value = "old_value",
                                           deletion_log = deletaion_log,
                                           deletion_log_uuid = "X_uuid",
                                           check_for_deletion_log =T))


  #### check deletion log

  test_raw_data <- tibble::tibble(
    uuid = paste0("uuid",1:6),
    gender = rep(c("male","female"),3),
    expenditure= c(200,300,240,44444,300,280),
    to_remove = NA_character_
  )

  test_cleaning_log <- tibble::tibble(
    uuid = c("uuid1","uuid3"),
    question_name = c("gender","expenditure"),
    change_type = c("change_response","blank_response"),
    old_value = c("male","240"),
    new_value = c("female",NA_character_),
    comment = c("An alteration was performed","changed to NA")
  )

  test_deletion_log <- tibble::tibble(
    uuid = c("uuid6","uuid2"),
    change_type = c("remove_survey","remove_survey"),
    comment = c("No matching uuid in the cleaned dataset","c"))

  expected_cleaning_log <- test_deletion_log |> dplyr::bind_rows(test_cleaning_log) |> as.data.frame()

  test_clean_data <- tibble::tibble(
    uuid = paste0("uuid",1:5),
    gender = c("female","female","male","female","male"),
    expenditure= c(200,300,NA_real_,44444,300)
  )

  output <- review_cleaning_log(raw_data = test_raw_data,raw_data_uuid = "uuid",
                      clean_data = test_clean_data,
                      clean_data_uuid = "uuid",
                      cleaning_log = test_cleaning_log,
                      cleaning_log_uuid = "uuid",
                      cleaning_log_change_type_column = "change_type",
                      cleaning_log_question_name = "question_name",
                      cleaning_log_new_value = "new_value",
                      cleaning_log_old_value = "old_value",
                      deletion_log = test_deletion_log,
                      deletion_log_uuid = "uuid",
                      check_for_deletion_log =T)
  testthat::expect_equal(output$comment, "This survey should be deleted from the clean dataset but it was not deleted")

})

