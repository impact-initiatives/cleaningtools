testthat::test_that("Create Enumerator Performance", {
  raw_data <- cleaningtools_raw_data
  cleaning_log <- cleaningtools_cleaning_log

  testthat::expect_error(create_enumerator_performance(raw_data,cleaning_log, col_enum = "absd"))

  testthat::expect_equal(length(create_enumerator_performance(raw_data, cleaning_log, col_enum = "enumerator_num")), 5)

  ## TESTING First Table
  test_data_raw <- data.frame(enumerator_num = c(1,2,4,4,1,6,3,2,1,5))

  expected_output <- data.frame(enumerator_num = c(1,2,3,4,5,6),
                                Number = c(3,2,1,2,1,1))
  actual_ouput <- create_enumerator_performance(test_data_raw,cleaning_log, col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[1]],
                         expected_output)
  ## TESTING Second Table
  test_cleaning_log <- data.frame(enumerator_num = c(1,2,4,4,1,6,3,2,1,5),
                                  issue = c("duplicate","duplicate","outlier",
                                            "other_responses","test","test",
                                            "other_responses","duplicate","outlier","test"),
                                  change_type = c("remove_survey","remove_survey","no_action",
                                                  "change_response","remove_survey","remove_survey",
                                                  "change_resporse","no_action","change_response","remove_survey"))

  expected_output <- data.frame(enumerator_num = c(1,3,4),
                            Number = c(1,1,1))
  actual_ouput <- create_enumerator_performance(test_data_raw,test_cleaning_log, col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[2]],
                         expected_output)

  ## TESTING Third Table
  expected_output <- data.frame(enumerator_num = c(1,3,4),
                            issue = c("outlier","other_responses","other_responses"),
                            Number = c(1,1,1))
  actual_ouput <- create_enumerator_performance(test_data_raw,test_cleaning_log, col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[3]],
                         expected_output)

  ## TESTING fourth Table
  expected_output <- data.frame(enumerator_num = c(1,2,5,6),
                                Number = c(2,1,1,1))
  actual_ouput <- create_enumerator_performance(test_data_raw,test_cleaning_log, col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[4]],
                         expected_output)

  ## TESTING fifth Table
  expected_output <- data.frame(enumerator_num = c(1,1,2,5,6),
                                issue = c("duplicate","test","duplicate","test","test"),
                                Number = c(1,1,1,1,1))
  actual_ouput <- create_enumerator_performance(test_data_raw,test_cleaning_log, col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[5]],
                         expected_output)


})
