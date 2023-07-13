testthat::test_that("Create Cleaning Logbook", {
  raw_data <- cleaningtools_raw_data
  cleaning_log <- cleaningtools_cleaning_log

  testthat::expect_error(create_cleaning_logbook(raw_data,cleaning_log,uuid = "X_uuid", col_enum = "absd"))
  testthat::expect_equal(length(create_cleaning_logbook(raw_data,cleaning_log,uuid = "X_uuid", col_enum = "enumerator_num")), 3)

## TESTING First Table
test_data_raw <- data.frame(X_uuid = c("abcd","eeds","eedt","sdks"),
                            deviceid = c(1,2,3,4),
                            enumerator_num = c(1,2,4,4))

expected_output <- data.frame(uuid = c("abcd","eeds","eedt","sdks"),
                              `enumerator ID` = c(1,2,4,4)) %>%
  dplyr::rename("enumerator ID" = enumerator.ID)
actual_ouput <- create_cleaning_logbook(test_data_raw,cleaning_log,uuid = "X_uuid", col_enum = "enumerator_num")

testthat::expect_equal(actual_ouput[[1]],
                       expected_output)

  ## TESTING Second Table
  test_cleaning_log <- data.frame(X_uuid = c("abcd","eeds","eedt","sdks"),
                                  enumerator_num = c(1,2,4,4),
                                  questions = c("q1","q2","q3","q3"),
                                  reason = c("f1","f2","f4","f3"),
                                  issue = c("duplicate","duplicate","outlier",
                                            "other_responses"),
                                  change_type = c("remove_survey","remove_survey","no_action",
                                                  "change_response"),
                                  old_value= c(1,1,1,1), new_value = c(2,2,2,2))

  expected_output <- data.frame(uuid = c("eedt","sdks"),
                                `Enumerator ID` = c(4,4),
                                `device ID` = c(3,4),
                                question.name = c("q3","q3"),
                                issue = c("outlier","other_responses"),
                                `Type of Issue (Select from dropdown list)` = c(NA,NA),
                                feedback = c("f4","f3"),
                                changed = c("No","Yes"),
                                old.value = c(1,1), new.value = c(2,2)) %>%
    dplyr::rename("Enumerator ID" = Enumerator.ID,
                  "device ID" = device.ID,
                  "Type of Issue (Select from dropdown list)" = Type.of.Issue..Select.from.dropdown.list.)

  actual_ouput <- create_cleaning_logbook(test_data_raw,test_cleaning_log,uuid = "X_uuid", col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[2]],
                         expected_output)

  ## TESTING Third Table
  expected_output <- data.frame(uuid = c("abcd","eeds"),
                                `Enumerator ID` = c(1,2),
                                `device ID` = c(1,2),
                                Issue = c("duplicate","duplicate"),
                                `Type of Issue (Select from dropdown list)` = c(NA,NA),
                                feedback = c("f1","f2")) %>%
    dplyr::rename("Enumerator ID" = Enumerator.ID,
                  "device ID" = device.ID,
                  "Type of Issue (Select from dropdown list)" = Type.of.Issue..Select.from.dropdown.list.)
  actual_ouput <- create_cleaning_logbook(test_data_raw,test_cleaning_log, col_enum = "enumerator_num")

  testthat::expect_equal(actual_ouput[[3]],
                         expected_output)


})
