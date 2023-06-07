#add_percentage_missing

test_that("Test that if kobo is not in a correct format it will throw an error", {
  test_data <- data.frame(
    uuid = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7)
  ) %>% dplyr::rename(`_uuid` = uuid)

  kobo_survey <- data.frame(ttpe = c("text", "select_one"), namme = c("name", "admin1"))

  testthat::expect_error(
    add_percentage_missing(test_data, kobo_survey = kobo_survey),
    "Cannot identify type and/or name columns in kobo"
  )
  kobo_survey2 <- data.frame(type = c("text", "select_one"), namme = c("name", "admin1"))

  testthat::expect_error(
    add_percentage_missing(test_data, kobo_survey = kobo_survey2),
    "Cannot identify type and/or name columns in kobo"
  )

  kobo_survey3 <- data.frame(ttpe = c("text", "select_one"), name = c("name", "admin1"))
  testthat::expect_error(
    add_percentage_missing(test_data, kobo_survey = kobo_survey3),
    "Cannot identify type and/or name columns in kobo"
  )
})
# test that if the new names exists, it will stop
test_that("test that if the new names exists, it will stop", {
  test_data <- data.frame(uuid = c(1:3),
                          percentage_missing = c(.8,.8,.9))  %>% dplyr::rename(`_uuid` = uuid)
  testthat::expect_error(add_percentage_missing(test_data),
               "There is already a column called percentage_missing")
})

test_that("test that if the new names exists, it will stop", {
  test_data <- data.frame(uuid = c(1:3),
                          perc_missing = c(.8,.8,.9))  %>% dplyr::rename(`_uuid` = uuid)
  testthat::expect_error(add_percentage_missing(test_data, col_name = "perc_missing"),
               "There is already a column called perc_missing")

})

test_that("test that the columns are added with the correct values with a kobo", {
  data_test <- data.frame(uuid = c(1:3),
                          col_1 = c(1:3),
                          col_2 = c(NA, NA, "expenditures"),
                          col_3 = c("with need",NA, "with need"),

                          col_4 = c("food health school", NA, "food"),
                          col_4.food = c(1,NA,1),
                          col_4.health = c(1,NA,0),
                          col_4.school = c(1,NA,0))
  kobo_survey <- data.frame(type = c("uuid", "integer", "select_one choice2", "select_one choice3", "select_multiple choice4"),
                            name = c("uuid", "col_1", "col_2", "col_3", "col_4"))

  expected_results <- data.frame(uuid = c(1:3),
                                 col_1 = c(1:3),
                                 col_2 = c(NA, NA, "expenditures"),
                                 col_3 = c("with need",NA, "with need"),
                                 col_4 = c("food health school", NA, "food"),
                                 col_4.food = c(1,NA,1),
                                 col_4.health = c(1,NA,0),
                                 col_4.school = c(1,NA,0),
                                 percentage_missing = c(.25, .75,0))
  testthat::expect_equal(add_percentage_missing(data_test, kobo_survey = kobo_survey,
                                                type_to_include = c("integer","select_one","select_multiple")),
               expected_results)

  testthat::expect_error(add_percentage_missing(data_test, kobo_survey = kobo_survey,
                                                type_to_include = c("select_one","integger")))
  testthat::expect_no_error(add_percentage_missing(data_test))
})
test_that("test that the columns are added with the correct values without a kobo", {
  data_test <- data.frame(uuid = c(1:3),
                          col_1 = c(1:3),
                          col_2 = c(NA, NA, "expenditures"),
                          col_3 = c("with need",NA, "with need"),
                          col_4 = c("food health school", NA, "food"),
                          col_4.food = c(1,NA,1),
                          col_4.health = c(1,NA,0),
                          col_4.school = c(1,NA,0))

  expected_results <- data.frame(uuid = c(1:3),
                                 col_1 = c(1:3),
                                 col_2 = c(NA, NA, "expenditures"),
                                 col_3 = c("with need",NA, "with need"),

                                 col_4 = c("food health school", NA, "food"),
                                 col_4.food = c(1,NA,1),
                                 col_4.health = c(1,NA,0),
                                 col_4.school = c(1,NA,0),
                                 percentage_missing = c(1/8, 6/8,0))
  expect_equal(add_percentage_missing(data_test),
               expected_results)
})
test_that("it returns the dataframe with only 1 added column", {
  data_test <- data.frame(uuid = c(1:3),
                          col_1 = c(1:3),
                          col_2 = c(NA, NA, "expenditures"),
                          col_3 = c("with need",NA, "with need"),
                          col_4 = c("food health school", NA, "food"),
                          col_4.food = c(1,NA,1),
                          col_4.health = c(1,NA,0),
                          col_4.school = c(1,NA,0)) %>%
    dplyr::rename(`_uuid` = uuid)
  expect_equal(ncol(add_percentage_missing(data_test)),
               ncol(data_test) + 1)
})



# check_percentage_missing
test_that("check_percentage_missing returns the correct values", {
  no_flag_data_test <- data.frame(
    uuid = letters[1:3],
    col_1 = c(1:3),
    col_2 = c(NA, NA, "expenditures"),
    col_3 = c("with need", NA, "with need"),
    col_4 = c("food health school", NA, "food"),
    col_4.food = c(1, NA, 1),
    col_4.health = c(1, NA, 0),
    col_4.school = c(1, NA, 0),
    percentage_missing = c(.125, .750, 0)
  )

  no_flag_expected_output <- list(checked_dataset = no_flag_data_test,
                                  percentage_missing_log = dplyr::tibble(
                                    uuid = character(),
                                    issue = character(),
                                    question  = character(),
                                    old_value = character()
                                    ))

  expect_equal(check_percentage_missing(no_flag_data_test), no_flag_expected_output)

  data_test <- data.frame(
    uuid = letters,
    any_cols = LETTERS,
    any_number = 1:26,
    percentage_missing = c(rep(.05,25),.99)
  )

  expected_output <- list(checked_dataset = data_test,
                          percentage_missing_log = dplyr::tibble(
                            uuid = "z",
                            issue = "Percentages of missing values from this survey is different from others",
                            question  = "percentage_missing",
                            old_value = "0.99",
                            ))
  expect_equal(check_percentage_missing(data_test), expected_output)

  renamed_data_test <- data.frame(
    X_uuid = letters,
    any_cols = LETTERS,
    any_number = 1:26,
    blanks = c(rep(.05,25),.99)
  )

  renamed_expected_output <- list(checked_dataset = renamed_data_test,
                                  percentage_missing_log = dplyr::tibble(
                                    uuid = "z",
                                    issue = "Percentages of missing values from this survey is different from others",
                                    question  = "blanks",
                                    old_value = "0.99",
                                  ))

  expect_equal(check_percentage_missing(renamed_data_test, uuid_var = "X_uuid", .col_to_check = "blanks"), renamed_expected_output)

  renamed_log_expected_output <- list(checked_dataset = data_test,
                                      blanks_log = dplyr::tibble(
                                        uuid = "z",
                                        issue = "Percentages of missing values from this survey is different from others",
                                        question  = "percentage_missing",
                                        old_value = "0.99",
                                      ))
  expect_equal(check_percentage_missing(data_test, log_name = "blanks_log"), renamed_log_expected_output)

  list_test <- list(checked_dataset = data_test,
                    outlier_log = data.frame(uuid = "a",
                                             question = c("variableA"),
                                             old_value = c("1"),
                                             issue = c("possible outlier")))

  list_expected_output <- list(checked_dataset = data_test,
                               outlier_log = data.frame(uuid = "a",
                                                        question = c("variableA"),
                                                        old_value = c("1"),
                                                        issue = c("possible outlier")),
                               percentage_missing_log = dplyr::tibble(
                                 uuid = "z",
                                 issue = "Percentages of missing values from this survey is different from others",
                                 question  = "percentage_missing",
                                 old_value = "0.99",
                               ))

  expect_equal(check_percentage_missing(list_test), list_expected_output)

})


test_that("If column does not exist, return an error", {
  data_test <- data.frame(
    uuid = c(1:3),
    col_1 = c(1:3),
    col_2 = c(NA, NA, "expenditures"),
    col_3 = c("with need", NA, "with need"),
    col_4 = c("food health school", NA, "food"),
    col_4.food = c(1, NA, 1),
    col_4.health = c(1, NA, 0),
    col_4.school = c(1, NA, 0),
    percentage_missing = c(.125, .750, 0)
  )

  testthat::expect_error(
    check_percentage_missing(data_test, uuid_var = "X_uuid"),
    "Cannot find X_uuid in the names of the dataset"
  )

  testthat::expect_error(
    check_percentage_missing(data_test, .col_to_check = "blanks"),
    "Cannot find blanks in the names of the dataset"
  )
})

test_that("Check if there is no checked_dataset in a list, it will throws an error", {

  list_test <- list(dataset = data.frame(uuid = letters[1:6],
                                         percentage_missing = c(0.01,0.05,0.75)),
                    outlier_log = data.frame(uuid = "a",
                                          question = c("variableA"),
                                          old_value = c("1"),
                                          issue = c("possible outlier")))
  expect_error(check_percentage_missing(list_test), "Cannot identify the dataset in the list"
)
})

