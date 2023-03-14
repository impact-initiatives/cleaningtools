#add_percentage_missing

# test that inputs is correct -> dataset and kobo?
test_that("Test that if input is not dataset, nor a list with checked_dataset it makes some errors", {
  test_list <- list(
    dataset = data.frame(
      `_uuid` = c(letters[1:4], "a", "b", "c"),
      col_a = runif(7),
      col_b = runif(7)
    ),
    outlier_log = data.frame(
      uuid = NULL,
      value = NULL,
      variable = NULL,
      issue = NULL
    )
  )
  testthat::expect_error(
    add_percentage_missing(test_list),
    "Cannot identify the dataset in the list"
  )
})

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
  expect_error(add_percentage_missing(test_data),
               "There is already a column called percentage_missing")
})

test_that("test that if the new names exists, it will stop", {
  test_data <- data.frame(uuid = c(1:3),
                          perc_missing = c(.8,.8,.9))  %>% dplyr::rename(`_uuid` = uuid)
  expect_error(add_percentage_missing(test_data, col_name = "perc_missing"),
               "There is already a column called perc_missing")

})

test_that("test that if uuid column does not exist, it will stop", {
  test_data <- data.frame(uuid = c(1:3))
  expect_error(add_percentage_missing(test_data),
               "Cannot find _uuid in the names of the dataset")

  test_data2 <- data.frame(id = c(1:3))
  expect_error(add_percentage_missing(test_data, uuid_col = "X_uuid"),
               "Cannot find X_uuid in the names of the dataset")
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
  expect_equal(add_percentage_missing(data_test, kobo_survey = kobo_survey, uuid_col = "uuid"),
               expected_results)
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
  expect_equal(add_percentage_missing(data_test, uuid_col = "uuid"),
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

###checks that the denominator is equal to the number of variables.


#check_percentage_missing
# test that inputs is correct
# test that if there are 3 values to flag log is returned with 3 values flagged
# test that if there are no values to flag, the log returned is empty
# test that if works with a specific name
# "If column does not exist, return an error"
# Adds to the list if there is already a check
# "Check that the list has an object called checked_dataset"



