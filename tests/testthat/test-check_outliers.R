test_that("Outliers check", {
  ### test cols to add

  outliers1 <- check_outliers(dataset = cleaningtools_raw_data, uuid_column = "X_uuid", columns_not_to_check = "enumerator_num", strongness_factor = 1.5)
  outliers2 <- check_outliers(cleaningtools_raw_data, uuid_column = "X_uuid", strongness_factor = 1.5)

  expect_false("enumerator_num" %in% outliers1$potential_outliers$question)
  expect_true("uuid" %in% names(outliers1$potential_outliers))
  expect_true("enumerator_num" %in% outliers2$potential_outliers$question)

  ## test with null in unique number
  outliers3 <- check_outliers(cleaningtools_raw_data, uuid_column = "X_uuid", minimum_unique_value_of_variable = 10)
  outliers4 <- check_outliers(cleaningtools_raw_data, uuid_column = "X_uuid", minimum_unique_value_of_variable = NULL)

  expect_false("air_coolers_nb" %in% outliers3$potential_outliers$question)
  expect_true("air_coolers_nb" %in% outliers4$potential_outliers$question)


  df_outlier <- data.frame(
    uuid = paste0("uuid_", sample(1:1000, size = 100)),
    age = c(sample(1:75, replace = T, size = 95), c(200, 1, 5, 0, 150)),
    expense = c(sample(200:500, replace = T, size = 95), c(600, 100, 0, 1020, 1050)),
    income = c(sample(20000:50000, replace = T, size = 95), c(60, 0, 80, 1020, 1050))
  )

  outliers5 <- check_outliers(dataset = df_outlier, uuid_column = "uuid", strongness_factor = 1.5)



  outliers6 <- check_outliers(dataset = df_outlier, uuid_column = "uuid", strongness_factor = 3)

  expect_true(nrow(outliers5$potential_outliers) > nrow(outliers6$potential_outliers))


  df_list <- list(df_outlier = df_outlier)
  expect_no_error(check_outliers(dataset = df_list, uuid_column = "uuid", element_name = "df_outlier"))
  expect_length(check_outliers(dataset = df_list, uuid_column = "uuid", element_name = "df_outlier"), 2)
})




test_that("outlier_check returns correct results", {
  set.seed(12333)
  df_outlier <- data.frame(
    uuid = paste0("uuid_", 1:100),
    one_value = c(round(runif(90, min = 45, max = 55)), round(runif(5)), round(runif(5, 99, 100))),
    expense = c(sample(200:500, replace = T, size = 95), c(600, 100, 80, 1020, 1050)),
    income = c(c(60, 0, 80, 1020, 1050), sample(20000:50000, replace = T, size = 95)),
    yy = c(rep(100, 99), 10)
  )

  one_value_outlier <- data.frame(
    uuid = paste0("uuid_", c(91:100)),
    issue = "outlier (normal distribution)",
    question = "one_value",
    old_value = df_outlier[91:100, "one_value"]
  )
  expense_outlier <- data.frame(
    uuid = c(paste0("uuid_", c(99:100)), paste0("uuid_", c(97:98))),
    issue = c("outlier (normal distribution)", "outlier (normal distribution)", "outlier (log distribution)", "outlier (log distribution)"),
    question = "expense",
    old_value = c(1020, 1050, 100, 80)
  )

  income_outlier <- data.frame(
    uuid = paste0("uuid_", c(1, 2, 3)),
    issue = "outlier (log distribution)",
    question = "income",
    old_value = c(60, 0, 80)
  )


  yy_outlier <- data.frame(
    uuid = paste0("uuid_", 100),
    issue = "outlier (normal distribution)",
    question = "yy",
    old_value = 10
  )

  expected_outcome <- rbind(one_value_outlier, expense_outlier, income_outlier, yy_outlier)


  outliers_xx <- check_outliers(dataset = df_outlier, uuid_column = "uuid", strongness_factor = 3)
  expect_equal(outliers_xx$potential_outliers, expected_outcome, ignore_attr = TRUE)
})



test_that("Outliers check with kobo", {
  expect_warning(check_outliers(
    dataset = cleaningtools_raw_data, uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    kobo_choices = NULL, strongness_factor = 3
  ))
  expect_warning(check_outliers(
    dataset = cleaningtools_raw_data, uuid_column = "X_uuid", kobo_survey = NULL,
    kobo_choices = cleaningtools_choices, strongness_factor = 3
  ))
  expect_no_warning(check_outliers(
    dataset = cleaningtools_raw_data, uuid_column = "X_uuid", kobo_survey = NULL,
    kobo_choices = NULL, strongness_factor = 3
  ))


  a <- cleaningtools_raw_data |> dplyr::mutate_if(is.logical, as.integer)
  outliers_xx <- check_outliers(
    dataset = a, uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    kobo_choices = cleaningtools_choices, strongness_factor = 3,
    remove_choice_multiple = F
  )
  outliers_yy <- check_outliers(
    dataset = a, uuid_column = "X_uuid", strongness_factor = 3,
    remove_choice_multiple = T
  )

  testthat::expect_true("primary_livelihood.support" %in% outliers_xx$potential_outliers$question)
  testthat::expect_false("primary_livelihood.support" %in% outliers_yy$potential_outliers$question)
})
