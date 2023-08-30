#####################
# create_audit_list #
#####################
test_that("if 100 audits, no dataset, return a list of 100.", {
  expected_results_1 <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_1.RDS"))
  expect_equal(
    create_audit_list(audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_100.zip")),
    expected_results_1
  )
})
test_that("if 92 audits, 1 dataset of 92 rows, 92 audits in the dataset, returns 92", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS"))
  expected_results_2 <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))

  expect_equal(
    create_audit_list(
      audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_92.zip"),
      uuid = "X_uuid",
      dataset = df_for_test
    ),
    expected_results_2
  )
})
test_that("if 100 audits, 1 dataset of 90 rows, 90 audits in the dataset, returns 90", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS")) %>%
    dplyr::sample_n(size = 90)

  expect_warning(
    results <- create_audit_list(
      audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_100.zip"),
      uuid = "X_uuid",
      dataset = df_for_test
    ),
    "10 audit files are found but not in the dataset. They won't be read."
  )

  expect_true(all(names(results) %in% df_for_test$X_uuid))
  expect_true(all(df_for_test$X_uuid %in% names(results)))
  expect_equal(length(results), nrow(df_for_test))
})

test_that("if 100 audits, 1 dataset of 110 rows, 92 audits in thee dataset, return 110 (92 + 18 missing)", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits_110.RDS"))

  create_audit_list(
    audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_100.zip"),
    uuid = "X_uuid",
    dataset = df_for_test
  ) %>%
    expect_warning("10 audit files are found but not in the dataset. They won't be read.") %>%
    expect_warning(
      "18 audit files were not found. They were added as empty dataframes."
    )

  results <- create_audit_list(
    audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_100.zip"),
    uuid = "X_uuid",
    dataset = df_for_test
  ) %>%
    suppressWarnings()

  expect_true(all(names(results) %in% df_for_test$X_uuid))
  expect_true(all(df_for_test$X_uuid %in% names(results)))
  expect_equal(length(results), nrow(df_for_test))
})


test_that("throw warnings if folder is different structure (i.e. one extra folder for 1 audit)", {
  expect_error(
    create_audit_list(audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_different_structure.zip")),
    "Please check the audit zip, some folders are not following the same structure."
  )
})

test_that("throw error if uuid in dataset not found", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits_110.RDS"))

  expect_error(
    create_audit_list(
      audit_zip_path = testthat::test_path("fixtures/audits/audit_for_tests_100.zip"),
      dataset = df_for_test
    ),
    "The variable uuid cannot be identified in the dataset provided."
  )
})

test_that("if zip folder start with attachement, return a list of 100.", {
  expected_results_1 <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_1.RDS"))
  expect_equal(
    create_audit_list(audit_zip_path = testthat::test_path("fixtures/audits/audit_for_test_attachement.zip")),
    expected_results_1
  )
})

###########################
# add_duration_from_audit #
###########################

test_that("add duration with start and end variable", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS")) %>%
    dplyr::select(X_uuid)
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))
  expected_outcomes <- readRDS(testthat::test_path("fixtures/audits/test_add_duration_audits_start_end.RDS"))

  results <- add_duration_from_audit(df_for_test,
    uuid = "X_uuid",
    audit_list = audit_list,
    start_question = "consent",
    end_question = "hh_preferred_channel_of_receiving_information",
    sum_all = FALSE
  )
  expect_equal(results, expected_outcomes)
})

test_that("add duration with the sum", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS")) %>%
    dplyr::select(X_uuid)
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))
  expected_outcomes <- readRDS(testthat::test_path("fixtures/audits/test_add_duration_audits_sum_all.RDS"))

  results <- add_duration_from_audit(df_for_test,
    uuid = "X_uuid",
    audit_list = audit_list
  )
  expect_equal(results, expected_outcomes)
})

test_that("add duration with sum all and from and to", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS")) %>%
    dplyr::select(X_uuid)
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))
  expected_outcomes <- readRDS(testthat::test_path("fixtures/audits/test_add_duration_audits_sum_all.RDS"))
  expected_outcomes2 <- readRDS(testthat::test_path("fixtures/audits/test_add_duration_audits_start_end.RDS"))
  expected_outcomes_final <- dplyr::left_join(expected_outcomes, expected_outcomes2, by = "X_uuid")

  results <- add_duration_from_audit(df_for_test,
    uuid = "X_uuid",
    audit_list = audit_list,
    start_question = "consent",
    end_question = "hh_preferred_channel_of_receiving_information",
    sum_all = TRUE
  )
  expect_equal(results, expected_outcomes_final)
})


test_that("test that if the new names exists, it will stop", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS")) %>%
    dplyr::select(X_uuid) %>%
    dplyr::mutate(
      duration_audit_sum_all_ms = NA_integer_,
      duration_audit_sum_all_minutes = NA_integer_
    )
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))


  expect_error(
    add_duration_from_audit(df_for_test,
      uuid = "X_uuid",
      audit_list = audit_list
    ),
    "duration_audit seems to be already used as name in your dataset."
  )
  df_for_test2 <- df_for_test %>%
    dplyr::rename(
      time_audit_sum_all_ms = duration_audit_sum_all_ms,
      time_audit_sum_all_minutes = duration_audit_sum_all_minutes
    )
  expect_error(
    add_duration_from_audit(df_for_test2,
      col_name_prefix = "time_audit",
      uuid = "X_uuid",
      audit_list = audit_list
    ),
    "time_audit seems to be already used as name in your dataset."
  )
})

test_that("test that dataframes in audit list has uuid as names", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS"))
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS")) %>%
    purrr::set_names(paste0("hello", names(.)))
  expect_error(
    add_duration_from_audit(df_for_test,
      uuid = "X_uuid",
      audit_list = audit_list
    ),
    "It seems no uuid are found as name of any data frame of audit list, make sure the data frame are saved with the uuid as name."
  )
})

test_that("test that when uuid variable name is not correct, it gives an error", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS"))
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))
  expect_error(
    add_duration_from_audit(df_for_test,
      audit_list = audit_list
    ),
    "uuid variable cannot be found in the dataset."
  )
})

test_that("test that all audits containts at least event, start, end, node", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS"))
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS")) %>%
    purrr::map(~ dplyr::select(.x, -node))
  expect_error(
    add_duration_from_audit(df_for_test,
      audit_list = audit_list,
      uuid_column = "X_uuid"
    ),
    "Some columns are missing in the audits, please make sure to have at least event, node, start, end"
  )
})

test_that("test if start_question or end_question is missing, it gives an error", {
  df_for_test <- readRDS(testthat::test_path("fixtures/audits/test_df_audits.RDS"))
  audit_list <- readRDS(testthat::test_path("fixtures/audits/test_create_audit_list_2.RDS"))
  expect_error(
    add_duration_from_audit(df_for_test,
      uuid = "X_uuid",
      audit_list = audit_list,
      end_question = "hh_preferred_channel_of_receiving_information",
      sum_all = FALSE
    ),
    "start_question is missing"
  )
  expect_error(
    add_duration_from_audit(df_for_test,
      uuid = "X_uuid",
      audit_list = audit_list,
      start_question = "consent",
      sum_all = FALSE
    ),
    "end_question is missing"
  )
})

##################
# check_duration #
##################

test_that("4 outside of boundaries, return 4 flagged", {
  testdata <- data.frame(
    uuid = c(letters[1:7]),
    duration_audit_start_end_ms = c(
      2475353, 375491, 2654267, 311585, 817270,
      2789505, 8642007
    ),
    duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
  )
  expected_results <- list(
    checked_dataset = testdata,
    duration_log = data.frame(
      uuid = c("b", "d", "e", "g"),
      old_value = c(6, 5, 14, 144),
      question = rep("duration_audit_start_end_minutes", 4),
      issue = rep("Duration is lower or higher than the thresholds", 4)
    )
  )
  expect_equal(check_duration(testdata, "duration_audit_start_end_minutes"), expected_results)
})

test_that("no outside of boundaries, no flagged", {
  testdata <- data.frame(
    uuid = c(letters[1:7]),
    duration_audit_start_end_ms = c(
      2475353, 375491, 2654267, 311585, 817270,
      2789505, 8642007
    ),
    duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
  )

  expected_results <- list(
    checked_dataset = testdata,
    duration_log = data.frame(
      uuid = character(),
      old_value = double(),
      question = character(),
      issue = character()
    )
  )
  expect_equal(
    check_duration(testdata,
      "duration_audit_start_end_minutes",
      lower_bound = 3,
      higher_bound = 150
    ),
    expected_results
  )
})

test_that("no outside of boundaries, no flagged - names different than uuid", {
  testdata <- data.frame(
    uuid = c(letters[1:7]),
    duration_audit_start_end_ms = c(
      2475353, 375491, 2654267, 311585, 817270,
      2789505, 8642007
    ),
    duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
  )
  expected_results <- list(
    checked_dataset = testdata,
    duration_log = data.frame(
      uuid = character(),
      old_value = double(),
      question = character(),
      issue = character()
    )
  )
  expect_equal(
    check_duration(testdata,
      column_to_check = "duration_audit_start_end_minutes",
      uuid_column = "uuid",
      lower_bound = 3,
      higher_bound = 170
    ),
    expected_results
  )
})

test_that("4 outside of boundaries, return 4 flagged - names different than uuid", {
  testdata <- data.frame(
    uuid = c(letters[1:7]),
    duration_audit_start_end_ms = c(
      2475353, 375491, 2654267, 311585, 817270,
      2789505, 8642007
    ),
    duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
  )
  expected_results <- list(
    checked_dataset = testdata,
    duration_log = data.frame(
      uuid = c("b", "d", "e", "g"),
      old_value = c(6, 5, 14, 144),
      question = rep("duration_audit_start_end_minutes", 4),
      issue = rep("Duration is lower or higher than the thresholds", 4)
    )
  )
  expect_equal(
    check_duration(testdata,
      column_to_check = "duration_audit_start_end_minutes",
      uuid_column = "uuid"
    ),
    expected_results
  )
})

test_that("Adds to the list if there is already a check.", {
  test_list <- list(
    checked_dataset = data.frame(
      uuid = c(letters[1:7]),
      duration_audit_start_end_ms = c(
        2475353, 375491, 2654267, 311585, 817270,
        2789505, 8642007
      ),
      duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
    ),
    outlier_log = data.frame(
      uuid = NULL,
      old_value = NULL,
      question = NULL,
      issue = NULL
    )
  )

  expected_results <- list(
    checked_dataset = test_list$checked_dataset,
    outlier_log = data.frame(
      uuid = NULL,
      old_value = NULL,
      question = NULL,
      issue = NULL
    ),
    duration_log = data.frame(
      uuid = c("b", "d", "e", "g"),
      old_value = c(6, 5, 14, 144),
      question = rep("duration_audit_start_end_minutes", 4),
      issue = rep("Duration is lower or higher than the thresholds", 4)
    )
  )
  expect_equal(
    check_duration(test_list,
      column_to_check = "duration_audit_start_end_minutes"
    ),
    expected_results
  )
})

test_that("Check that the list has an object called checked_dataset", {
  test_list <- list(
    dataset = data.frame(
      uuid = c(letters[1:4], "a", "b", "c"),
      col_a = runif(7),
      col_b = runif(7)
    ),
    outlier_log = data.frame(
      uuid = NULL,
      old_value = NULL,
      question = NULL,
      issue = NULL
    )
  )
  testthat::expect_error(
    check_duration(test_list),
    "Cannot identify the dataset in the list"
  )
})

test_that("If column does not exist, return an error", {
  testdata <- data.frame(
    uuid = c(letters[1:7]),
    duration_audit_start_end_ms = c(
      2475353, 375491, 2654267, 311585, 817270,
      2789505, 8642007
    ),
    duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
  )
  testthat::expect_error(
    check_duration(testdata, "duration_audit_sum_all_minutes"),
    "Cannot find duration_audit_sum_all_minutes in the names of the dataset"
  )
})
