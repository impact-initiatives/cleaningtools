testthat::test_that("Error checks", {
  testthat::expect_error(check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_sample_frame,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid"
  ))
  testthat::expect_error(check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "uuid"
  ))
  t
  testthat::expect_no_error(check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid", idnk_value = "dont_know"
  ))
})

testthat::test_that("Exact equals", {
  actual_output <- check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid", idnk_value = "dont_know",
    return_all_results = TRUE
  )[[2]] %>% head()

  expected_output <- data.frame(
    uuid = c(
      "3370f726-395a-4675-94fe-9e745e0b36e9",
      "93095da3-5291-4d16-a19a-41bf13144bfe",
      "db5e05db-94e9-44aa-9206-3e1c17a7a233",
      "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2",
      "0858486a-1d3d-492b-863f-b050cb9fe7af",
      "193d5f36-93b9-4c97-9205-13aa7e3a6c7f"
    ),
    num_cols_not_NA = c(75, 85, 85, 75, 74, 75),
    total_columns_compared = c(148, 148, 148, 148, 148, 148),
    num_cols_dont_know = c(0, 0, 0, 0, 0, 0),
    id_most_similar_survey = c(
      "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2",
      "db5e05db-94e9-44aa-9206-3e1c17a7a233",
      "93095da3-5291-4d16-a19a-41bf13144bfe",
      "3370f726-395a-4675-94fe-9e745e0b36e9",
      "93893e39-9c82-4e19-b480-4dc78033157b",
      "ac42b381-4d3b-42b4-96fb-676d43a8c4e7"
    ),
    number_different_columns = c(9, 9, 9, 9, 10, 10),
    issue = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("Check all uuids", {
  actual_output <- check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid", idnk_value = "dont_know",
    return_all_results = TRUE
  )[[2]] %>%
    dplyr::arrange(uuid) %>%
    dplyr::select(uuid) %>%
    dplyr::pull()
  expected_output <- cleaningtools::cleaningtools_raw_data %>%
    dplyr::arrange(X_uuid) %>%
    dplyr::select(X_uuid) %>%
    dplyr::pull()
  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("Returns a list of 2 dataframe", {
  actual_output <- check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid", idnk_value = "dont_know"
  )
  testthat::expect_equal(length(actual_output), 2)
  testthat::expect_equal(names(actual_output), c("checked_dataset", "soft_duplicate_log"))
})


testthat::test_that("Test the argument return_all_results filters correctly", {
  # default threshold
  treshold_7_actual_output <- check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid", idnk_value = "dont_know",
    return_all_results = FALSE,
  )[[2]]

  treshold_7_expected_output <- data.frame(
    uuid = character(),
    issue = character()
  )

  testthat::expect_equal(treshold_7_actual_output, treshold_7_expected_output)

  # treshold set to 9
  treshold_9_actual_output <- check_soft_duplicates(
    dataset = cleaningtools::cleaningtools_raw_data,
    kobo_survey = cleaningtools::cleaningtools_survey,
    uuid_column = "X_uuid", idnk_value = "dont_know",
    threshold = 9,
    return_all_results = FALSE,
  )[[2]]


  treshold_9_expected_output <- data.frame(
    uuid = c(
      "3370f726-395a-4675-94fe-9e745e0b36e9",
      "93095da3-5291-4d16-a19a-41bf13144bfe",
      "db5e05db-94e9-44aa-9206-3e1c17a7a233",
      "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2"
    ),
    num_cols_not_NA = c(75, 85, 85, 75),
    total_columns_compared = c(148, 148, 148, 148),
    num_cols_dont_know = c(0, 0, 0, 0),
    id_most_similar_survey = c(
      "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2",
      "db5e05db-94e9-44aa-9206-3e1c17a7a233",
      "93095da3-5291-4d16-a19a-41bf13144bfe",
      "3370f726-395a-4675-94fe-9e745e0b36e9"
    ),
    number_different_columns = c(9, 9, 9, 9),
    issue = rep("Less than 9 differents options", 4)
  )
  testthat::expect_equal(treshold_9_actual_output, treshold_9_expected_output)
})
