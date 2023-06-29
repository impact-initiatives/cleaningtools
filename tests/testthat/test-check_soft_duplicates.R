testthat::test_that("Error checks", {
  testthat::expect_error(check_soft_duplicates(dataset = cleaningtools::cleaningtools_sample_frame,
                                               kobo_survey = cleaningtools::cleaningtools_survey,
                                               uuid = "X_uuid"))
  testthat::expect_error(check_soft_duplicates(dataset = cleaningtools::cleaningtools_raw_data,
                                               kobo_survey = cleaningtools::cleaningtools_survey,
                                               uuid = "uuid"))
  t
  testthat::expect_no_error(check_soft_duplicates(dataset = cleaningtools::cleaningtools_raw_data,
                                                  kobo_survey = cleaningtools::cleaningtools_survey,
                                                  uuid = "X_uuid",idnk_value = "dont_know"))
})

testthat::test_that("Exact equals", {
  actual_output <- check_soft_duplicates(dataset = cleaningtools::cleaningtools_raw_data,
                                         kobo_survey = cleaningtools::cleaningtools_survey,
                                         uuid = "X_uuid",idnk_value = "dont_know")[[2]] %>% head()

  expected_output <- data.frame(X_uuid = c("3370f726-395a-4675-94fe-9e745e0b36e9",
                                         "93095da3-5291-4d16-a19a-41bf13144bfe",
                                         "db5e05db-94e9-44aa-9206-3e1c17a7a233",
                                         "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2",
                                         "0858486a-1d3d-492b-863f-b050cb9fe7af",
                                         "193d5f36-93b9-4c97-9205-13aa7e3a6c7f"),
                                num_cols_not_NA = c(76,86,86,76,75,76),
                                num_cols_dont_know = c(0,0,0,0,0,0),
                                id_most_similar_survey = c("dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2",
                                                           "db5e05db-94e9-44aa-9206-3e1c17a7a233",
                                                           "93095da3-5291-4d16-a19a-41bf13144bfe",
                                                           "3370f726-395a-4675-94fe-9e745e0b36e9",
                                                           "93893e39-9c82-4e19-b480-4dc78033157b",
                                                           "ac42b381-4d3b-42b4-96fb-676d43a8c4e7"),
                                number_different_columns = c(10,10,10,10,11,11))

  testthat::expect_equal(actual_output,expected_output)


  })

testthat::test_that("Check all uuids", {
  actual_output <- check_soft_duplicates(dataset = cleaningtools::cleaningtools_raw_data,
                                         kobo_survey = cleaningtools::cleaningtools_survey,
                                         uuid = "X_uuid", idnk_value = "dont_know")[[2]] %>%
    arrange(X_uuid) %>% select(X_uuid) %>% pull
  expected_output <- cleaningtools::cleaningtools_raw_data %>% arrange(X_uuid) %>% select(X_uuid) %>% pull
  testthat::expect_equal(actual_output,expected_output)
})

testthat::test_that("Returns a list of 2 dataframe", {
  actual_output <- check_soft_duplicates(dataset = cleaningtools::cleaningtools_raw_data,
                                         kobo_survey = cleaningtools::cleaningtools_survey,
                                         uuid = "X_uuid",idnk_value = "dont_know")
  testthat::expect_equal(length(actual_output),2)
  testthat::expect_equal(names(actual_output),c("checked_dataset","soft_duplicate_log"))
})
