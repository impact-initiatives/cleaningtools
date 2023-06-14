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

  expected_output <- data.frame(X_uuid = c("0858486a-1d3d-492b-863f-b050cb9fe7af",
                                         "3370f726-395a-4675-94fe-9e745e0b36e9",
                                         "93095da3-5291-4d16-a19a-41bf13144bfe",
                                         "93893e39-9c82-4e19-b480-4dc78033157b",
                                         "db5e05db-94e9-44aa-9206-3e1c17a7a233",
                                         "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2"),
                                num_cols_not_NA = c(82,83,93,82,93,83),
                                num_cols_dont_know = c(0,0,0,0,0,0),
                                id_most_similar_survey = c("93893e39-9c82-4e19-b480-4dc78033157b",
                                                           "dc7bf25b-e18b-4b9e-bb34-5d7a1e762eb2",
                                                           "db5e05db-94e9-44aa-9206-3e1c17a7a233",
                                                           "0858486a-1d3d-492b-863f-b050cb9fe7af",
                                                           "93095da3-5291-4d16-a19a-41bf13144bfe",
                                                           "3370f726-395a-4675-94fe-9e745e0b36e9"),
                                number_different_columns = c(14,14,14,14,14,14))

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
