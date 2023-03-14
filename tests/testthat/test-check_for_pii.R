library(testthat)
library(tibble)

test_that("Check for PII", {

### TEST 1::Happy path, columns found in the dataframe



  to_rm <- c("date_assessment", "neighbourhood", "return_date", "water_supply_rest_neighbourhood",
             "water_supply_other_neighbourhoods", "water_supply_other_neighbourhoods_why",
             "consent_telephone_number")

  no_pii_data <- raw_data %>% select(-all_of(to_rm))

  df_list <- list(raw_data=raw_data,
                  no_pii_data =  no_pii_data)


  output1 <- check_for_pii(df = df_list,element_name = "raw_data",words_to_look = "date")
  output2 <- check_for_pii(df = df_list,element_name = "raw_data")
  output3 <- check_for_pii(df = df_list,element_name = "no_pii_data")
  output4 <- check_for_pii(df = df_list,element_name = "raw_data",words_to_look = c("date","enumerator"))


  output_fm_data_frame1 <- check_for_pii(raw_data,words_to_look = "date")
  output_fm_data_frame2 <- check_for_pii(raw_data)


  output1$potential_PII$question %>% dput

  expected_outcome <-tibble(
    X_uuid = rep("all",7),
    question = c("date_assessment", "neighbourhood", "return_date", "water_supply_rest_neighbourhood",
                 "water_supply_other_neighbourhoods", "water_supply_other_neighbourhoods_why",
                 "consent_telephone_number"),
    issue = rep("Potential PII",7)
  )
  testthat::expect_identical(output1$potential_PII,expected_outcome)


  testthat::expect_equal(nrow(output1$potential_PII),7)
  testthat::expect_equal(nrow(output1$potential_PII),nrow(output_fm_data_frame1$potential_PII))

  testthat::expect_equal(nrow(output2$potential_PII),5)
  testthat::expect_equal(nrow(output2$potential_PII),nrow(output_fm_data_frame2$potential_PII))

  testthat::expect_identical(names(output1$potential_PII),c("X_uuid","question","issue"))
  testthat::expect_identical(names(output2$potential_PII),c("X_uuid","question","issue"))

  testthat::expect_identical(names(output_fm_data_frame2$potential_PII),c("X_uuid","question","issue"))


  testthat::expect_equal(nrow(output3$potential_PII),0)
  testthat::expect_equal(nrow(output4$potential_PII),8)

  ### test - output must be list

### TEST 2::Sad path, columns not found in the dataframe

  expect_error(check_for_pii(df_list,words_to_look ="Date"),regexp = "element_name is missing")
  expect_error(check_for_pii(df_list,element_name = "A",words_to_look ="Date"),regexp = "element_name not found")
  expect_warning(check_for_pii(raw_data,element_name = "a",words_to_look = "date"),regexp = "Input is a dataframe, ignoring element_name")
  expect_error(check_for_pii(raw_data,uuid = "uuid"),regexp = "uuid not found in the dataset")


  expect_length(output1,2)
  expect_length(output_fm_data_frame2,2)
  expect_identical(names(output_fm_data_frame2),c("df","potential_PII"))



})
