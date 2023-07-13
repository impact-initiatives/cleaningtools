testthat::test_that("Error messege test", {


  temp_dir_to_test <- withr::local_tempdir(fileext = "test")

  ############################3

  list <- cleaningtools::cleaningtools_raw_data |> check_for_pii() |>
    check_duplicate(uuid_col_name = "X_uuid") |>
    check_for_value(uuid_col_name = "X_uuid")

  cleaning_log_combined <-create_combined_log(list_of_log = list)


  workbook <- cleaning_log_combined |> create_xlsx_cleaning_log(cols_for_color = "question",output_path = NULL,
                                     cleaning_log_name = "cleaning_log",
                                     change_type_col = "change_type")
  ## checking as it must be a workbook
  expected <- structure("Workbook", package = "openxlsx")
  testthat::expect_equal(dput(class(workbook)),expected)


  ### expected outcome
  cleaning_log_combined |> create_xlsx_cleaning_log(cols_for_color = "question",
                                                    output_path = paste0(temp_dir_to_test, "\\testing_cleaning_log.xlsx"),
                                cleaning_log_name = "cleaning_log",
                                change_type_col = "change_type")


  expected_output_1 <- openxlsx::read.xlsx(testthat::test_path("fixtures", "write_cleaning_log.xlsx"),1)
  expected_output_2 <- openxlsx::read.xlsx(testthat::test_path("fixtures", "write_cleaning_log.xlsx"),2)

  actual_output_1 <- openxlsx::read.xlsx(paste0(temp_dir_to_test, "\\testing_cleaning_log.xlsx"),1)
  actual_output_2 <- openxlsx::read.xlsx(paste0(temp_dir_to_test, "\\testing_cleaning_log.xlsx"),2)


  testthat::expect_equal(expected_output_1,actual_output_1)

  testthat::expect_equal(expected_output_2,actual_output_2)


  ### check erroe


  testthat::expect_error(
    cleaning_log_combined |> create_xlsx_cleaning_log(cols_for_color = "question",
                                                      output_path = paste0(temp_dir_to_test, "\\testing_cleaning_log.xlsx"),
                                                      cleaning_log_name = "cleaning_log2",
                                                      change_type_col = "change_type"),
    "cleaning_log2 not found in the given list."
  )


  testthat::expect_error(
    cleaning_log_combined |> create_xlsx_cleaning_log(cols_for_color = "question",
                                                      output_path = paste0(temp_dir_to_test, "\\testing_cleaning_log.xlsx"),
                                                      cleaning_log_name = "cleaning_log",
                                                      change_type_col = "change_type2"),
    "change_type2 not found in cleaning_log."
  )

  cleaning_log_combined[["validation_rules"]] <- data.frame()
  testthat::expect_error(
    cleaning_log_combined |> create_xlsx_cleaning_log(cols_for_color = "question",
                                                      output_path = paste0(temp_dir_to_test, "\\testing_cleaning_log.xlsx"),
                                                      cleaning_log_name = "cleaning_log",
                                                      change_type_col = "change_type"),
    "The list currently has an element named `validation_rules`. Please consider renaming it."
  )


  })
