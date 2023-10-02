testthat::test_that("Error messege test", {
  temp_dir_to_test <- tempdir(check = T)

  ############################

  list <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid")

  cleaning_log_combined <- create_combined_log(list_of_log = list)


  workbook <- cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question", output_path = NULL,
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type"
  )

  ## checking as it must be a workbook
  expected <- structure("Workbook", package = "openxlsx")
  testthat::expect_equal(dput(class(workbook)), expected)


  ### expected outcome
  cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type"
  )


  expected_output_1 <- openxlsx::read.xlsx(testthat::test_path("fixtures", "write_cleaning_log.xlsx"), 3)
  expected_output_2 <- openxlsx::read.xlsx(testthat::test_path("fixtures", "write_cleaning_log.xlsx"), 2)

  actual_output_1 <- openxlsx::read.xlsx(paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"), 4)
  actual_output_2 <- openxlsx::read.xlsx(paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"), 2)


  testthat::expect_equal(expected_output_1, actual_output_1)

  testthat::expect_equal(expected_output_2, actual_output_2)


  ### check error

  testthat::expect_error(
    cleaning_log_combined |> create_xlsx_cleaning_log(
      column_for_color = "question",
      output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
      cleaning_log_name = "cleaning_log2",
      change_type_col = "change_type"
    ),
    "cleaning_log2 not found in the given list."
  )


  testthat::expect_error(
    cleaning_log_combined |> create_xlsx_cleaning_log(
      column_for_color = "question",
      output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
      cleaning_log_name = "cleaning_log",
      change_type_col = "change_type2"
    ),
    "change_type2 not found in cleaning_log."
  )

  cleaning_log_combined[["validation_rules"]] <- data.frame()
  testthat::expect_error(
    cleaning_log_combined |> create_xlsx_cleaning_log(
      column_for_color = "question",
      output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
      cleaning_log_name = "cleaning_log",
      change_type_col = "change_type"
    ),
    "The list currently has an element named `validation_rules`. Please consider renaming it."
  )
})


testthat::expect_error(
  cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type",
    use_dropdown = T
  ),
  "Kobo survey and choices sheets should be provided to use dropdown lists"
)

testthat::expect_error(
  cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type",
    kobo_survey = cleaningtools_survey,
    use_dropdown = T
  ),
  "Kobo survey and choices sheets should be provided to use dropdown lists"
)

testthat::expect_error(
  cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type",
    kobo_choices = cleaningtools_choices,
    use_dropdown = T
  ),
  "Kobo survey and choices sheets should be provided to use dropdown lists"
)


testthat::expect_error(
  cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type",
    kobo_survey = data.frame(),
    kobo_choices = cleaningtools_choices,
    use_dropdown = T
  ),
  "The Kobo survey dataframe is not valid"
)

testthat::expect_error(
  cleaning_log_combined |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = paste0(temp_dir_to_test, "/testing_cleaning_log.xlsx"),
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type",
    kobo_survey = cleaningtools_survey,
    kobo_choices = data.frame(),
    use_dropdown = T
  ),
  "The Kobo choices dataframe is not valid"
)

testthat::test_that("Testing the function is working as expected", {
  testthat::expect_no_error({
    clog_test <- cleaningtools_raw_data |> check_logical(
      check_to_perform = 'treat_cook_water == "always_treat"',
      uuid_column = "X_uuid",
      description = "description",
      check_id = "check_4",
      columns_to_clean = "rental_contract"
    )

    create_combined_log(clog_test) |> create_xlsx_cleaning_log(
      column_for_color = "question",
      output_path = NULL,
      cleaning_log_name = "cleaning_log",
      change_type_col = "change_type",
      kobo_survey = cleaningtools::cleaningtools_survey,
      kobo_choices = cleaningtools::cleaningtools_choices,
      use_dropdown = T,
      sm_dropdown_type = "logical"
    )
  })


  testthat::expect_equal(
    object = create_validation_list(choices = cleaningtools_choices, tool = cleaningtools_survey)$change_type_validation[1:4],
    expected = c("change_response", "blank_response", "remove_survey", "no_action")
  )


  testthat::expect_equal(
    object = create_validation_list(choices = cleaningtools_choices, tool = cleaningtools_survey)$consent_share_location[1:2],
    expected = c("yes", "no")
  )



  tmp_file <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".xlsx")

  clog_test <- cleaningtools_raw_data |> check_logical(
    check_to_perform = 'treat_cook_water == "always_treat"',
    uuid_column = "X_uuid",
    description = "description",
    check_id = "check_4",
    columns_to_clean = "rental_contract"
  )


  create_combined_log(clog_test) |> create_xlsx_cleaning_log(
    column_for_color = "question",
    output_path = tmp_file,
    cleaning_log_name = "cleaning_log",
    change_type_col = "change_type",
    kobo_survey = cleaningtools::cleaningtools_survey,
    kobo_choices = cleaningtools::cleaningtools_choices,
    use_dropdown = TRUE,
    sm_dropdown_type = "logical"
  )

  generated_output <- openxlsx::read.xlsx(
    xlsxFile = tmp_file,
    sheet = "validation_rules"
  )

  expected_output <- readRDS(testthat::test_path("fixtures", "validation_rules_sheet.rds"))


  testthat::expect_equal(
    object = generated_output,
    expected = expected_output
  )

  testthat::expect_equal(
    object = names(generated_output[1]),
    expected = names(expected_output[1])
  )
})
