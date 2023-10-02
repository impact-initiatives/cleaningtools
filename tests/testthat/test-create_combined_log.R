test_that("error and warning check", {
  list <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid")

  ### NULL check

  testthat::expect_warning(
    create_combined_log(list_of_log = list, dataset_name = NULL),
    "You have a checked_dataset element in the list_of_log even though you have set dataset_name to NULL. Please check the parameter."
  )


  list_2 <- list[names(list)[!names(list) %in% "checked_dataset"]]

  testthat::expect_message(
    create_combined_log(list_of_log = list_2, dataset_name = NULL),
    "No dataset name is provided. Assuming that the dataset does not exist in the list_of_log."
  )

  testthat::expect_error(
    create_combined_log(list_of_log = list, dataset_name = "dataset"),
    "dataset can not be found in the list_of_log."
  )


  testthat::expect_error(create_combined_log(list_of_log = "list"), "list_of_log must be a list which should contain the logs.")

  list_df <- list[[2]] |> as.data.frame()
  testthat::expect_error(create_combined_log(list_of_log = list_df), "list_of_log must be a list which should contain the logs.")
})

test_that("expect equal", {
  list <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid")


  output <- create_combined_log(list_of_log = list)


  testthat::expect_equal(names(output), c("checked_dataset", "cleaning_log"))


  expected <- structure(
    list(
      uuid = c(
        "all", "all", "all", "all", "all", "ac26e24d-12be-4729-bae7-21060ee00a28",
        "e8b1ba82-59df-4910-b9c8-29bc3fd516ab"
      ),
      question = c(
        "neighbourhood", "water_supply_rest_neighbourhood", "water_supply_other_neighbourhoods",
        "water_supply_other_neighbourhoods_why", "consent_telephone_number",
        "X_index", "X_index"
      ),
      issue = structure(c("Potential PII", "Potential PII", "Potential PII", "Potential PII", "Potential PII", "Possible value to be changed to NA", "Possible value to be changed to NA"),
        class = c("glue", "character")
      ),
      old_value = c(NA, NA, NA, NA, NA, "88", "99"),
      change_type = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
      new_value = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
      check_binding = c("neighbourhood ~/~ all",
                        "water_supply_rest_neighbourhood ~/~ all",
                        "water_supply_other_neighbourhoods ~/~ all",
                        "water_supply_other_neighbourhoods_why ~/~ all",
                        "consent_telephone_number ~/~ all",
                        "X_index ~/~ ac26e24d-12be-4729-bae7-21060ee00a28",
                        "X_index ~/~ e8b1ba82-59df-4910-b9c8-29bc3fd516ab")
    ),
    row.names = c(NA, -7L),
    class = c("data.frame")
  )

  actual <- output$cleaning_log |> as.data.frame()

  testthat::expect_equal(actual, expected)


  ## WITH NULL
  list_2 <- list[names(list)[!names(list) %in% "checked_dataset"]]

  output <- create_combined_log(list_of_log = list_2, dataset_name = NULL)
  testthat::expect_equal(names(output), c("cleaning_log"))
  actual <- output$cleaning_log |> as.data.frame()
  testthat::expect_equal(actual, expected)
})




testthat::test_that("check binding capability where data type is different", {
  check_data <- cleaningtools::cleaningtools_raw_data %>%
    add_percentage_missing()
  checkss <- cleaningtools::check_pii(dataset = check_data, uuid_column = "X_uuid") %>%
    cleaningtools::check_outliers(uuid_column = "X_uuid") %>%
    check_others(
      uuid_column = "X_uuid",
      columns_to_check = names(cleaningtools::cleaningtools_raw_data |> dplyr::select(ends_with("_other")) |> dplyr::select(-contains(".")))
    ) %>%
    check_percentage_missing(uuid_column = "X_uuid") %>%
    check_logical(uuid_column = "X_uuid", check_to_perform = "inc_employment_pension == tot_expenses", description = "income equals to expenses")

  cannot_bind <- create_combined_log(list_of_log = checkss)

  testthat::expect_equal(nrow(cannot_bind$cleaning_log), 722)
})
