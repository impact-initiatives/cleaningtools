testthat::test_that("error messege check ", {
  # list_of_log is a dataframe. It must be either list or NULL

  testthat::expect_error(add_info_to_cleaning_log(list_of_log = cleaningtools::cleaningtools_clean_data))

  #### Element check

  list_test <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid")

  ### element check- dataset
  testthat::expect_error(add_info_to_cleaning_log(list_of_log = list_test, dataset = "s"))

  ### element check- cleaning_log
  testthat::expect_error(add_info_to_cleaning_log(list_of_log = list_test, cleaning_log = "cl"))


  ### list_of_log NULL


  list_test2 <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid") |>
    create_combined_log()

  df <- list_test2$checked_dataset
  cl <- list_test2$cleaning_log


  testthat::expect_error(add_info_to_cleaning_log(cleaning_log = list_test2, dataset = df))
  testthat::expect_error(add_info_to_cleaning_log(cleaning_log = cl, dataset = list_test2))
})



testthat::test_that("expect equal ", {
  ######## test with list
  actual <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid") |>
    create_combined_log() |>
    add_info_to_cleaning_log(dataset_uuid_column = "X_uuid")


  testthat::expect_equal(length(actual), 2)
  testthat::expect_equal(names(actual), c("checked_dataset", "cleaning_log"))

  actual <- actual$cleaning_log

  expected <- structure(
    list(
      uuid = c("ac26e24d-12be-4729-bae7-21060ee00a28", "all", "all", "all", "all", "all", "e8b1ba82-59df-4910-b9c8-29bc3fd516ab"),
      question = c(
        "X_index", "neighbourhood", "water_supply_rest_neighbourhood",
        "water_supply_other_neighbourhoods", "water_supply_other_neighbourhoods_why",
        "consent_telephone_number", "X_index"
      ),
      issue = c("Possible value to be changed to NA", "Potential PII", "Potential PII", "Potential PII", "Potential PII", "Potential PII", "Possible value to be changed to NA"),
      old_value = c("88", NA, NA, NA, NA, NA, "99"),
      change_type = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
      new_value = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
      check_binding = c(
        "X_index ~/~ ac26e24d-12be-4729-bae7-21060ee00a28",
        "neighbourhood ~/~ all",
        "water_supply_rest_neighbourhood ~/~ all",
        "water_supply_other_neighbourhoods ~/~ all",
        "water_supply_other_neighbourhoods_why ~/~ all",
        "consent_telephone_number ~/~ all",
        "X_index ~/~ e8b1ba82-59df-4910-b9c8-29bc3fd516ab"
      ),
      enumerator_num = c(13L, NA, NA, NA, NA, NA, 9L),
      date_assessment = c("2021-07-06", NA, NA, NA, NA, NA, "2021-07-07")
    ),
    row.names = c(NA, -7L), class = "data.frame"
  )

  testthat::expect_equal(actual, expected)




  ############### check with dataframe

  list_test3 <- cleaningtools::cleaningtools_raw_data |>
    check_pii(uuid_column = "X_uuid") |>
    check_duplicate(uuid_column = "X_uuid") |>
    check_value(uuid_column = "X_uuid") |>
    create_combined_log()




  df <- list_test3$checked_dataset
  cl <- list_test3$cleaning_log

  actual <- add_info_to_cleaning_log(dataset = df, cleaning_log = cl, dataset_uuid_column = "X_uuid")

  actual <- actual$cleaning_log
  testthat::expect_equal(actual, expected)

  ############ test_with_diff list

  raw <- list(
    df = df,
    cl = cl
  )

  actual <- add_info_to_cleaning_log(list_of_log = raw, cleaning_log = "cl", dataset = "df", dataset_uuid_column = "X_uuid")

  testthat::expect_equal(length(actual), 2)
  testthat::expect_equal(names(actual), c("checked_dataset", "cleaning_log"))


  actual <- actual$cleaning_log
  testthat::expect_equal(actual, expected)



  ############ test_with_diff uuid


  df <- df |> dplyr::rename(
    id = X_uuid
  )

  cl <- cl |> dplyr::rename(
    id1 = uuid
  )

  raw <- list(
    df = df,
    cl = cl
  )

  actual <- add_info_to_cleaning_log(list_of_log = raw, cleaning_log = "cl", dataset = "df", dataset_uuid_column = "id", cleaning_log_uuid_column = "id1")

  testthat::expect_equal(length(actual), 2)
  testthat::expect_equal(names(actual), c("checked_dataset", "cleaning_log"))


  expected <- expected |> dplyr::rename(
    id1 = uuid
  )

  actual <- actual$cleaning_log
  testthat::expect_equal(actual, expected)




  ########### Multiple join column ########################
  df <- df |> dplyr::mutate(
    primary_key_2 = "a"
  )

  cl <- cl |> dplyr::mutate(
    primary_key = "a"
  )
  cl[6, "primary_key"] <- NA_character_




  raw <- list(
    df = df,
    cl = cl
  )

  actual <- add_info_to_cleaning_log(
    list_of_log = raw,
    cleaning_log = "cl",
    dataset = "df",
    dataset_uuid_column = c("id", "primary_key_2"),
    cleaning_log_uuid_column = c("id1", "primary_key")
  )

  actual <- actual$cleaning_log


  expected <- structure(
    list(
      id1 = c("ac26e24d-12be-4729-bae7-21060ee00a28", "all", "all", "all", "all", "all", "e8b1ba82-59df-4910-b9c8-29bc3fd516ab"), primary_key = c(NA, "a", "a", "a", "a", "a", "a"),
      question = c(
        "X_index", "neighbourhood", "water_supply_rest_neighbourhood",
        "water_supply_other_neighbourhoods", "water_supply_other_neighbourhoods_why",
        "consent_telephone_number", "X_index"
      ),
      issue = c("Possible value to be changed to NA", "Potential PII", "Potential PII", "Potential PII", "Potential PII", "Potential PII", "Possible value to be changed to NA"),
      old_value = c("88", NA, NA, NA, NA, NA, "99"),
      change_type = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
      new_value = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
      check_binding = c(
        "X_index ~/~ ac26e24d-12be-4729-bae7-21060ee00a28",
        "neighbourhood ~/~ all",
        "water_supply_rest_neighbourhood ~/~ all",
        "water_supply_other_neighbourhoods ~/~ all",
        "water_supply_other_neighbourhoods_why ~/~ all",
        "consent_telephone_number ~/~ all",
        "X_index ~/~ e8b1ba82-59df-4910-b9c8-29bc3fd516ab"
      ),
      enumerator_num = c(13L, NA, NA, NA, NA, NA, 9L),
      date_assessment = c("2021-07-06", NA, NA, NA, NA, NA, "2021-07-07")
    ),
    row.names = c(NA, -7L), class = "data.frame"
  )

  expected[1, "date_assessment"] <- NA_character_
  expected[1, "enumerator_num"] <- NA_real_
  testthat::expect_equal(actual, expected)
})
