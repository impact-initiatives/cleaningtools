testthat::test_that("Error check", {
  testthat::expect_error(review_others(
    dataset = cleaningtools::cleaningtools_clean_data,
    uuid_column = "uuid", kobo_survey = cleaningtools_survey
  ))

  testthat::expect_error(review_others(
    dataset = cleaningtools::cleaningtools_clean_data,
    uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    information_to_add = "eenumerator_num"
  ))

  testthat::expect_no_error(review_others(
    dataset = cleaningtools::cleaningtools_clean_data,
    uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    information_to_add = "enumerator_num"
  ) %>%
    suppressWarnings())

  review_others(
    dataset = cleaningtools::cleaningtools_clean_data,
    uuid_column = "X_uuid",
    kobo_survey = cleaningtools_survey,
    columns_not_to_check = "consent_stelephone_number",
    information_to_add = "enumerator_num"
  ) %>%
    testthat::expect_warning() %>%
    testthat::expect_warning()
})


testthat::test_that("expect equal", {
  actual <- review_others(
    dataset = cleaningtools::cleaningtools_clean_data,
    uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    information_to_add = "enumerator_num"
  )[1:5, ] %>%
    suppressWarnings()


  expected_df <- structure(list(
    uuid = c(
      "f58a7fda-27e8-4003-90b3-479bebbb99ab",
      "956b5ed0-5a62-41b7-aec3-af93fbc5b494", "3413afd2-8f05-4a6e-8ec7-5d64dc8fea23",
      "630d0067-d84a-4fd0-8c36-029e87913c40", "2cd180e8-7f2b-460b-82b5-fb9d163f8e7b"
    ), enumerator_num = c("15", "16", "16", "8", "8"),
    question = c("consent_telephone_number", "consent_telephone_number", "consent_telephone_number", "consent_telephone_number", "consent_telephone_number"),
    old_value = c("yes", "yes", "yes", "yes", "yes"),
    issue = c(
      "consent_telephone_number is selected but telephone_number is not found in the dataset",
      "consent_telephone_number is selected but telephone_number is not found in the dataset",
      "consent_telephone_number is selected but telephone_number is not found in the dataset",
      "consent_telephone_number is selected but telephone_number is not found in the dataset",
      "consent_telephone_number is selected but telephone_number is not found in the dataset"
    ), check_id = c("id- 27", "id- 27", "id- 27", "id- 27", "id- 27"), check_binding = c(
      "id- 27 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab",
      "id- 27 ~/~ 956b5ed0-5a62-41b7-aec3-af93fbc5b494", "id- 27 ~/~ 3413afd2-8f05-4a6e-8ec7-5d64dc8fea23",
      "id- 27 ~/~ 630d0067-d84a-4fd0-8c36-029e87913c40", "id- 27 ~/~ 2cd180e8-7f2b-460b-82b5-fb9d163f8e7b"
    )
  ), row.names = c(NA, 5L), class = "data.frame")

  testthat::expect_equal(actual, expected_df)


  #### check if all the values are flagging or not

  df <- cleaningtools_clean_data
  df$shelter_occupation_other[1] <- "select_one_error"
  df$primary_livelihood_other[10] <- "select_multiple_error"

  actual <- review_others(
    dataset = df,
    uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    columns_not_to_check = "consent_telephone_number"
  ) %>%
    suppressWarnings()

  expected <- structure(
    list(
      uuid = c(
        "f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "84c8db18-b1fa-4ba6-a764-b77b642353a3",
        "84c8db18-b1fa-4ba6-a764-b77b642353a3"
      ),
      question = c(
        "shelter_occupation", "shelter_occupation_other",
        "primary_livelihood_other", "primary_livelihood.other"
      ), old_value = c("renting", "select_one_error", "select_multiple_error", "FALSE"),
      issue = c(
        "shelter_occupation_other has value but the shelter_occupation column is not other(Not matching with kobo relevancy.)",
        "shelter_occupation_other has value but the shelter_occupation column is not other(Not matching with kobo relevancy.)",
        "primary_livelihood_other is NOT NA but the binary column ( primary_livelihood.other) is selected as FALSE/0/NA",
        "primary_livelihood_other is NOT NA but the binary column ( primary_livelihood.other) is selected as FALSE/0/NA"
      ),
      check_id = c("id- 28", "id- 28", "id- 42", "id- 42"),
      check_binding = c(
        "id- 28 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "id- 28 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "id- 42 ~/~ 84c8db18-b1fa-4ba6-a764-b77b642353a3",
        "id- 42 ~/~ 84c8db18-b1fa-4ba6-a764-b77b642353a3"
      )
    ),
    row.names = c(NA, -4L), class = "data.frame"
  )

  testthat::expect_equal(actual, expected)


  #### check if all the values are flagging or not 2

  df <- cleaningtools_clean_data
  df$shelter_occupation[1] <- "other"
  df$primary_livelihood.other[10] <- TRUE

  actual <- review_others(
    dataset = df,
    uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    columns_not_to_check = "consent_telephone_number"
  ) %>%
    suppressWarnings()

  expected <- structure(
    list(
      uuid = c(
        "f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "84c8db18-b1fa-4ba6-a764-b77b642353a3",
        "84c8db18-b1fa-4ba6-a764-b77b642353a3"
      ),
      question = c(
        "shelter_occupation", "shelter_occupation_other",
        "primary_livelihood_other", "primary_livelihood.other"
      ), old_value = c("other", "NA", "NA", "TRUE"),
      issue = c(
        "shelter_occupation_other is NA but the shelter_occupation column is seleted as other/relevent choice(other)",
        "shelter_occupation_other is NA but the shelter_occupation column is seleted as other/relevent choice(other)",
        "primary_livelihood_other is NA but the binary column (primary_livelihood.other)is selected as TRUE/1",
        "primary_livelihood_other is NA but the binary column (primary_livelihood.other)is selected as TRUE/1"
      ), check_id = c("id- 35", "id- 35", "id- 45", "id- 45"),
      check_binding = c(
        "id- 35 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "id- 35 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "id- 45 ~/~ 84c8db18-b1fa-4ba6-a764-b77b642353a3",
        "id- 45 ~/~ 84c8db18-b1fa-4ba6-a764-b77b642353a3"
      )
    ),
    row.names = c(NA, -4L), class = "data.frame"
  )

  testthat::expect_equal(actual, expected)


  #### check if all the values are flagging or not 3

  df <- cleaningtools_clean_data
  df$shelter_occupation[1] <- "other"
  df$primary_livelihood.other[10] <- TRUE
  df$primary_livelihood_other[10] <- "TRUEs"

  actual <- review_others(
    data = df,
    uuid_column = "X_uuid", kobo_survey = cleaningtools_survey,
    columns_not_to_check = "consent_telephone_number"
  ) %>%
    suppressWarnings()

  expected <- structure(
    list(
      uuid = c(
        "f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "f58a7fda-27e8-4003-90b3-479bebbb99ab"
      ),
      question = c(
        "shelter_occupation",
        "shelter_occupation_other"
      ),
      old_value = c("other", "NA"),
      issue = c(
        "shelter_occupation_other is NA but the shelter_occupation column is seleted as other/relevent choice(other)",
        "shelter_occupation_other is NA but the shelter_occupation column is seleted as other/relevent choice(other)"
      ),
      check_id = c("id- 35", "id- 35"),
      check_binding = c(
        "id- 35 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab",
        "id- 35 ~/~ f58a7fda-27e8-4003-90b3-479bebbb99ab"
      )
    ),
    row.names = c(NA, -2L), class = "data.frame"
  )

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Test that create_logic_for_others select multiple is correctly picked", {
  # Test that review_others works correctly with select multiple and / separator
  test_data <- cleaningtools_clean_data
  names(test_data) <- gsub("\\.", "/", names(test_data))
  names(test_data)[1] <- "start"
  expect_no_error(review_others(test_data,
    uuid = "X_uuid",
    kobo_survey = cleaningtools_survey,
    sm_sep = "/"
  )) %>%
    suppressWarnings()
})
