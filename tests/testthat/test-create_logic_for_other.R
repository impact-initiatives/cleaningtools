test_that("create logic for other", {
  logic_df <- create_logic_for_other(
    kobo_survey = cleaningtools::cleaningtools_survey,
    sm_separator = ".",
    dataset = cleaningtools::cleaningtools_clean_data,
    compare_with_dataset = T
  ) %>%
    suppressWarnings()

  testthat::expect_warning(create_logic_for_other(
    kobo_survey = cleaningtools::cleaningtools_survey,
    sm_separator = ".",
    dataset = cleaningtools::cleaningtools_clean_data,
    compare_with_dataset = T
  ))
  testthat::expect_error(create_logic_for_other(
    kobo_survey = cleaningtools::cleaningtools_survey,
    sm_separator = ".",
    compare_with_dataset = T
  ))

  testthat::expect_equal(nrow(logic_df), 47)


  # Other/choice is not select in parent column but text/_other column not found in the dataset
  logic_filter_1 <- c(
    "(`water_sources.other` == TRUE | `water_sources.other` == 1 ) & !is.na(`water_sources.other`)",
    paste0("water_source_drinking == ", "\"", "other", "\"")
  )

  logic_df_filter_1 <- logic_df |> dplyr::filter(grepl(" is selected but ", description))



  testthat::expect_true(all(logic_filter_1 %in% logic_df_filter_1$logic))


  # "Text column has value but the parent column is not corresponding with the relevancy"

  logic_df_filter_2 <- logic_df |>
    dplyr::filter(grepl("(Not matching with kobo relevancy.)", description))


  logic_filter_2 <- c("!is.na(shelter_occupation_other) & (shelter_occupation!=\"other\")")
  testthat::expect_true(all(logic_filter_2 %in% logic_df_filter_2$logic))


  # "Text column is NA but the parent column is seleted as other/relevent choice"
  logic_df_filter_3 <- logic_df |>
    dplyr::filter(grepl(" column is seleted as other", description))

  logic_filter_3 <- c("is.na(shelter_occupation_other) & (shelter_occupation==\"other\")")
  testthat::expect_true(all(logic_filter_3 %in% logic_df_filter_3$logic))


  # "Text column is NOT NA but the binary column is selected as FALSE/0/NA"
  logic_df_filter_4 <- logic_df |>
    dplyr::filter(grepl("is selected as FALSE/0/NA", description))


  logic_filter_4 <- c("!is.na(primary_livelihood_other) & (`primary_livelihood.other`==0 |`primary_livelihood.other`==FALSE | is.na(`primary_livelihood.other`))")
  testthat::expect_true(all(logic_filter_4 %in% logic_df_filter_4$logic))



  # "Text column is NA but the binary column is selected as TRUE/1"
  logic_df_filter_5 <- logic_df |>
    dplyr::filter(grepl("is selected as TRUE/1", description))

  logic_filter_5 <- c("is.na(primary_livelihood_other) & (`primary_livelihood.other`==1 |`primary_livelihood.other`== TRUE)")
  testthat::expect_true(all(logic_filter_5 %in% logic_df_filter_5$logic))
})

test_that("check with compare with dataset is FALSE", {
  logic_df <- create_logic_for_other(
    kobo_survey = cleaningtools::cleaningtools_survey,
    sm_separator = "."
  )
  testthat::expect_false("water_sources.other is selected but water_sources_other is not found in the dataset" %in% logic_df$description)
})

test_that("check sm_spe", {
  logic_df <- create_logic_for_other(
    kobo_survey = cleaningtools::cleaningtools_survey,
    sm_separator = "/"
  )
  testthat::expect_false("water_sources.other is selected but water_sources_other is not found in the dataset" %in% logic_df$description)


  # "Text column has value but the parent column is not corresponding with the relevancy"

  logic_df_filter_2 <- logic_df |>
    dplyr::filter(grepl("Not matching with kobo relevancy.", description))



  logic_filter_2 <- c("!is.na(shelter_occupation_other) & (shelter_occupation!=\"other\")")
  testthat::expect_true(all(logic_filter_2 %in% logic_df_filter_2$logic))


  # "Text column is NA but the parent column is seleted as other/relevent choice"
  logic_df_filter_3 <- logic_df |>
    dplyr::filter(grepl("column is seleted as other", description))


  logic_filter_3 <- c("is.na(shelter_occupation_other) & (shelter_occupation==\"other\")")
  testthat::expect_true(all(logic_filter_3 %in% logic_df_filter_3$logic))


  # "Text column is NOT NA but the binary column is selected as FALSE/0/NA"
  logic_df_filter_4 <- logic_df |>
    dplyr::filter(grepl("is selected as FALSE/0/NA", description))



  logic_filter_4 <- c("!is.na(primary_livelihood_other) & (`primary_livelihood/other`==0 |`primary_livelihood/other`==FALSE | is.na(`primary_livelihood/other`))")
  testthat::expect_true(all(logic_filter_4 %in% logic_df_filter_4$logic))



  # "Text column is NA but the binary column is selected as TRUE/1"
  logic_df_filter_5 <- logic_df |>
    dplyr::filter(grepl("is selected as TRUE/1", description))

  logic_filter_5 <- c("is.na(primary_livelihood_other) & (`primary_livelihood/other`==1 |`primary_livelihood/other`== TRUE)")
  testthat::expect_true(all(logic_filter_5 %in% logic_df_filter_5$logic))
})


test_that("expect equal", {
  logic_df <- create_logic_for_other(
    kobo_survey = cleaningtools::cleaningtools_survey,
    sm_separator = ".",
    dataset = cleaningtools::cleaningtools_clean_data,
    compare_with_dataset = T
  ) %>%
    suppressWarnings()

  unique_df <- logic_df[!duplicated(logic_df$description), ]


  expected_df <- structure(list(id = c(
    "id- 1", "id- 2", "id- 3", "id- 4", "id- 5",
    "id- 6", "id- 7", "id- 8", "id- 9", "id- 10", "id- 11", "id- 12",
    "id- 13", "id- 14", "id- 15", "id- 16", "id- 17", "id- 18", "id- 19",
    "id- 20", "id- 21", "id- 22", "id- 23", "id- 24", "id- 25", "id- 26",
    "id- 27", "id- 28", "id- 29", "id- 30", "id- 31", "id- 32", "id- 33",
    "id- 34", "id- 35", "id- 36", "id- 37", "id- 38", "id- 39", "id- 40",
    "id- 41", "id- 42", "id- 43", "id- 44", "id- 45", "id- 46", "id- 47"
  ), logic = c(
    "(`water_sources.other` == TRUE | `water_sources.other` == 1 ) & !is.na(`water_sources.other`)",
    "water_source_drinking == \"other\"", "(`treat_drink_water_how.other` == TRUE | `treat_drink_water_how.other` == 1 ) & !is.na(`treat_drink_water_how.other`)",
    "water_source_cook == \"other\"", "(`treat_cook_water_how.other` == TRUE | `treat_cook_water_how.other` == 1 ) & !is.na(`treat_cook_water_how.other`)",
    "(`piped_quality.no_other` == TRUE | `piped_quality.no_other` == 1 ) & !is.na(`piped_quality.no_other`)",
    "(`tap_quality.no_other` == TRUE | `tap_quality.no_other` == 1 ) & !is.na(`tap_quality.no_other`)",
    "(`borehole_quality.no_other` == TRUE | `borehole_quality.no_other` == 1 ) & !is.na(`borehole_quality.no_other`)",
    "(`trucking_quality.no_other` == TRUE | `trucking_quality.no_other` == 1 ) & !is.na(`trucking_quality.no_other`)",
    "water_source_bath == \"other\"", "water_source_clothes == \"other\"",
    "water_source_house == \"other\"", "water_source_animals == \"other\"",
    "water_source_crops == \"other\"", "pump_horsepower == \"other\"",
    "pay_water_charges_method == \"other\"", "pay_water_charges_prefer == \"other\"",
    "(`why_share_connection.other` == TRUE | `why_share_connection.other` == 1 ) & !is.na(`why_share_connection.other`)",
    "(`why_no_subscription.other` == TRUE | `why_no_subscription.other` == 1 ) & !is.na(`why_no_subscription.other`)",
    "(`own_subscription_assist_type.other` == TRUE | `own_subscription_assist_type.other` == 1 ) & !is.na(`own_subscription_assist_type.other`)",
    "(`problems_water_who_appr.other` == TRUE | `problems_water_who_appr.other` == 1 ) & !is.na(`problems_water_who_appr.other`)",
    "(`problems_water_who_attend.other` == TRUE | `problems_water_who_attend.other` == 1 ) & !is.na(`problems_water_who_attend.other`)",
    "(`water_supply_other_neighbourhoods_how.other` == TRUE | `water_supply_other_neighbourhoods_how.other` == 1 ) & !is.na(`water_supply_other_neighbourhoods_how.other`)",
    "(`users_build_trust.other` == TRUE | `users_build_trust.other` == 1 ) & !is.na(`users_build_trust.other`)",
    "(`help_office_improve.other` == TRUE | `help_office_improve.other` == 1 ) & !is.na(`help_office_improve.other`)",
    "(`how_engaged_by_office.other` == TRUE | `how_engaged_by_office.other` == 1 ) & !is.na(`how_engaged_by_office.other`)",
    "consent_telephone_number == \"yes\"", "!is.na(shelter_occupation_other) & (shelter_occupation!=\"other\")",
    "!is.na(pay_water_charges_why_not_other) & (pay_water_charges_why_not!=\"other\")",
    "!is.na(problems_water_who_appr_not_other) & (problems_water_who_appr_not!=\"other\")",
    "!is.na(problems_water_main_complaint_other) & (problems_water_main_complaint!=\"other\")",
    "!is.na(level_service_why_poor_other) & (level_service_why_poor!=\"other\")",
    "!is.na(trust_water_office_why_not) & (trust_water_office!=\"no\")",
    "!is.na(prefer_not_engage_other) & (prefer_not_engage!=\"other\")",
    "is.na(shelter_occupation_other) & (shelter_occupation==\"other\")",
    "is.na(pay_water_charges_why_not_other) & (pay_water_charges_why_not==\"other\")",
    "is.na(problems_water_who_appr_not_other) & (problems_water_who_appr_not==\"other\")",
    "is.na(problems_water_main_complaint_other) & (problems_water_main_complaint==\"other\")",
    "is.na(level_service_why_poor_other) & (level_service_why_poor==\"other\")",
    "is.na(trust_water_office_why_not) & (trust_water_office==\"no\")",
    "is.na(prefer_not_engage_other) & (prefer_not_engage==\"other\")",
    "!is.na(primary_livelihood_other) & (`primary_livelihood.other`==0 |`primary_livelihood.other`==FALSE | is.na(`primary_livelihood.other`))",
    "!is.na(access_water_enough_why_not_other) & (`access_water_enough_why_not.other`==0 |`access_water_enough_why_not.other`==FALSE | is.na(`access_water_enough_why_not.other`))",
    "!is.na(suggestions_improve_services_other) & (`suggestions_improve_services.other`==0 |`suggestions_improve_services.other`==FALSE | is.na(`suggestions_improve_services.other`))",
    "is.na(primary_livelihood_other) & (`primary_livelihood.other`==1 |`primary_livelihood.other`== TRUE)",
    "is.na(access_water_enough_why_not_other) & (`access_water_enough_why_not.other`==1 |`access_water_enough_why_not.other`== TRUE)",
    "is.na(suggestions_improve_services_other) & (`suggestions_improve_services.other`==1 |`suggestions_improve_services.other`== TRUE)"
  ), description = c(
    "water_sources.other is selected but water_sources_other is not found in the dataset",
    "water_source_drinking is selected but water_source_drinking_other is not found in the dataset",
    "treat_drink_water_how.other is selected but treat_drink_water_how_other is not found in the dataset",
    "water_source_cook is selected but water_source_cook_other is not found in the dataset",
    "treat_cook_water_how.other is selected but treat_cook_water_how_other is not found in the dataset",
    "piped_quality.no_other is selected but piped_quality_other is not found in the dataset",
    "tap_quality.no_other is selected but tap_quality_other is not found in the dataset",
    "borehole_quality.no_other is selected but borehole_quality_other is not found in the dataset",
    "trucking_quality.no_other is selected but trucking_quality_other is not found in the dataset",
    "water_source_bath is selected but water_source_bath_other is not found in the dataset",
    "water_source_clothes is selected but water_source_clothes_other is not found in the dataset",
    "water_source_house is selected but water_source_house_other is not found in the dataset",
    "water_source_animals is selected but water_source_animals_other is not found in the dataset",
    "water_source_crops is selected but water_source_crops_other is not found in the dataset",
    "pump_horsepower is selected but pump_horsepower_other is not found in the dataset",
    "pay_water_charges_method is selected but pay_water_charges_method_other is not found in the dataset",
    "pay_water_charges_prefer is selected but pay_water_charges_prefer_other is not found in the dataset",
    "why_share_connection.other is selected but why_share_connection_other is not found in the dataset",
    "why_no_subscription.other is selected but why_no_subscription_other is not found in the dataset",
    "own_subscription_assist_type.other is selected but own_subscription_assist_type_other is not found in the dataset",
    "problems_water_who_appr.other is selected but problems_water_who_appr_other is not found in the dataset",
    "problems_water_who_attend.other is selected but problems_water_who_attend_other is not found in the dataset",
    "water_supply_other_neighbourhoods_how.other is selected but water_supply_other_neighbourhoods_how_other is not found in the dataset",
    "users_build_trust.other is selected but users_build_trust_other is not found in the dataset",
    "help_office_improve.other is selected but help_office_improve_other is not found in the dataset",
    "how_engaged_by_office.other is selected but how_engaged_by_office_other is not found in the dataset",
    "consent_telephone_number is selected but telephone_number is not found in the dataset",
    "shelter_occupation_other has value but the shelter_occupation column is not other(Not matching with kobo relevancy.)",
    "pay_water_charges_why_not_other has value but the pay_water_charges_why_not column is not other(Not matching with kobo relevancy.)",
    "problems_water_who_appr_not_other has value but the problems_water_who_appr_not column is not other(Not matching with kobo relevancy.)",
    "problems_water_main_complaint_other has value but the problems_water_main_complaint column is not other(Not matching with kobo relevancy.)",
    "level_service_why_poor_other has value but the level_service_why_poor column is not other(Not matching with kobo relevancy.)",
    "trust_water_office_why_not has value but the trust_water_office column is not no(Not matching with kobo relevancy.)",
    "prefer_not_engage_other has value but the prefer_not_engage column is not other(Not matching with kobo relevancy.)",
    "shelter_occupation_other is NA but the shelter_occupation column is seleted as other/relevent choice(other)",
    "pay_water_charges_why_not_other is NA but the pay_water_charges_why_not column is seleted as other/relevent choice(other)",
    "problems_water_who_appr_not_other is NA but the problems_water_who_appr_not column is seleted as other/relevent choice(other)",
    "problems_water_main_complaint_other is NA but the problems_water_main_complaint column is seleted as other/relevent choice(other)",
    "level_service_why_poor_other is NA but the level_service_why_poor column is seleted as other/relevent choice(other)",
    "trust_water_office_why_not is NA but the trust_water_office column is seleted as other/relevent choice(no)",
    "prefer_not_engage_other is NA but the prefer_not_engage column is seleted as other/relevent choice(other)",
    "primary_livelihood_other is NOT NA but the binary column ( primary_livelihood.other) is selected as FALSE/0/NA",
    "access_water_enough_why_not_other is NOT NA but the binary column ( access_water_enough_why_not.other) is selected as FALSE/0/NA",
    "suggestions_improve_services_other is NOT NA but the binary column ( suggestions_improve_services.other) is selected as FALSE/0/NA",
    "primary_livelihood_other is NA but the binary column (primary_livelihood.other)is selected as TRUE/1",
    "access_water_enough_why_not_other is NA but the binary column (access_water_enough_why_not.other)is selected as TRUE/1",
    "suggestions_improve_services_other is NA but the binary column (suggestions_improve_services.other)is selected as TRUE/1"
  ), variables_to_clean_column = c(
    "water_sources.other", "water_source_drinking",
    "treat_drink_water_how.other", "water_source_cook", "treat_cook_water_how.other",
    "piped_quality.no_other", "tap_quality.no_other", "borehole_quality.no_other",
    "trucking_quality.no_other", "water_source_bath", "water_source_clothes",
    "water_source_house", "water_source_animals", "water_source_crops",
    "pump_horsepower", "pay_water_charges_method", "pay_water_charges_prefer",
    "why_share_connection.other", "why_no_subscription.other", "own_subscription_assist_type.other",
    "problems_water_who_appr.other", "problems_water_who_attend.other",
    "water_supply_other_neighbourhoods_how.other", "users_build_trust.other",
    "help_office_improve.other", "how_engaged_by_office.other", "consent_telephone_number",
    "shelter_occupation,shelter_occupation_other", "pay_water_charges_why_not,pay_water_charges_why_not_other",
    "problems_water_who_appr_not,problems_water_who_appr_not_other",
    "problems_water_main_complaint,problems_water_main_complaint_other",
    "level_service_why_poor,level_service_why_poor_other", "trust_water_office,trust_water_office_why_not",
    "prefer_not_engage,prefer_not_engage_other", "shelter_occupation,shelter_occupation_other",
    "pay_water_charges_why_not,pay_water_charges_why_not_other",
    "problems_water_who_appr_not,problems_water_who_appr_not_other",
    "problems_water_main_complaint,problems_water_main_complaint_other",
    "level_service_why_poor,level_service_why_poor_other", "trust_water_office,trust_water_office_why_not",
    "prefer_not_engage,prefer_not_engage_other", "primary_livelihood_other,primary_livelihood.other",
    "access_water_enough_why_not_other,access_water_enough_why_not.other",
    "suggestions_improve_services_other,suggestions_improve_services.other",
    "primary_livelihood_other,primary_livelihood.other", "access_water_enough_why_not_other,access_water_enough_why_not.other",
    "suggestions_improve_services_other,suggestions_improve_services.other"
  )), row.names = c(NA, -47L), class = c("tbl_df", "tbl", "data.frame"))

  testthat::expect_equal(expected_df, unique_df)
})


testthat::test_that("Test that create_logic_for_others with both select_multiple separator", {
  actual <- create_logic_for_other(
    kobo_survey = cleaningtools_survey,
    sm_sep = "/"
  )

  expected <- create_logic_for_other(kobo_survey = cleaningtools_survey)

  actual$logic <- gsub("/", "\\.", actual$logic)
  actual$variables_to_clean_column <- gsub("/", "\\.", actual$variables_to_clean_column)

  expect_equal(actual$logic, expected$logic)

  expect_equal(actual$variables_to_clean_column, expected$variables_to_clean_column)
})
