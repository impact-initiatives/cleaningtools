test_that("create logic for other", {
  expect_equal(2 * 2, 4)

 logic_df <- create_logic_for_other(kobo_survey = cleaningtools::cleaningtools_survey,
                         sm_sep = ".",
                         data = cleaningtools::cleaningtools_clean_data,
                         compare_with_dataset = T)

  testthat::expect_warning(create_logic_for_other(kobo_survey = cleaningtools::cleaningtools_survey,
                                      sm_sep = ".",
                                      data = cleaningtools::cleaningtools_clean_data,
                                      compare_with_dataset = T))
  testthat::expect_error(create_logic_for_other(kobo_survey = cleaningtools::cleaningtools_survey,
                                                  sm_sep = ".",
                                                  compare_with_dataset = T))

  testthat::expect_equal(nrow(logic_df),47)


  # Other/choice is not select in parent column but text/_other column not found in the dataset
  logic_filter_1 <-  c("(water_sources.other == TRUE | water_sources.other == 1 ) & !is.na(water_sources.other)"
  ,paste0("water_source_drinking == ", "\"","other","\""))

  logic_df_filter_1 <- logic_df |> dplyr::filter(description == "Other/choice is not select in parent column but text/_other column not found in the dataset")

  testthat::expect_true(all(logic_filter_1 %in% logic_df_filter_1$logic))


  # "Text column has value but the parent column is not corresponding with the relevancy"

  logic_df_filter_2 <- logic_df |>
    dplyr::filter(description == "Text column has value but the parent column is not corresponding with the relevancy")


  logic_filter_2 <-  c("!is.na(shelter_occupation_other) & (shelter_occupation!=\"other\")")
  testthat::expect_true(all(logic_filter_2 %in% logic_df_filter_2$logic))


  # "Text column is NA but the parent column is seleted as other/relevent choice"
  logic_df_filter_3 <- logic_df |>
    dplyr::filter(description == "Text column is NA but the parent column is seleted as other/relevent choice")

  logic_filter_3 <-  c("is.na(shelter_occupation_other) & (shelter_occupation==\"other\")")
  testthat::expect_true(all(logic_filter_3 %in% logic_df_filter_3$logic))


  # "Text column is NOT NA but the binary column is selected as FALSE/0/NA"
  logic_df_filter_4 <- logic_df |>
    dplyr::filter(description == "Text column is NOT NA but the binary column is selected as FALSE/0/NA")
  logic_filter_4 <-  c("!is.na(primary_livelihood_other) & (primary_livelihood.other==0 |primary_livelihood.other==FALSE | is.na(primary_livelihood.other))")
  testthat::expect_true(all(logic_filter_4 %in% logic_df_filter_4$logic))



  # "Text column is NA but the binary column is selected as TRUE/1"
  logic_df_filter_5 <- logic_df |>
    dplyr::filter(description == "Text column is NA but the binary column is selected as TRUE/1")
  logic_filter_5 <-  c("is.na(primary_livelihood_other) & (primary_livelihood.other==1 |primary_livelihood.other== TRUE)")
  testthat::expect_true(all(logic_filter_5 %in% logic_df_filter_5$logic))


})

test_that("check with compare with data is FALSE", {
  logic_df <- create_logic_for_other(kobo_survey = cleaningtools::cleaningtools_survey,
                                     sm_sep = ".")
  testthat::expect_false("Other/choice is not select in parent column but text/_other column not found in the dataset" %in% logic_df$description)

})

test_that("check sm_spe", {
  logic_df <- create_logic_for_other(kobo_survey = cleaningtools::cleaningtools_survey,
                                     sm_sep = "/")
  testthat::expect_false("Other/choice is not select in parent column but text/_other column not found in the dataset" %in% logic_df$description)


  # "Text column has value but the parent column is not corresponding with the relevancy"

  logic_df_filter_2 <- logic_df |>
    dplyr::filter(description == "Text column has value but the parent column is not corresponding with the relevancy")


  logic_filter_2 <-  c("!is.na(shelter_occupation_other) & (shelter_occupation!=\"other\")")
  testthat::expect_true(all(logic_filter_2 %in% logic_df_filter_2$logic))


  # "Text column is NA but the parent column is seleted as other/relevent choice"
  logic_df_filter_3 <- logic_df |>
    dplyr::filter(description == "Text column is NA but the parent column is seleted as other/relevent choice")

  logic_filter_3 <-  c("is.na(shelter_occupation_other) & (shelter_occupation==\"other\")")
  testthat::expect_true(all(logic_filter_3 %in% logic_df_filter_3$logic))


  # "Text column is NOT NA but the binary column is selected as FALSE/0/NA"
  logic_df_filter_4 <- logic_df |>
    dplyr::filter(description == "Text column is NOT NA but the binary column is selected as FALSE/0/NA")
  logic_filter_4 <-  c("!is.na(primary_livelihood_other) & (primary_livelihood/other==0 |primary_livelihood/other==FALSE | is.na(primary_livelihood/other))")
  testthat::expect_true(all(logic_filter_4 %in% logic_df_filter_4$logic))



  # "Text column is NA but the binary column is selected as TRUE/1"
  logic_df_filter_5 <- logic_df |>
    dplyr::filter(description == "Text column is NA but the binary column is selected as TRUE/1")
  logic_filter_5 <-  c("is.na(primary_livelihood_other) & (primary_livelihood/other==1 |primary_livelihood/other== TRUE)")
  testthat::expect_true(all(logic_filter_5 %in% logic_df_filter_5$logic))


})


test_that("expect equal", {
  logic_df <- create_logic_for_other(kobo_survey = cleaningtools::cleaningtools_survey,
                                     sm_sep = ".",
                                     data = cleaningtools::cleaningtools_clean_data,
                                     compare_with_dataset = T)

  unique_df <- logic_df[!duplicated(logic_df$description),]


  expected_df <- structure(list(id = c("id- 1", "id- 28", "id- 35", "id- 42",
                        "id- 45"), logic = c("(water_sources.other == TRUE | water_sources.other == 1 ) & !is.na(water_sources.other)",
                                             "!is.na(shelter_occupation_other) & (shelter_occupation!=\"other\")",
                                             "is.na(shelter_occupation_other) & (shelter_occupation==\"other\")",
                                             "!is.na(primary_livelihood_other) & (primary_livelihood.other==0 |primary_livelihood.other==FALSE | is.na(primary_livelihood.other))",
                                             "is.na(primary_livelihood_other) & (primary_livelihood.other==1 |primary_livelihood.other== TRUE)"
                        ), description = c("Other/choice is not select in parent column but text/_other column not found in the dataset",
                                           "Text column has value but the parent column is not corresponding with the relevancy",
                                           "Text column is NA but the parent column is seleted as other/relevent choice",
                                           "Text column is NOT NA but the binary column is selected as FALSE/0/NA",
                                           "Text column is NA but the binary column is selected as TRUE/1"
                        ), variables_to_clean_column = c("water_sources.other,water_sources_other",
                                                         "shelter_occupation,shelter_occupation_other", "shelter_occupation,shelter_occupation_other",
                                                         "primary_livelihood_other,primary_livelihood.other", "primary_livelihood_other,primary_livelihood.other"
                        )), row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"
                        ))

  testthat::expect_equal(expected_df,unique_df)


})



