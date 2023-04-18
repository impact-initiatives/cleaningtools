test_that("review_sample_frame", {

  cl <- cleaningtools::clean_data |> dplyr::mutate(
    consent = c(rep("no",10),rep("yes",546),rep("yes_yes",5),rep("no_no",17)
    ))

  #######   check warning ############
  testthat::expect_warning(review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame,
                                                            sample_frame_strata_col = "Neighbourhood",
                                                            sample_frame_target_survey_col ="Total.no.of.HH",
                                                            clean_data = cl,
                                                            clean_data_strata_column = "neighbourhood",
                                                            consent_column = "consent",
                                                            value_for_consent_yes = c("yes_yes","yes")))

  testthat::expect_warning(review_sample_frame_with_dataset(sample_frame = (cleaningtools::sample_frame |> dplyr::select(-c("Remaining"))),
                                                            sample_frame_strata_col = "Neighbourhood",
                                                            sample_frame_target_survey_col ="Total.no.of.HH",
                                                            clean_data = cl,
                                                            clean_data_strata_column = "neighbourhood",
                                                            consent_column = "consent",
                                                            value_for_consent_yes = c("yes_yes","yes")))

  testthat::expect_no_warning(review_sample_frame_with_dataset(sample_frame = (cleaningtools::sample_frame |> dplyr::select(-c("Remaining","Collected"))),
                                                               sample_frame_strata_col = "Neighbourhood",
                                                               sample_frame_target_survey_col ="Total.no.of.HH",
                                                               clean_data = cl,
                                                               clean_data_strata_column = "neighbourhood",
                                                               consent_column = "consent",
                                                               value_for_consent_yes = c("yes_yes","yes")))

  ### check error
  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame,
                                                          sample_frame_strata_col = "Neghbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "Neghbourhood not found in the sample frame")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total_no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "Total_no.of.HH not found in the sample frame")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neghbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "neghbourhood not found in the clean data")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "cnsent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "cnsent not found in the clean data")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yesyes","yes")),
                         "yesyes not found in the consent column")


  ## strata checks

  cl2 <- cl
  cl2$neighbourhood <- cl2$neighbourhood |> stringr::str_replace_all("al_askary1","al_askary111") |>
    stringr::str_replace_all("askari_2","aaaskari_2")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = (cleaningtools::sample_frame |> dplyr::select(-c("Remaining","Collected"))),
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl2,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "The above strata was not found in the sample frame.")

  ### expect equal
  expected <- structure(list(Managed.by = c("Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ", "Talafar ",
                                            "Talafar ", "Talafar ", "Talafar "),
                             Governorate = c("Ninewa",
                                             "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa",
                                             "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa",
                                             "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa",
                                             "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa",
                                             "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa",
                                             "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa", "Ninewa"),
                             Neighbourhood = c("al_askary1", "al_askary2", "al_askary3",
                                               "al_askary4", "al_jazeera1", "al_jazeera2", "al_jazeera3",
                                               "al_jazeera4", "al_kefah_al_shimaly", "al_khadraa1", "al_khadraa2",
                                               "al_kifah_aljinubi1", "al_kifah_aljinubi2", "al_mualimin",
                                               "al_muntadhar", "al_muthana_1", "al_muthana_2", "al_naser1",
                                               "al_nasir2", "al_nasir3", "al_nasir4", "al_needa", "al_noor",
                                               "al_qadissiya_1", "al_qadissiya_2", "al_qala_1", "al_rabea",
                                               "al_sada_village", "al_salam", "al_senaa_al_shamaliya", "al_sinaa_south",
                                               "al_tahrir", "al_taleaa_1", "al_taleaa_3", "al_uruba_1",
                                               "al_uruba_2", "al_wahda1", "al_wahda2", "al_wahda3", "al_zahra",
                                               "askari_2", "saad"),
                             Total.no.of.HH = c(22L, 19L, 5L, 6L,
                                                12L, 15L, 5L, 2L, 27L, 13L, 10L, 9L, 11L, 16L, 11L, 14L,
                                                14L, 4L, 27L, 14L, 8L, 35L, 23L, 18L, 24L, 14L, 13L, 6L,
                                                15L, 1L, 5L, 7L, 11L, 10L, 13L, 17L, 20L, 18L, 7L, 14L, 19L,
                                                24L), Collected = c(22, 19, 5, 6, 12, 15, 5, 2, 27, 13, 10,
                                                                    9, 11, 16, 11, 14, 14, 4, 27, 14, 8, 35, 23, 18, 24, 14,
                                                                    12, 6, 15, 0, 5, 7, 9, 10, 13, 17, 20, 11, 7, 14, 3, 24),
                             Remaining = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0,
                                           0, 0, 0, 7, 0, 0, 16, 0)), class = "data.frame", row.names = c(NA, -42L))




  actual <- review_sample_frame_with_dataset(sample_frame = (cleaningtools::sample_frame |> dplyr::select(-c("Remaining","Collected"))),
                                             sample_frame_strata_col = "Neighbourhood",
                                             sample_frame_target_survey_col ="Total.no.of.HH",
                                             clean_data = cl,
                                             clean_data_strata_column = "neighbourhood",
                                             consent_column = "consent",
                                             value_for_consent_yes = c("yes_yes","yes"))
  testthat::expect_equal(actual,expected)

})
