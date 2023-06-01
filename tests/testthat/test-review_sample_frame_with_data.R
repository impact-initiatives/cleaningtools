test_that("review_sample_frame", {

  cl <- cleaningtools::cleaningtools_clean_data |> dplyr::mutate(
    consent = c(rep("no",10),rep("yes",546),rep("yes_yes",5),rep("no_no",17)
    ))

  #######   check warning ############
  testthat::expect_warning(review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame,
                                                            sample_frame_strata_col = "Neighbourhood",
                                                            sample_frame_target_survey_col ="Total.no.of.HH",
                                                            clean_data = cl,
                                                            clean_data_strata_column = "neighbourhood",
                                                            consent_column = "consent",
                                                            value_for_consent_yes = c("yes_yes","yes")))

  testthat::expect_warning(review_sample_frame_with_dataset(sample_frame = (cleaningtools::cleaningtools_sample_frame |> dplyr::select(-c("Remaining"))),
                                                            sample_frame_strata_col = "Neighbourhood",
                                                            sample_frame_target_survey_col ="Total.no.of.HH",
                                                            clean_data = cl,
                                                            clean_data_strata_column = "neighbourhood",
                                                            consent_column = "consent",
                                                            value_for_consent_yes = c("yes_yes","yes")))

  testthat::expect_no_warning(review_sample_frame_with_dataset(sample_frame = (cleaningtools::cleaningtools_sample_frame |> dplyr::select(-c("Remaining","Collected"))),
                                                               sample_frame_strata_col = "Neighbourhood",
                                                               sample_frame_target_survey_col ="Total.no.of.HH",
                                                               clean_data = cl,
                                                               clean_data_strata_column = "neighbourhood",
                                                               consent_column = "consent",
                                                               value_for_consent_yes = c("yes_yes","yes")))

  ### check error
  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame,
                                                          sample_frame_strata_col = "Neghbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "Neghbourhood not found in the sample frame")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total_no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "Total_no.of.HH not found in the sample frame")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neghbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "neghbourhood not found in the clean data")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "cnsent",
                                                          value_for_consent_yes = c("yes_yes","yes")),
                         "cnsent not found in the clean data")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame,
                                                          sample_frame_strata_col = "Neighbourhood",
                                                          sample_frame_target_survey_col ="Total.no.of.HH",
                                                          clean_data = cl,
                                                          clean_data_strata_column = "neighbourhood",
                                                          consent_column = "consent",
                                                          value_for_consent_yes = c("yesyes","yes")),
                         "yesyes not found in the consent column")


  ## strata checks

  cl2 <- cl
  cl2$neighbourhood <- cl2$neighbourhood |> stringr::str_replace_all("A1","al_ry111") |>
    stringr::str_replace_all("Bx1","sdsdsfdfdfdfsdadsgvs")

  testthat::expect_error(review_sample_frame_with_dataset(sample_frame = (cleaningtools::cleaningtools_sample_frame |> dplyr::select(-c("Remaining","Collected"))),
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
                             Neighbourhood = c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "c", "Bx1",
                                               "Bx2", "Bdd1", "Bdd2", "cc", "Bcccc", "Bccccccc_1", "Bccccccc_2",
                                               "r1", "tr2", "tr3", "tr4", "oyt", "frgdf", "rtrtrtr_1", "rtrtrtr_2",
                                               "fdfddfdfdfd_1", "ofhfyt", "yttytyty", "ghghg", "sdddddds", "dddddddd",
                                               "eewr", "rfbv_1", "rfbv_3", "ftgtrh_1", "ftgtrh_2", "ytjhfsd1",
                                               "ytjhfsd2", "ytjhfsd3", "dgfd", "hgh", "ethgfbfds"),
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




  actual <- review_sample_frame_with_dataset(sample_frame = (cleaningtools::cleaningtools_sample_frame |> dplyr::select(-c("Remaining","Collected"))),
                                             sample_frame_strata_col = "Neighbourhood",
                                             sample_frame_target_survey_col ="Total.no.of.HH",
                                             clean_data = cl,
                                             clean_data_strata_column = "neighbourhood",
                                             consent_column = "consent",
                                             value_for_consent_yes = c("yes_yes","yes"))
  testthat::expect_equal(actual,expected)

})
