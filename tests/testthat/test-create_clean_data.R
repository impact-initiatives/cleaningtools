
testthat::test_that("implement cleaning log",{

  cleaningtools_clean_data_fi <- create_clean_data(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                                   cl = cleaningtools::cleaningtools_cleaning_log,
                                                   cl_change_type_col = "change_type",
                                                   cl_change_col =  "questions",cl_new_val = "new_value",
                                                   cl_uuid = "X_uuid")
  expect_equal(nrow(cleaningtools_clean_data_fi),nrow(cleaningtools::cleaningtools_clean_data))


  ## check_multiple input
  cleaning_log_1 <- cleaningtools::cleaningtools_cleaning_log
  cleaning_log_1$change_type[[2]] <- "change_res"
  cleaning_log_1$change_type[[3]] <- "Chane_res"
  cleaning_log_1$change_type[[30]]<- "no_change"
  cleaning_log_1$change_type[[724]] <- "blank"
  cleaning_log_1$change_type[[646]] <- "delete"




  cleaningtools_clean_data_multiple <- create_clean_data(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                                         cl = cleaning_log_1,
                                                         cl_change_type_col = "change_type",
                                                         values_for_change_response = c("change_res","Chane_res","change_response"),
                                                         values_for_blank_response = c("blank_response","blank"),
                                                         values_for_no_change = c("no_action","no_change"),
                                                         values_for_remove_survey =  c("remove_survey","delete"),
                                                         cl_change_col =  "questions",cl_new_val = "new_value",
                                                         cl_uuid = "X_uuid")

  cleaning_log_1_log_only <- cleaning_log_1 |> dplyr::filter(!change_type %in% c("remove_survey","delete"))
  deletation_log <- cleaning_log_1 |> dplyr::filter(change_type %in% c("remove_survey","delete"))


  expect_equal(nrow(review_cleaning_log(raw_data = cleaningtools::cleaningtools_raw_data,
                                        raw_data_uuid = "X_uuid",
                                        clean_data = cleaningtools_clean_data_multiple,
                                        clean_data_uuid = "X_uuid",
                                        cleaning_log = cleaning_log_1_log_only,
                                        cleaning_log_uuid = "X_uuid",
                                        cleaning_log_question_name = "questions",
                                        cleaning_log_new_value = "new_value",
                                        cleaning_log_old_value = "old_value",
                                        deletion_log = deletation_log,
                                        deletion_log_uuid = "X_uuid",
                                        check_for_deletion_log = T
  ) |> dplyr::filter(df.change_type != "no_action")),3)

  #####
  #
  # a <-  review_cleaning_log(cleaningtools_raw_data = cleaningtools::cleaningtools_raw_data,cleaningtools_raw_data_uuid = "X_uuid",
  #                      cleaningtools_clean_data = cleaningtools_clean_data_multiple,
  #                      cleaningtools_clean_data_uuid = "X_uuid",cleaning_log = cleaning_log_1_log_only,
  #                      cleaning_log_uuid = "X_uuid",
  #                      cleaning_log_question_name = "questions",
  #                      cleaning_log_new_value = "new_value",
  #                      cleaning_log_old_value = "old_value",
  #                      deletion_log = deletation_log,
  #                      deletion_log_uuid = "X_uuid",
  #                      check_for_deletion_log = T
  #  )|> dplyr::filter(df.change_type != "no_action")


  #####

  ## check change response
  # change_response
  expect_equal(cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "728e4de0-7356-4bb5-9db4-9479b3ffe098",]$treat_drink_water,"never_treat")
  # change_res
  expect_equal(cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "fcc6b9e8-c72a-4228-8dc0-6e803114b6d6",]$water_tank_litres_nb,7000)
  # remove
  expect_false("248f7774-319a-4368-91d2-a1f8dec9972d" %in%  cleaningtools_clean_data_multiple$X_uuid)
  # delete
  expect_false("683e97a1-a6df-4442-b1ba-effe04612b86" %in%  cleaningtools_clean_data_multiple$X_uuid)

  #no_action
  expect_equal(cleaningtools::cleaningtools_raw_data[cleaningtools::cleaningtools_raw_data$X_uuid == "bc6df6b9-c888-4cd3-9198-a52f3a1eb677","air_coolers_hours"],
               cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "bc6df6b9-c888-4cd3-9198-a52f3a1eb677","air_coolers_hours"])

  # no change
  expect_equal(cleaningtools::cleaningtools_raw_data[cleaningtools::cleaningtools_raw_data$X_uuid == "f949f8ee-6536-4627-a850-dc0b092ba379","air_coolers_nb"],
               cleaningtools_clean_data_multiple[cleaningtools_clean_data_multiple$X_uuid == "f949f8ee-6536-4627-a850-dc0b092ba379","air_coolers_nb"])


  #### NA check
  cleaning_log_na <- cleaningtools::cleaningtools_cleaning_log
  cleaning_log_na$change_type[[1]] <- NA

  expect_error(create_clean_data(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                 cl = cleaning_log_na,
                                 cl_change_type_col = "change_type",
                                 values_for_no_change =c("no_action",""),
                                 cl_change_col =  "questions",cl_new_val = "new_value",
                                 cl_uuid = "X_uuid"), "Missing values in change_type")

  expect_equal(nrow(check_cleaning_log(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                       cl = cleaning_log_na,
                                       cl_change_type_col = "change_type",
                                       cl_change_col =  "questions",cl_new_val = "new_value",
                                       cl_uuid = "X_uuid")),1)

  ### wrong question
  cleaning_log_wrong <- cleaningtools::cleaningtools_cleaning_log %>% dplyr::mutate(
    questions = cleaningtools::cleaningtools_cleaning_log$questions %>% stringr::str_replace_all("treat_drink_water","treat_drink_water2"))


  expect_error(create_clean_data(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                 cl = cleaning_log_wrong,
                                 cl_change_type_col = "change_type",
                                 cl_change_col =  "questions",cl_new_val = "new_value",
                                 cl_uuid = "X_uuid"), "Make sure all names in cl_change_col values in the cleaning log are in dataset")


  expect_equal(nrow(check_cleaning_log(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                       cl = cleaning_log_wrong,
                                       values_for_change_response = "change_response",

                                       cl_change_type_col = "change_type",
                                       cl_change_col =  "questions",cl_new_val = "new_value",
                                       cl_uuid = "X_uuid")),4)


  expect_output(check_cleaning_log(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                   cl = cleaning_log_wrong,
                                   values_for_change_response = "change_response",
                                   cl_change_type_col = "change_type",
                                   cl_change_col =  "questions",cl_new_val = "new_value",
                                   cl_uuid = "X_uuid"),"cleaning log has issues, see output table")


  expect_error(create_clean_data(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                 cl = cleaningtools::cleaningtools_cleaning_log,
                                 cl_change_type_col = "change_type",
                                 change_type_for_blank_response = "c",
                                 cl_change_col =  "questions",cl_new_val = "new_value",
                                 cl_uuid = "X_uuid"))

  expect_error(create_clean_data(df = cleaningtools::cleaningtools_raw_data,df_uuid = "X_uuid",
                                 cl = cleaningtools::cleaningtools_cleaning_log,
                                 cl_change_type_col = "change_ype",
                                 cl_change_col =  "questions",cl_new_val = "new_value",
                                 cl_uuid = "X_uuid"), "cl_change_type_col column not found in cleaning log")


})


testthat::test_that("check cleaning log",{

  expect_no_error(check_cleaning_log(df = cleaningtools::cleaningtools_raw_data,
                                     df_uuid = "X_uuid",
                                     cl = cleaningtools::cleaningtools_cleaning_log,
                                     cl_change_type_col = "change_type",
                                     values_for_change_response = "change_response",
                                     cl_change_col = "questions",
                                     cl_uuid = "X_uuid",
                                     cl_new_val = "new_value"))

  expect_error(check_cleaning_log(df = cleaningtools::cleaningtools_raw_data,
                                  df_uuid = "X_uuid",
                                  cl = cleaningtools::cleaningtools_cleaning_log,
                                  cl_change_type_col = "change_type",
                                  values_for_change_response = "change_esponse",cl_change_col = "questions",
                                  cl_uuid = "X_uuid",cl_new_val = "new_value"), "Value in values_for_change_response not found")

  expect_error(check_cleaning_log(df = cleaningtools::cleaningtools_raw_data,
                                  df_uuid = "X_uuid",
                                  cl = cleaningtools::cleaningtools_cleaning_log,
                                  cl_change_type_col = "chage_type",
                                  values_for_change_response = "change_response",cl_change_col = "questions",
                                  cl_uuid = "X_uuid",cl_new_val = "new_value"), "cl_change_type_col column not found in cleaning log")


})

