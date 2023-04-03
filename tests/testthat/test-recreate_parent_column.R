test_that("recreate other columns", {


####### sm_sep = "." single "."

pre_clean_test <- dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("xx,yy","xx,zy",
               "zy","xx,xz,zy",
               NA_character_,"xz"),
    reason.xx = c(0,1,0,1,0,0),
    reason.yy = c(1,0,0,0,1,0),
    reason.xz = c(0,0,0,1,0,1),
    reason.zy = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_)

  )


# expected result
  expected_test <-  dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("yy","xx,zy",
               "zy","xx,xz,zy",
               "yy","xz"),
    reason.xx = c(0,1,0,1,0,0),
    reason.yy = c(1,0,0,0,1,0),
    reason.xz = c(0,0,0,1,0,1),
    reason.zy = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_))

  ### apply function
  testthat::expect_no_warning(recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "."))
  actual_result <- recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = ".")

  testthat::expect_equal(actual_result,expected_test)


  ####### sm_sep = "." multiple "."


  pre_clean_test <- dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("xx,yy","xx,zy",
               "zy","xx,xz,zy",
               NA_character_,"xz"),
    reason.x.x. = c(0,1,0,1,0,0),
    reason.yy = c(1,0,0,0,1,0),
    reason.x.z = c(0,0,0,1,0,1),
    reason.zy = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_)

  )

  expected_test <-  dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("yy","x_x_,zy",
               "zy","x_x_,x_z,zy",
               "yy","x_z"),
    reason.x_x_ = c(0,1,0,1,0,0),
    reason.yy = c(1,0,0,0,1,0),
    reason.x_z = c(0,0,0,1,0,1),
    reason.zy = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_))

  actual_result <- recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = ".")

  testthat::expect_equal(actual_result,expected_test)
  testthat::expect_warning(recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "."))


  ####### check sm_sep = "/" mix with "."


  pre_clean_test <- dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("x.x.,yy","x.x.,zy",
               "zy","x.x.,x.z,zy",
               NA_character_,"x.z"),
    `reason/x.x.` = c(0,1,0,1,0,0),
    `reason/yy` = c(1,0,0,0,1,0),
    `reason/x.z` = c(0,0,0,1,0,1),
    `reason/zy` = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_)

  )


  expected_test <-  dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("yy","x.x.,zy",
               "zy","x.x.,x.z,zy",
               "yy","x.z"),
    `reason/x.x.` = c(0,1,0,1,0,0),
    `reason/yy` = c(1,0,0,0,1,0),
    `reason/x.z` = c(0,0,0,1,0,1),
    `reason/zy` = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_))

  actual_result <- recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "/")

  testthat::expect_equal(actual_result,expected_test)



  ####### sm_sep = "/"with multiple "/"


  pre_clean_test <- dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("x.x.,yy","x.x.,zy",
               "zy","x.x.,x.z,zy",
               NA_character_,"x.z"),
    `reason/x.x/` = c(0,1,0,1,0,0),
    `reason/yy` = c(1,0,0,0,1,0),
    `reason/x.z` = c(0,0,0,1,0,1),
    `reason/z/y` = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_)

  )


  expected_test <-  dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("yy","x.x_,z_y",
               "z_y","x.x_,x.z,z_y",
               "yy","x.z"),
    `reason/x.x_` = c(0,1,0,1,0,0),
    `reason/yy` = c(1,0,0,0,1,0),
    `reason/x.z` = c(0,0,0,1,0,1),
    `reason/z_y` = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_))

  testthat::expect_warning(recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "/"))
  actual_result <- recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "/")

  testthat::expect_equal(actual_result,expected_test)



  ##### Check with KObo

  survey_sheet <- dplyr::tibble(type = "select_multiple xxx",
                         name = "reason")
  choice_sheet <- dplyr::tibble(list_name =c("xxx","xxx","xxx","xxx"),
                                name = c("x/x","y.y","xz","z_y"))

  pre_clean_test <- dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("x/x,y.y","x/x,z_y",
               "z_y","x/x,xz,z_y",
               NA_character_,"xz"),
    `reason/x/x` = c(0,1,0,1,0,0),
    `reason/y.y` = c(1,0,0,0,1,0),
    `reason/xz` = c(0,0,0,1,0,1),
    `reason/z_y` = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_)

  )


  expected_result <- dplyr::tibble(
    uuid = paste0("uuid_",1:6),
    gender = rep(c("male","female"),3),
    reason = c("y.y","x/x,z_y",
               "z_y","x/x,xz,z_y",
               "y.y","xz"),
    `reason/x/x` = c(0,1,0,1,0,0),
    `reason/y.y` = c(1,0,0,0,1,0),
    `reason/xz` = c(0,0,0,1,0,1),
    `reason/z_y` = c(0,1,1,1,0,0),
    reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_)

  )


  actual_result <- recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "/",
                                          kobo_survey_sheet = survey_sheet,
                                          kobo_choices_sheet = choice_sheet)

  testthat::expect_equal(expected_result,actual_result)

  ### check error message
  choice_sheet <- dplyr::tibble(list_name =c("xxx","xxx","xxx","xxx"),
                                name = c("x/x","y.y","xz","x_y")) ## checking missing column
  testthat::expect_warning(recreate_parent_column(df = pre_clean_test,uuid = "uuid",sm_sep = "/",
                                      kobo_survey_sheet = survey_sheet,
                                      kobo_choices_sheet = choice_sheet))


})


