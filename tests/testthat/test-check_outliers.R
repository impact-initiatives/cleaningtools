library(dplyr)
library(tidyr)

test_that("Outliers check", {

  ### test cols to add

  outliers1 <- check_outliers(raw_data,columns_to_add = "X_uuid",columns_to_remove = "enumerator_num")
  outliers2 <- check_outliers(raw_data,columns_to_add = "X_uuid")

  expect_false("enumerator_num" %in% outliers1$question)
  expect_true("X_uuid" %in% names(outliers1))
  expect_true("enumerator_num" %in% outliers2$question)

  ## test with null in unique number
  outliers3 <- check_outliers(raw_data,columns_to_add = "X_uuid",minumum_unique_value_of_variable = 10)
  outliers4 <- check_outliers(raw_data,columns_to_add = "X_uuid",minumum_unique_value_of_variable = NULL)

  expect_false("air_coolers_nb" %in% outliers3$question)
  expect_true("air_coolers_nb" %in% outliers4$question)


  df_outlier<- data.frame(
    uuid = paste0("uuid_", sample(1:1000,size = 100)),
    age =c(sample(1:75,replace = T,size = 95),c(200,1,5,120,150)),
    expense = c(sample(200:500,replace = T,size = 95),c(600,100,80,1020,1050)),
    income = c(sample(20000:50000,replace = T,size = 95),c(60,0,80,1020,1050))
  )

  outliers5 <- check_outliers(df = df_outlier,columns_to_add = "uuid",strongness_factor = 1.5)

  boxplot_outlier_age <- (boxplot.stats(df_outlier$age))$out
  outliers5_age_outliers <- (outliers5[outliers5$question == "age",])$old_value
  expect_true(all(boxplot_outlier_age %in% outliers5_age_outliers ))

  outliers6 <- check_outliers(df = df_outlier,columns_to_add = "uuid",strongness_factor = 3)

  expect_true(nrow(outliers5) > nrow(outliers6))



})



