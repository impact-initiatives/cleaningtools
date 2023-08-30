# testing fcs-----------------------------------------------------------------

testthat::test_that("Check_fcs", {
  check <- evaluate_promise(check_fcs(
    dataset = cleaningtools_food_consumption_df,
    uuid_column = "X_uuid",
    cereals_column = "cereals_grains_roots_tubers",
    pulses_column = "beans_legumes_pulses_nuts",
    dairy_column = "milk_dairy_products",
    meat_column = "meat_fish_eggs",
    vegetables_column = "vegetables",
    fruits_column = "fruite",
    oil_column = "oil_fat_butter",
    sugar_column = "sugar_sugary_food"
  ))



  df_issue_removed <- cleaningtools_food_consumption_df %>% dplyr::filter(!X_uuid %in% check$result$X_uuid) ### getting rid of potential issue

  res_check <- evaluate_promise(check_fcs(
    dataset = df_issue_removed,
    uuid_column = "X_uuid",
    cereals_column = "cereals_grains_roots_tubers",
    pulses_column = "beans_legumes_pulses_nuts",
    dairy_column = "milk_dairy_products",
    meat_column = "meat_fish_eggs",
    vegetables_column = "vegetables",
    fruits_column = "fruite",
    oil_column = "oil_fat_butter",
    sugar_column = "sugar_sugary_food"
  ))


  expect_equal(nrow(res_check$result), 0)
})
