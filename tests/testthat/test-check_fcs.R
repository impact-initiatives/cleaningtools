library(dplyr)
library(testthat)

# testing fcs-----------------------------------------------------------------

testthat::test_that("Check_fcs",{

  check <- evaluate_promise(check_fcs(dataset = cleaningtools_food_consumption_df,
                                      uuid = "X_uuid",
                                      cereals = "cereals_grains_roots_tubers",
                                      pulses = "beans_legumes_pulses_nuts",
                                      dairy = "milk_dairy_products",
                                      meat ="meat_fish_eggs",
                                      vegetables ="vegetables",
                                      fruits ="fruite",
                                      oil ="oil_fat_butter",
                                      sugar= "sugar_sugary_food"))



  df_issue_removed <- cleaningtools_food_consumption_df %>% filter(!X_uuid %in% check$result$X_uuid) ### getting rid of potential issue

  res_check <- evaluate_promise(check_fcs(dataset = df_issue_removed,
                                          uuid = "X_uuid",
                                          cereals = "cereals_grains_roots_tubers",
                                          pulses = "beans_legumes_pulses_nuts",
                                          dairy = "milk_dairy_products",
                                          meat ="meat_fish_eggs",
                                          vegetables ="vegetables",
                                          fruits ="fruite",
                                          oil ="oil_fat_butter",
                                          sugar= "sugar_sugary_food"))


  expect_equal(nrow(res_check$result),0)

})
