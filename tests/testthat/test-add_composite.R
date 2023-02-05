
library(testthat)
library(tidyverse)

# prepare data ------------------------------------------------------------
df <- read.csv("../reference_files/clean_data.csv")
 # df <- read.csv("data/cleaned_data/clean_data.csv")



check <- check_fcs(dataset = df,
                   uuid = "X_uuid",
                   cereals = "cereals_grains_roots_tubers",
                   pulses = "beans_legumes_pulses_nuts",
                   dairy = "milk_dairy_products",
                   meat ="meat_fish_eggs",
                   vegetables ="vegetables",
                   fruits ="fruite",
                   oil ="oil_fat_butter",
                   sugar= "sugar_sugary_food")



df_issue_removed <- df %>% filter(!X_uuid %in% check$X_uuid) ### getting rid of potential issue

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



res<-evaluate_promise(make_fcs(dataset = df_issue_removed,var_name = "fcs_test",
                               cereals = "cereals_grains_roots_tubers",
                               pulses = "beans_legumes_pulses_nuts",
                               dairy = "milk_dairy_products",
                               meat ="meat_fish_eggs",
                               vegetables ="vegetables",
                               fruits ="fruite",
                               oil ="oil_fat_butter",
                               sugar= "sugar_sugary_food"))

# testing -----------------------------------------------------------------



testthat::test_that("Check_fcs",{
  expect_equal(nrow(res_check$result),0)

})

testthat::test_that("Check make_fcs",{

  testthat::expect_warning(make_fcs(dataset = df,var_name = "fcs_test",
                                    cereals = "cereals_grains_roots_tubers",
                                    pulses = "beans_legumes_pulses_nuts",
                                    dairy = "milk_dairy_products",
                                    meat ="meat_fish_eggs",
                                    vegetables ="vegetables",
                                    fruits ="fruite",
                                    oil ="oil_fat_butter",
                                    sugar= "sugar_sugary_food"),
                           "Potential issue:: There are 105 observations where all the variables of food consumption score are the same.")

  testthat::expect_equal(df |> ncol()+2, ncol(res$result) )

})
