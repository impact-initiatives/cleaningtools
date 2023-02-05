#' Add the household hunger scale to the dataset
#'
#' Adds the 6 columns used for the Household Hunger Scale (HHS) to a dataframe.
#'
#' @param dataset A dataframe
#' @param new_colname The prefix for the new columns. It has to be a string.
#' @param new_category_label A vector of 5 strings that will replace the default labels
#' c("none", "little", "moderate", "severe", "very_severe")
#' @param hhs1a The name of the column "In the past 4 weeks (30 days), was there ever no food to eat
#' of any kind in your house because of lack of resources to get food?". It has to be a string.
#' @param hhs1b The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hhs2a The name of the column "In the past 4 weeks (30 days), did you or any household
#' member go to sleep at night hungry because there was not enough food?". It has to be a string.
#' @param hhs2b The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hhs3a The name of the column "In the past 4 weeks (30 days), did you or any household
#' member go a whole day and night without eating anything at all because there was not enough food?".
#' It has to be a string.
#' @param hhs3b The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param yesno_choice A vector of 2 strings from the choices for the yes/no set of questions in
#' this order: c("yes", "no")
#' @param frequency_choice A vector of 3 strings from the choices for the frequency set of questions
#' in this order:  c("rarely_1_2", "sometimes_3_10", "often_10_times")
#'
#' @return It returns the dataframe with 5 extras columns: score for the 3 sets of questions
#' (from 0 to 2), the HHS score (from 0 to 6) and the HHS category.
#' @export
#'
#' @examples
add_hhs <- function(dataset,
                    new_colname = "hhs",
                    new_category_label = c("none", "little", "moderate", "severe", "very_severe"),
                    hhs1a = "fs_hhs_nofood_yn",
                    hhs1b = "fs_hhs_nofood_freq",
                    hhs2a = "fs_hhs_sleephungry_yn",
                    hhs2b = "fs_hhs_sleephungry_freq",
                    hhs3a = "fs_hhs_daynoteating_yn",
                    hhs3b = "fs_hhs_daynoteating_freq",
                    yesno_choice = c("yes", "no"),
                    frequency_choice = c("rarely_1_2", "sometimes_3_10", "often_10_times")) {
  score1 <- paste0(new_colname, "_1")
  score2 <- paste0(new_colname, "_2")
  score3 <- paste0(new_colname, "_3")
  score_name <- paste0(new_colname, "_score")
  cat_name <- paste0(new_colname, "_cat")

  dataset %>%
    # individual scores are calculated to be able to deal with NA in the score creation
    mutate(
      !!sym(score1) := recoding_hhs(!!sym(hhs1a), !!sym(hhs1b), yesno_choice, frequency_choice),
      !!sym(score2) := recoding_hhs(!!sym(hhs2a), !!sym(hhs2b), yesno_choice, frequency_choice),
      !!sym(score3) := recoding_hhs(!!sym(hhs3a), !!sym(hhs3b), yesno_choice, frequency_choice),
      !!sym(score_name) := rowSums(across(c(!!sym(score1), !!sym(score2), !!sym(score3))),
        na.rm = F
      ), # not calculating if there is NA
      !!sym(cat_name) := case_when(
        !!sym(score_name) == 0 ~ new_category_label[1],
        !!sym(score_name) == 1 ~ new_category_label[2],
        !!sym(score_name) <= 3 ~ new_category_label[3],
        !!sym(score_name) == 4 ~ new_category_label[4],
        !!sym(score_name) <= 6 ~ new_category_label[5],
        TRUE ~ NA_character_
      )
    )
}

#' Recode the Household Hunger Score set of questions
#'
#' This is a helper for the function add_hhs
#'
#' @param hhs_yesno the yes/no column of a given set
#' @param hhs_freq the frequency column of the given set
#' @param yesno_choice A vector of 2 strings from the choices for the yes/no set of questions in
#' this order: c("yes", "no")
#' @param frequency_choice A vector of 3 strings from the choices for the frequency set of questions
#' in this order:  c("rarely_1_2", "sometimes_3_10", "often_10_times")
#'
#' @return
#' A vector of the same length from 0 to 2.
#' @export
#'
#' @examples
recoding_hhs <- function(hhs_yesno, hhs_freq, yesno_choice, frequency_choice) {
  case_when(
    {{ hhs_yesno }} == yesno_choice[2] ~ 0,
    {{ hhs_freq }} %in% frequency_choice[1:2] ~ 1,
    {{ hhs_freq }} %in% frequency_choice[3] ~ 2,
    TRUE ~ NA_real_
  )
}




#' FCS component checks
#' @param dataset Data set
#' @param uuid  Column containing unique identifier
#' @param cereals Variable name for- In the last 7 days, on how many days did your household eat cereals, grains, roots and tubers, including wild roots?
#' @param pulses Variable name for-  In the last 7 days, on how many days did your household eat any beans / legumes, pulses or nuts?
#' @param dairy Variable name for- In the last 7 days, on how many days did your household drink milk or eat other dairy products?
#' @param meat  Variable name for-  In the last 7 days, on how many days did your household eat meat, fish or eggs?
#' @param vegetables Variable name for- In the last 7 days, on how many days did your household eat vegetables or leaves, including all wild vegetables and leaves?
#' @param fruits Variable name for- In the last 7 days, on how many days did your household eat fruits, including all wild fruits?
#' @param oil Variable name for- In the last 7 days, on how many days did your household eat oil, fat, or butter?
#' @param sugar Variable name for- In the last 7 days, on how many days did your household eat sugar or sugary foods?
#' @return dataset with potential issues
#' @export
#'


check_fcs <- function(dataset,
                      uuid = "X_uuid",
                      cereals = "fs_fcs_cerealgrainroottuber",
                      pulses = "fs_fcs_beansnuts",
                      dairy = "fs_fcs_dairy",
                      meat = "fs_fcs_meatfishegg",
                      vegetables = "fs_fcs_vegetableleave",
                      fruits = "fs_fcs_fruit",
                      oil = "fs_fcs_fat",
                      sugar = "fs_fcs_sugar") {
  fcs_cols <- c(cereals, pulses, dairy, meat, vegetables, fruits, oil, sugar)

  check_df <- dataset %>%
    filter(pmax(!!!syms(fcs_cols), na.rm = T) == pmin(!!!syms(fcs_cols), na.rm = T)) %>%
    select(uuid, all_of(fcs_cols)) %>%
    mutate(
      issue = "All the vlaues of of food consumption variables are the same"
    ) ## filtering only observation where all the values are same. Didn't use if_all to consider NA.

  warring_ma <- paste0("Potential issue:: There are ", nrow(check_df), " observations where all the variables of food consumption score are the same.Check result.")

  if (nrow(check_df) > 0) {
    warning(warring_ma)
  }

  if (nrow(check_df) == 0) {
    print("No issue found")
  }
  check_df
}




#' Making FCS variables
#' @param dataset Data set
#' @param var_name  Intended FCS variable name. Note than for score variable name will be {given_name + _score} and for category, it will be {given_name + _cat}.
#' @param cereals Variable name for- In the last 7 days, on how many days did your household eat cereals, grains, roots and tubers, including wild roots?
#' @param pulses Variable name for-  In the last 7 days, on how many days did your household eat any beans / legumes, pulses or nuts?
#' @param dairy Variable name for- In the last 7 days, on how many days did your household drink milk or eat other dairy products?
#' @param meat  Variable name for-  In the last 7 days, on how many days did your household eat meat, fish or eggs?
#' @param vegetables Variable name for- In the last 7 days, on how many days did your household eat vegetables or leaves, including all wild vegetables and leaves?
#' @param fruits Variable name for- In the last 7 days, on how many days did your household eat fruits, including all wild fruits?
#' @param oil Variable name for- In the last 7 days, on how many days did your household eat oil, fat, or butter?
#' @param sugar Variable name for- In the last 7 days, on how many days did your household eat sugar or sugary foods?
#' @param threshold Threshold for FC category. Default is c(28,42). Means anything less than 29 will be poor, 29 to 42 will be bordeline and greater than 42 will be acceptable.
#' @return dataset with potential issues
#' @export
#'








make_fcs <- function(dataset,
                     var_name = "fcs",
                     cereals = "fs_fcs_cerealgrainroottuber",
                     pulses = "fs_fcs_beansnuts",
                     dairy = "fs_fcs_dairy",
                     meat = "fs_fcs_meatfishegg",
                     vegetables = "fs_fcs_vegetableleave",
                     fruits = "fs_fcs_fruit",
                     oil = "fs_fcs_fat",
                     sugar = "fs_fcs_sugar",
                     threshold = c(28, 42)) {


  # check FC values ---------------------------------------------------------


  fcs_cols <- c(cereals, pulses, dairy, meat, vegetables, fruits, oil, sugar)

  check_df <- dataset %>% filter(pmax(!!!syms(fcs_cols), na.rm = T) == pmin(!!!syms(fcs_cols), na.rm = T))
  warring_ma <- paste0("Potential issue:: There are ", nrow(check_df), " observations where all the variables of food consumption score are the same.")

  if (nrow(check_df) > 0) {
    warning(warring_ma)
  }



  # Creating FC variables  --------------------------------------------------



  var_score <- paste0(var_name, "_score")
  var_cat <- paste0(var_name, "_category")
  dataset <- dataset %>%
    mutate(
      !!sym(var_score) := !!sym(cereals) * 2 + !!sym(pulses) * 3 + !!sym(dairy) * 4 +
        !!sym(meat) * 4 + !!sym(vegetables) + !!sym(fruits) + !!sym(oil) * 0.5 + !!sym(sugar) * 0.5,
      !!sym(var_cat) := case_when(
        !!sym(var_score) <= threshold[1] ~ "poor",
        !!sym(var_score) <= threshold[2] ~ "bordeline",
        !!sym(var_score) < 112 ~ "acceptable",
        TRUE ~ NA_character_
      )
    )
  print(paste0("Variable name for food consumption score is ", var_score))
  print(paste0("Variable name for food consumption category is ", var_cat))
  return(dataset)
}
