library(testthat)
library(dplyr)

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
#' @importFrom dplyr filter select mutate




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
    select(!!sym(uuid), all_of(fcs_cols)) %>%
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

