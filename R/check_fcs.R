#' FCS component checks
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is uuid.
#' @param cereals_column Variable name for- In the last 7 days, on how many days did your household eat cereals, grains, roots and tubers, including wild roots?
#' @param pulses_column Variable name for-  In the last 7 days, on how many days did your household eat any beans / legumes, pulses or nuts?
#' @param dairy_column Variable name for- In the last 7 days, on how many days did your household drink milk or eat other dairy products?
#' @param meat_column  Variable name for-  In the last 7 days, on how many days did your household eat meat, fish or eggs?
#' @param vegetables_column Variable name for- In the last 7 days, on how many days did your household eat vegetables or leaves, including all wild vegetables and leaves?
#' @param fruits_column Variable name for- In the last 7 days, on how many days did your household eat fruits, including all wild fruits?
#' @param oil_column Variable name for- In the last 7 days, on how many days did your household eat oil, fat, or butter?
#' @param sugar_column Variable name for- In the last 7 days, on how many days did your household eat sugar or sugary foods?
#' @return dataset with potential issues
#' @export

check_fcs <- function(dataset,
                      uuid_column = "uuid",
                      cereals_column = "fs_fcs_cerealgrainroottuber",
                      pulses_column = "fs_fcs_beansnuts",
                      dairy_column = "fs_fcs_dairy",
                      meat_column = "fs_fcs_meatfishegg",
                      vegetables_column = "fs_fcs_vegetableleave",
                      fruits_column = "fs_fcs_fruit",
                      oil_column = "fs_fcs_fat",
                      sugar_column = "fs_fcs_sugar") {
  fcs_cols <- c(
    cereals_column, pulses_column, dairy_column, meat_column, vegetables_column,
    fruits_column, oil_column, sugar_column
  )

  check_df <- dataset %>%
    dplyr::filter(pmax(!!!rlang::syms(fcs_cols), na.rm = T) == pmin(!!!rlang::syms(fcs_cols), na.rm = T)) %>%
    dplyr::select(!!rlang::sym(uuid_column), dplyr::all_of(fcs_cols)) %>%
    dplyr::mutate(
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
