#'
#' Check for value(s) in the dataset
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param element_name Name of the dataset in list
#' @param values_to_look Values to look. Default are- 99,999,999,88,888,888
#' @return A dataframe as cleaning log format
#' @export
#' @examples
#' df <- data.frame(
#'   X_uuid = paste0("uuid_", 1:100),
#'   age = c(sample(18:80, replace = TRUE, size = 96), 99, 99, 98, 88),
#'   gender = c("99", sample(c("male", "female"),
#'     replace = TRUE, size = 95
#'   ), "98", "98", "88", "888")
#' )
#' check_value(
#'   dataset = df, uuid_column = "X_uuid",
#'   element_name = "checked_dataset",
#'   values_to_look = c(99, 98, 88, 888)
#' )
#'
check_value <- function(dataset,
                        uuid_column = "uuid",
                        element_name = "checked_dataset",
                        values_to_look = c(99, 999, 999, 88, 888, 888)) {
  values_to_look <- values_to_look |> as.character()


  ######### checking input

  if (!is.list(dataset)) {
    stop("Input must be a dataframe or list.")
  }

  checked_dataset <- dataset

  if (!is.data.frame(dataset) & is.list(dataset)) {
    if (is.null(element_name)) {
      stop("element_name is missing")
    }
    if (!element_name %in% names(dataset)) {
      stop("element_name not found")
    }
  }

  if (!is.data.frame(dataset) & is.list(dataset)) {
    dataset <- dataset[[element_name]]
  }

  #######################


  dataset <- dataset |> dplyr::rename(uuid = !!rlang::sym(uuid_column))
  dataset <- dataset |> dplyr::mutate_all(as.character)

  dataset_only_na <- dataset |> dplyr::filter_all(dplyr::any_vars(. %in% values_to_look))

  flaged_value <- dataset_only_na |>
    tidyr::pivot_longer(cols = !uuid) |>
    dplyr::filter(value %in% values_to_look) |>
    dplyr::rename(
      question = name,
      old_value = value
    ) |>
    dplyr::mutate(issue = "Possible value to be changed to NA")

  ## create output
  if (is.data.frame(checked_dataset)) {
    return(list(
      checked_dataset = checked_dataset,
      flaged_value = flaged_value
    ))
  }

  if (!is.data.frame(checked_dataset)) {
    list_dataset <- list(flaged_value = flaged_value)

    return(append(checked_dataset, list_dataset))
  }
}
