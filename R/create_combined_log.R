#' Merging the cleaning logs
#'
#' @param list_of_log A list file containig all the logs
#' @param dataset_name The dataset's name, if it exists in the list_of_log
#'
#' @return Combined cleaning log
#' @export
#'
#' @examples
#' list <- cleaningtools::cleaningtools_raw_data |>
#'   check_pii(uuid_column = "X_uuid") |>
#'   check_duplicate(uuid_column = "X_uuid") |>
#'   check_value(uuid_column = "X_uuid")
#'
#' create_combined_log(list_of_log = list)
#'
create_combined_log <- function(list_of_log,
                                dataset_name = "checked_dataset") {
  ## log must be a list

  if (is.data.frame(list_of_log) | is.character(list_of_log)) {
    stop(glue::glue("list_of_log must be a list which should contain the logs."))
  }

  ## look for dataset name
  if (!is.null(dataset_name)) {
    if (!dataset_name %in% names(list_of_log)) {
      stop(glue::glue(dataset_name, " can not be found in the list_of_log."))
    }
  }

  if (is.null(dataset_name) & "checked_dataset" %in% names(list_of_log)) {
    warning(glue::glue("You have a checked_dataset element in the list_of_log even though you have set dataset_name to NULL. Please check the parameter."))
  }

  if (is.null(dataset_name) & !"checked_dataset" %in% names(list_of_log)) {
    message(glue::glue("No dataset name is provided. Assuming that the dataset does not exist in the list_of_log."))
  }



  output <- list()

  if (!is.null(dataset_name)) {
    output[["checked_dataset"]] <- list_of_log[[dataset_name]]
  }

  list_of_log_only <- list_of_log[names(list_of_log)[!names(list_of_log) %in% dataset_name]]


  list_of_log_only <- list_of_log_only %>%
    purrr::map(.f = ~ dplyr::mutate(., dplyr::across(
      .cols = tidyselect::everything(),
      .fns = ~ format(., scientific = F, justify = "none", trim = T)
    )))

  print(names(list_of_log) |> glue::glue_collapse(", ") %>% glue::glue("List of element to combine- ", .))

  output[["cleaning_log"]] <- dplyr::bind_rows(list_of_log_only) |>
    dplyr::mutate(
      change_type = NA_character_,
      new_value = NA_character_
      )

  if(is.null(output[["cleaning_log"]][["check_binding"]])) {
    output[["cleaning_log"]] <- output[["cleaning_log"]] |>
      dplyr::mutate(
        check_binding = paste(question, uuid, sep = " ~/~ ")
      )
  } else {
    output[["cleaning_log"]] <- output[["cleaning_log"]] |>
      dplyr::mutate(
        check_binding = dplyr::case_when(is.na(check_binding) ~ paste(question, uuid, sep = " ~/~ "),
                                         TRUE ~  check_binding)
      )

  }

  output
}
