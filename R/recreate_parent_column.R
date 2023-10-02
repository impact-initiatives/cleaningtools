#' @name auto_detect_sm_parents
#' @rdname auto_detect_sm_parents
#' @title Detect select multiple parent columns
#' @description `auto_detect_sm_parents` is mean to detect select multiple parent columns in a way that does
#' not rely on the XLSForm as the input
#' @param dataset dataset to correct
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a list of select multiple parent columns in data set.
#' @export
auto_detect_sm_parents <- function(dataset, sm_separator = ".") {
  sm_parents <-
    sub(glue::glue(".[^\\{sm_separator}]*$"),
        "",
        colnames(dataset))
  sm_parents <- data.frame(col_names = sm_parents[sm_parents != ""])
  select_multiple_detected <- sm_parents %>%
    dplyr::group_by(col_names) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(col_names)
  return(as.character(select_multiple_detected$col_names))
}
#' @name auto_sm_parent_child
#' @rdname auto_sm_parent_child
#' @title detect and group together select multiple parent and children columns
#' @description `auto_sm_parent_children` is mean to detect select multiple parent columns & children columns in a way that does
#' not rely on the XLSForm as the input
#' @param dataset dataset to correct
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a data frame containing the the child select multiple columns alongside there parents and
#' a log with all changes recorded.
#' @export


auto_sm_parent_children <- function(dataset, sm_separator = ".") {
  sm_parents <- auto_detect_sm_parents(dataset, sm_separator)
  sm_child <- dataset %>%
    dplyr::select(dplyr::starts_with(glue::glue("{sm_parents}{sm_separator}"))) %>%
    colnames()
  dplyr::tibble(sm_parent = sub(glue::glue(".[^\\{sm_separator}]*$"), "", sm_child),
                sm_child)
}



#' This function recreates the columns for select multiple questions
#'
#' @param dataset data frame
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param kobo_survey Kobo survey sheet.
#' @param kobo_choices Kobo choices sheet.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @param cleaning_log_to_append A cleaning log where to add the changes from this functions.
#' Names of the log from this function are  "uuid", "question", "change_type", "new_value",
#' "old_value", "comment". If the cleaning_log_to_append names are not matching, the only way is to
#' create without a cleaning_log_to_append, and rename the columns and then bind.
#'
#' @export
#' @examples
#' test_data <- dplyr::tibble(
#'   uuid = paste0("uuid_", 1:6),
#'   gender = rep(c("male", "female"), 3),
#'   reason = c(
#'     "xx,yy", "xx,zy",
#'     "zy", "xx,xz,zy",
#'     NA_character_, "xz"
#'   ),
#'   reason.xx = c(0, 1, 0, 1, 0, 0),
#'   reason.yy = c(1, 0, 0, 0, 1, 0),
#'   reason.xz = c(0, 0, 0, 1, 0, 1),
#'   reason.zy = c(0, 1, 1, 1, 0, 0),
#'   reason_zy = c(NA_character_, "A", "B", "C", NA_character_, NA_character_)
#' )
#' recreate_parent_column(dataset = test_data, uuid_column = "uuid", sm_separator = ".")
#'
recreate_parent_column <- function(dataset,
                                   uuid_column = "uuid",
                                   kobo_survey = NULL,
                                   kobo_choices = NULL,
                                   sm_separator = ".",
                                   cleaning_log_to_append = NULL) {
  checked_data <- dataset

  initial_order <- names(dataset)

  if (is.null(kobo_survey)) {
    old_name <- names(dataset)
    number_of_separator <- names(dataset) |>
      stringr::str_count(pattern = paste0("\\", sm_separator)) |>
      max(na.rm = T)
    for (i in 1:number_of_separator) {
      names(dataset) <-
        sub(paste0("(\\", sm_separator, ".*?)\\", sm_separator),
            "\\1_",
            names(dataset))
    }

    cols_order <- dataset %>% names()


    difference_df <- dplyr::tibble(old_name = old_name,
                                   new_name = cols_order) |> dplyr::filter(old_name != new_name)

    if (nrow(difference_df) > 0) {
      warning(
        "Column(s) names are renamed as multiple separators are found in dataset column names. Please see the above table with the new name."
      )

      print(difference_df)
    }


    select_multiple <-
      auto_sm_parent_children(dataset, sm_separator = sm_separator)
  }

  if (!is.null(kobo_survey)) {
    choice_to_join <- kobo_choices |> dplyr::select(list_name, name)

    select_multiple <- kobo_survey |>
      dplyr::filter(grepl("select_multiple", type)) |>
      dplyr::select(type, name) |>
      dplyr::mutate(type = stringr::str_replace_all(type, "select_multiple ", "")) |>
      dplyr::rename(list_name = type,
                    sm_parent = name) |>
      dplyr::left_join(choice_to_join, multiple = "all", by = "list_name") |>
      dplyr::mutate(sm_child = paste0(sm_parent, sm_separator, name)) |>
      dplyr::select(sm_parent, sm_child)

    missing_column <-
      select_multiple$sm_child[!select_multiple$sm_child %in% names(dataset)]

    if (length(missing_column) > 0) {
      print(missing_column)
      warning(paste0(
        "Ignoring the above column(s) as they do not exist in the dataset."
      ))
    }
    select_multiple <-
      select_multiple |> dplyr::filter(sm_child %in% names(dataset))
  }

  if (nrow(select_multiple) > 0) {
    select_multiple_list <- list()

    for (i in select_multiple$sm_parent) {
      select_multi_single <-
        select_multiple %>% dplyr::filter(sm_parent == i)
      concat_col <- select_multi_single$sm_parent %>% unique()
      choice_cols <- select_multi_single$sm_child %>% unique()

      df_only_cols <-
        dataset %>% dplyr::select(dplyr::all_of(choice_cols),
                                  dplyr::all_of(uuid_column))

      pivot_long <-
        df_only_cols %>% dplyr::mutate_at(names(df_only_cols), as.character)

      final_df <- pivot_long %>%
        tidyr::pivot_longer(
          cols = !dplyr::all_of(uuid_column),
          names_to = "cols",
          values_to = "value"
        ) %>%
        dplyr::filter(value == 1 |
                        value == TRUE |
                        value == "1" | value == "TRUE") %>%
        dplyr::group_by(!!rlang::sym(uuid_column)) %>%
        dplyr::summarise(!!rlang::sym(concat_col) := paste0(cols, collapse = " "))

      final_df[[concat_col]] <-
        final_df[[concat_col]] %>% stringr::str_replace_all(paste0(concat_col, "."), "")

      select_multiple_list[[concat_col]] <- final_df
    }

    final_df_for_export <-
      purrr::reduce(select_multiple_list, dplyr::full_join, by = uuid_column)
    concat_col_names_from_fina_export <- final_df_for_export %>%
      dplyr::select(!dplyr::all_of(uuid_column)) %>%
      names()

    data_with_fix_concat <- dataset %>%
      dplyr::select(-dplyr::all_of(concat_col_names_from_fina_export)) %>%
      dplyr::left_join(final_df_for_export, by = uuid_column)

    if (is.null(kobo_survey)) {
      data_with_fix_concat <-
        data_with_fix_concat %>% dplyr::select(dplyr::all_of(cols_order))
    }
    if (!is.null(kobo_survey)) {
      data_with_fix_concat <-
        data_with_fix_concat %>% dplyr::select(dplyr::all_of(initial_order))
    }

    correction_parent_sm_log <- create_cleaning_log(
      raw_dataset = checked_data,
      raw_dataset_uuid_column = uuid_column,
      clean_dataset = data_with_fix_concat,
      clean_dataset_uuid_column = uuid_column
    )
    if ("comment" %in% names(correction_parent_sm_log)) {
      correction_parent_sm_log <- correction_parent_sm_log %>%
        dplyr::mutate(
          comment = gsub(
            "An alteration was performed",
            "Parent column changed to match children columns",
            comment
          )
        )
    }

    if (!is.null(cleaning_log_to_append)) {
      list_to_return <- list(
        data_with_fix_concat = data_with_fix_concat,
        cleaning_log = dplyr::bind_rows(cleaning_log_to_append,
                                        correction_parent_sm_log)
      )

    } else {
      list_to_return <- list(data_with_fix_concat = data_with_fix_concat,
                             correction_parent_sm_log = correction_parent_sm_log)
    }
  }

  if (nrow(select_multiple) == 0) {
    correction_parent_sm_log <- data.frame(uuid = "all",
                                           comment = "No choice multiple questions/Nothing has changed")
    list_to_return <- list(data_with_fix_concat = dataset,
                           correction_parent_sm_log = correction_parent_sm_log)
  }

  return(list_to_return)

}
