#' Adds the percentage of missing values per row
#'
#' @param .dataset A dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param col_name string variable with the name of the new column to be created
#' @param uuid_col A string variable with the uuid
#' @param kobo_survey KOBO question or survey sheet including at least the type and name.
#' @param type_to_include Type (from KOBO) to be included in the columns default are
#' integer, date, text, select_one and select_multiple.
#'
#' @return a dataset with the one additional column with the percentage of missing value
#' @export
#'
#' @examples
#'   data_test <- data.frame(uuid = c(1:3),
#' col_1 = c(1:3),
#' col_2 = c(NA, NA, "expenditures"),
#' col_3 = c("with need",NA, "with need"),
#'
#' col_4 = c("food health school", NA, "food"),
#' col_4.food = c(1,NA,1),
#' col_4.health = c(1,NA,0),
#' col_4.school = c(1,NA,0))
#' kobo_survey <- data.frame(type = c("uuid", "integer", "select_one choice2", "select_one choice3", "select_multiple choice4"),
#'                           name = c("uuid", "col_1", "col_2", "col_3", "col_4"))
#' data_test %>% add_percentage_missing(kobo_survey = kobo_survey, uuid_col = "uuid")
#' data_test %>% add_percentage_missing(uuid_col = "uuid")
add_percentage_missing <- function(.dataset, col_name = "percentage_missing", uuid_col = "_uuid",
                                   kobo_survey = NULL,
                                   type_to_include =  c("integer","date","text","select_one","select_multiple")) {
  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if(!is.null(kobo_survey)) {
    if(!all(c("type","name") %in% names(kobo_survey))) {
      stop("Cannot identify type and/or name columns in kobo")
    }
  }

  if(col_name  %in% names(.dataset[["checked_dataset"]])) {
    msg <- glue::glue("There is already a column called ", col_name)
    stop(msg)
  }

  if (!(uuid_col %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", uuid_col, " in the names of the dataset")
    stop(msg)
  }


  if(is.null(kobo_survey)) {
    .dataset[["checked_dataset"]] <- .dataset[["checked_dataset"]]  %>%
      dplyr::mutate(!!rlang::sym(col_name) := rowSums(dplyr::across(.fns = is.na)) / ncol(.))
    return(.dataset[["checked_dataset"]])
  } else {
    lookup_value <- type_to_include %>% stringr::str_c(collapse = "|")

    col_to_count <- kobo_survey %>%
      dplyr::filter(stringr::str_detect(type, lookup_value)) %>%
      dplyr::filter(name %in% names(.dataset[["checked_dataset"]])) %>%
      dplyr::pull(name)

    .dataset[["checked_dataset"]] <- .dataset[["checked_dataset"]]  %>%
      dplyr::mutate(!!rlang::sym(col_name) := rowSums(dplyr::across(.cols = dplyr::all_of(col_to_count),
                                                                   .fns = is.na)) / length(col_to_count))
    return(.dataset[["checked_dataset"]])
  }

}

