#' Adds the percentage of missing values per row
#'
#' @param .dataset A dataset to add the percentage of missing values
#' @param col_name string variable with the name of the new column to be created, default is percentage_missing
#' @param kobo_survey KOBO question or survey sheet including at least the type and name.
#' @param type_to_include Types (from KOBO) to be included in the columns default are
#' integer, date, text, select_one and select_multiple.
#'
#' @return a dataset with the one additional column with the percentage of missing value
#' @export
#'
#' @seealso [cleaningtools::check_percentage_missing()]
#'
#' @examples
#' data_test <- data.frame(
#'   uuid = c(1:3),
#'   col_1 = c(1:3),
#'   col_2 = c(NA, NA, "expenditures"),
#'   col_3 = c("with need", NA, "with need"),
#'   col_4 = c("food health school", NA, "food"),
#'   col_4.food = c(1, NA, 1),
#'   col_4.health = c(1, NA, 0),
#'   col_4.school = c(1, NA, 0)
#' )
#' kobo_survey <- data.frame(
#'   type = c("uuid",
#'            "integer",
#'            "select_one choice2",
#'            "select_one choice3",
#'            "select_multiple choice4"),
#'   name = c("uuid", "col_1", "col_2", "col_3", "col_4")
#' )
#' data_test %>% add_percentage_missing(kobo_survey = kobo_survey,
#' type_to_include = c("integer","select_one","select_multiple"))
#' data_test %>% add_percentage_missing()
add_percentage_missing <-
  function(.dataset,
           col_name = "percentage_missing",
           kobo_survey = NULL,
           type_to_include = c("integer", "date", "text", "select_one", "select_multiple")) {
    if (!is.null(kobo_survey)) {
      if (!all(c("type", "name") %in% names(kobo_survey))) {
        stop("Cannot identify type and/or name columns in kobo")
      }

      type_only <- kobo_survey$type  |> stringr::word(1) |> unique()

      not_found <- type_to_include[!type_to_include %in% type_only]
        if(length(not_found)>0){
        msg <- not_found |> glue::glue_collapse(sep = ", ") %>%
          glue::glue("Following type: ",., " cannot be found in the kobo tool")
        stop(msg)
      }


    }

    if (col_name %in% names(.dataset)) {
      msg <- glue::glue("There is already a column called ", col_name)
      stop(msg)
    }

    if (is.null(kobo_survey)) {
      .dataset <- .dataset %>%
        dplyr::mutate(!!rlang::sym(col_name) := rowSums(dplyr::across(.cols = dplyr::everything(), .fns = is.na)) / ncol(.))
      return(.dataset)
    } else {
      lookup_value <- type_to_include %>% stringr::str_c(collapse = "|")

      col_to_count <- kobo_survey %>%
        dplyr::filter(stringr::str_detect(type, lookup_value)) %>%
        dplyr::filter(name %in% names(.dataset)) %>%
        dplyr::pull(name)

      .dataset <- .dataset %>%
        dplyr::mutate(!!rlang::sym(col_name) := rowSums(dplyr::across(
          .cols = dplyr::all_of(col_to_count),
          .fns = is.na
        )) / length(col_to_count))
      return(.dataset)
    }
  }


#' Check the percentages of missing value
#'
#' The function will flag if a survey for its missing values. The missing values column can be created
#' with add_percentage_missing and the values are flagged with check_outliers.
#'
#' @param .dataset a dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param uuid_var string character for the uuid, default is "uuid"
#' @param .col_to_check string character with the name of the columns to check. Default is
#' "percentage_missing"
#' @param log_name name of the log of flagged value, default is percentage_missing_log
#'
#' @return return a list with the dataset checked stored as checked_dataset and
#' a dataframe with the flagged values log
#' @export
#'
#' @seealso [cleaningtools::add_percentage_missing()], [cleaningtools::check_outliers()]
#' @examples
#' # Adding the percentage missing first
#' data_example <- data.frame(uuid = letters[1:3],
#'                            col_1 = c(1:3),
#'                            col_2 = c(NA, NA, "expenditures"),
#'                            col_3 = c("with need",NA, "with need"),
#'                            col_4 = c("food health school", NA, "food"),
#'                            col_4.food = c(1,NA,1),
#'                            col_4.health = c(1,NA,0),
#'                            col_4.school = c(1,NA,0))
#' data_example <- data_example %>%
#'   add_percentage_missing()
#' data_example %>% check_percentage_missing()
#'
#' # With a dataset that already has a percentage missing
#' data_example2 <- data.frame(uuid = letters,
#'                             any_cols = LETTERS,
#'                             any_number = 1:26,
#'                             percentage_missing = c(rep(.05,25),.99))
#' data_example2 %>% check_percentage_missing()
#'
#'
check_percentage_missing <- function(.dataset,
                                     uuid_var = "uuid",
                                     .col_to_check = "percentage_missing",
                                     log_name = "percentage_missing_log") {
  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (!(uuid_var %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", uuid_var, " in the names of the dataset")
    stop(msg)
  }

  if (!(.col_to_check %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", .col_to_check, " in the names of the dataset")
    stop(msg)
  }

  log <- .dataset[["checked_dataset"]] %>%
    dplyr::select(dplyr::all_of(c(uuid_var, .col_to_check))) %>%
    check_outliers(uuid_col_name = uuid_var)

  log[["potential_outliers"]] <- log[["potential_outliers"]] %>%
    dplyr::mutate(across(.cols = dplyr::everything(), .fns = as.character),
                  issue = "Percentages of missing values from this survey is different from others")

  .dataset[[log_name]] <- log[["potential_outliers"]]

  return(.dataset)

}
