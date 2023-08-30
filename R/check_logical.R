#' Check a logical test
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as
#' "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param information_to_add string vector optional, if to add some information to the log
#' (today, vilagge)
#' @param check_id name of the check to perform
#' @param check_to_perform test to perform as R code (in text format)
#' @param columns_to_clean variables to be put in the log. if not provided, it will try to detect
#' variables
#' @param description description of the check performed
#'
#' @return return a list with the dataset checked stored as checked_dataset, it will have all
#' a column added with the check_id  and a dataframe with the logical check log.
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   uuid = c(1:10) %>% as.character(),
#'   today = rep("2023-01-01", 10),
#'   location = rep(c("villageA", "villageB"), 5),
#'   distance_to_market = c(rep("less_30", 5), rep("more_30", 5)),
#'   access_to_market = c(rep("yes", 4), rep("no", 6)),
#'   number_children_05 = c(rep(c(0, 1), 4), 5, 6)
#' )
#'
#' check_logical(test_data,
#'   uuid_column = "uuid",
#'   check_id = "my_test",
#'   check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
#'   columns_to_clean = "distance_to_market, access_to_market",
#'   description = "distance to market less than 30 and no access"
#' )
#'
#' check_logical(test_data,
#'   uuid_column = "uuid",
#'   information_to_add = c("today", "location"),
#'   check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
#'   columns_to_clean = "distance_to_market, access_to_market",
#'   description = "distance to market less than 30 and no access"
#' )
check_logical <- function(dataset,
                          uuid_column = "uuid",
                          information_to_add = NULL,
                          check_id = "logical_xx",
                          check_to_perform,
                          columns_to_clean = NULL,
                          description) {
  if (is.data.frame(dataset)) {
    dataset <- list(checked_dataset = dataset)
  }
  if (!("checked_dataset" %in% names(dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (check_id %in% names(dataset[["checked_dataset"]])) {
    msg <- glue::glue(check_id, " is in the names of the dataset, please change check id name.")
    stop(msg)
  }

  if (is.na(description) | description == "") {
    msg <- "description cannot be empty"
    stop(msg)
  }

  if (any(is.null(columns_to_clean), is.na(columns_to_clean), columns_to_clean == "")) {
    msg <- "columns_to_clean not shared, results may not be accurate"
    warning(msg)
    tentative_var <- detect_variable(check_to_perform)
    variables_across_by <- c(tentative_var, information_to_add) %>%
      stringr::str_split(",", simplify = T) %>%
      stringr::str_trim() %>%
      as.vector()
    variables_across_by <- variables_across_by[variables_across_by != ""]
  }

  if (!any(is.null(columns_to_clean), is.na(columns_to_clean), columns_to_clean == "")) {
    variables_across_by <- c(columns_to_clean, information_to_add) %>%
      stringr::str_split(",", simplify = T) %>%
      stringr::str_trim() %>%
      as.vector()
    variables_across_by <- variables_across_by[variables_across_by != ""]
  }

  dataset[["checked_dataset"]] <- dataset[["checked_dataset"]] %>%
    dplyr::mutate(!!rlang::sym(as.character(check_id)) := eval(parse(text = check_to_perform)))

  trimmed_dataset <- dataset[["checked_dataset"]] %>%
    dplyr::filter(!!rlang::sym(check_id)) %>%
    dplyr::mutate(uuid := !!rlang::sym(uuid_column)) %>%
    dplyr::select(dplyr::all_of(c("uuid", variables_across_by)))

  if (exists("tentative_var")) {
    if (length(tentative_var) == 0) {
      dataset[[check_id]] <- trimmed_dataset %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ format(., scientific = F, justify = "none", trim = T)),
          question = "unable to identify",
          old_value = "please check this uuid for this check",
          issue = description,
          check_id = check_id,
          check_binding = paste(check_id, uuid, sep = " ~/~ ")
        )
      return(dataset)
    }
  }

  dataset[[check_id]] <- trimmed_dataset %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~ format(., scientific = F, justify = "none", trim = T))) %>%
    tidyr::pivot_longer(cols = !dplyr::all_of(c("uuid", information_to_add)), names_to = "question", values_to = "old_value") %>%
    dplyr::mutate(
      issue = description,
      check_id = check_id,
      check_binding = paste(check_id, uuid, sep = " ~/~ ")
    )
  return(dataset)
}

#' Check several logical test
#'
#' This is a wrapper around check_logical to allow several checks to be performed.
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as
#' "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param information_to_add string vector optional, if to add some information to the log (today, vilagge)
#' @param list_of_check a dataframe with the checks to perform
#' @param check_id_column the column name with the names of each test.
#' @param check_to_perform_column the column name with the checks to perform as R code (in text format)
#' @param columns_to_clean_column the column name with the variables to be put in the log.
#' @param description_column the column name with the description
#' @param bind_checks default is TRUE, to bind the checks into 1 log.
#'
#' @return return a list with the dataset checked stored as checked_dataset, it will have all
#' a column added with the check_id  and a dataframe with the logical check log.
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   uuid = c(1:10) %>% as.character(),
#'   distance_to_market = rep(c("less_30", "more_30"), 5),
#'   access_to_market = c(rep("yes", 4), rep("no", 6)),
#'   number_children_05 = c(rep(c(0, 1), 4), 5, 6),
#'   number_children_618 = c(rep(c(0, 1), 4), 5, 6)
#' )
#' check_list <- data.frame(
#'   name = c("logical_xx", "logical_yy", "logical_zz"),
#'   check = c(
#'     "distance_to_market == \"less_30\" & access_to_market == \"no\"",
#'     "number_children_05 > 3",
#'     "rowSums(dplyr::across(starts_with(\"number\")), na.rm = T) > 9"
#'   ),
#'   description = c(
#'     "distance to market less than 30 and no access",
#'     "number of children under 5 seems high",
#'     "number of children very high"
#'   ),
#'   columns_to_clean = c(
#'     "distance_to_market, access_to_market",
#'     "number_children_05",
#'     ""
#'   )
#' )
#' check_logical_with_list(test_data,
#'   uuid_column = "uuid",
#'   list_of_check = check_list,
#'   check_id_column = "name",
#'   check_to_perform_column = "check",
#'   columns_to_clean_column = "columns_to_clean",
#'   description_column = "description"
#' )
check_logical_with_list <- function(dataset,
                                    uuid_column = "uuid",
                                    information_to_add = NULL,
                                    list_of_check,
                                    check_id_column,
                                    check_to_perform_column,
                                    columns_to_clean_column = NULL,
                                    description_column,
                                    bind_checks = TRUE) {
  if (any(duplicated(list_of_check[[check_id_column]]))) {
    msg <- glue::glue("The column ", check_id_column, " from the checklist contains duplicated.")
    stop(msg)
  }

  if (is.data.frame(dataset)) {
    dataset <- list(checked_dataset = dataset)
  }
  if (!("checked_dataset" %in% names(dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  # to be used to reduce
  initial_names <- names(dataset[["checked_dataset"]])

  if (any(is.null(columns_to_clean_column), is.na(columns_to_clean_column), columns_to_clean_column == "")) {
    list_of_check[["columns_to_clean_column"]] <- NA_character_
    columns_to_clean_column <- "columns_to_clean_column"
  }

  # split the check in a list and map check_logical
  log_of_logical_checks <- list_of_check %>%
    dplyr::mutate(!!rlang::sym(check_id_column) := as.character(!!rlang::sym(check_id_column))) %>%
    dplyr::group_by(!!rlang::sym(check_id_column)) %>%
    dplyr::group_split() %>%
    purrr::map(~ check_logical(
      dataset = dataset,
      uuid_column = uuid_column,
      information_to_add = information_to_add,
      check_id = .[[check_id_column]],
      check_to_perform = .[[check_to_perform_column]],
      columns_to_clean = .[[columns_to_clean_column]],
      description = .[[description_column]]
    )) %>%
    purrr::set_names(list_of_check[[check_id_column]])


  dataset[["checked_dataset"]] <- log_of_logical_checks %>%
    purrr::map(function(xx) xx[["checked_dataset"]]) %>%
    purrr::reduce(dplyr::left_join, by = initial_names)

  log_from_check <- log_of_logical_checks %>%
    purrr::map(~ purrr::keep(., names(.) %in% list_of_check[[check_id_column]])) %>%
    purrr::map(dplyr::bind_rows)

  if (bind_checks == FALSE) {
    dataset <- append(dataset, log_from_check)
  } else {
    dataset[["logical_all"]] <- log_from_check %>% purrr::reduce(dplyr::bind_rows)
  }

  return(dataset)
}
