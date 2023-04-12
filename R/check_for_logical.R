#' Check for a logical test
#'
#' @param a dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param uuid_var string variable with the name of uuid column
#' @param variables_to_add string vector optional, if to add some information to the log (today, vilagge)
#' @param check_id name of the check to perform
#' @param check_to_perform test to perform as R code (in text format)
#' @param variables_to_clean variables to be put in the log. if not provided, it will try to detect variables
#' @param description description of the check performed
#'
#' @return return a list with the dataset checked stored as checked_dataset, it will have all
#' a column added with the check_id  and a dataframe with the logical check log.
#' @export
#'
#' @examples
#' test_data <- data.frame(uuid = c(1:10) %>% as.character(),
#'                         today = rep("2023-01-01", 10),
#'                         location = rep(c("villageA", "villageB"),5),
#'                         distance_to_market = c(rep("less_30", 5), rep("more_30",5)),
#'                         access_to_market = c(rep("yes",4), rep("no",6)),
#'                         number_children_05 = c(rep(c(0,1),4),5,6))
#'
#' check_for_logical(test_data,
#'                   uuid_var = "uuid",
#'                   check_id = "my_test",
#'                   check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
#'                   variables_to_clean = "distance_to_market, access_to_market",
#'                   description = "distance to market less than 30 and no access")
#'
#' check_for_logical(test_data,
#'                   uuid_var = "uuid",
#'                   variables_to_add = c("today", "location"),
#'                   check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
#'                   variables_to_clean = "distance_to_market, access_to_market",
#'                   description = "distance to market less than 30 and no access")
check_for_logical <- function(.dataset,
                              uuid_var = "_uuid",
                              variables_to_add = NULL,
                              check_id = "logical_xx",
                              check_to_perform,
                              variables_to_clean = NULL,
                              description) {
  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (check_id %in% names(.dataset[["checked_dataset"]])) {
    msg <- glue::glue(check_id, " is in the names of the dataset, please change check id name.")
    stop(msg)
  }

  if (is.na(description) | description == "") {
    msg <- "description cannot be empty"
    stop(msg)
  }

  if (is.null(variables_to_clean) | all(is.na(variables_to_clean)) | all(variables_to_clean == "")) {
    msg <-  "variables_to_clean not shared, results may not be accurate"
    warning(msg)
    tentative_var <- detect_variable(check_to_perform)
    variables_across_by <- c(tentative_var, variables_to_add) %>%
      stringr::str_split(",", simplify = T) %>%
      stringr::str_trim() %>%
      as.vector()
    variables_across_by <- variables_across_by[variables_across_by != ""]
  }

  if (!is.null(variables_to_clean)) {
    variables_across_by <- c(variables_to_clean, variables_to_add) %>%
      stringr::str_split(",", simplify = T) %>%
      stringr::str_trim() %>%
      as.vector()
    variables_across_by <- variables_across_by[variables_across_by != ""]
  }

  .dataset[["checked_dataset"]] <- .dataset[["checked_dataset"]] %>%
        mutate(!!sym(check_id) := eval(parse(text = check_to_perform)))

  if(exists("tentative_var")) {
    if(length(tentative_var) == 0) {
      .dataset[[check_id]] <- .dataset[["checked_dataset"]] |>
        dplyr::filter(!!sym(check_id)) |>
        dplyr::select(uuid := !!rlang::sym(uuid_var), dplyr::all_of(variables_across_by)) %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character),
                      question = "unable to identify",
                      old_value = "please check this uuid for this check",
                      issue = description,
                      check_id = check_id,
                      check_binding = paste(check_id, uuid, sep = " ~/~ "))
      return(.dataset)
    }
  }

  .dataset[[check_id]] <- .dataset[["checked_dataset"]] |>
    dplyr::filter(!!sym(check_id)) |>
    dplyr::select(uuid := !!rlang::sym(uuid_var), dplyr::all_of(variables_across_by)) %>%
    tidyr::pivot_longer(cols= -c("uuid", variables_to_add), names_to = "question", values_to = "old_value") %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character),
                  issue = description,
                  check_id = check_id,
                  check_binding = paste(check_id, uuid, sep = " ~/~ "))
  return(.dataset)
}

#' Check several logical test
#'
#' This is a wrapper around check_for_logical to allow several checks to be performed.
#'
#' @param a dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param uuid_var string variable with the name of uuid column
#' @param variables_to_add string vector optional, if to add some information to the log (today, vilagge)
#' @param list_of_check a dataframe with the checks to perform
#' @param check_id_column the column name with the names of each test.
#' @param check_to_perform_column the column name with the checks to perform as R code (in text format)
#' @param variables_to_clean_column the column name with the variables to be put in the log.
#' @param description_column the column name with the description
#' @param bind_checks default is TRUE, to bind the checks into 1 log.
#'
#' @return return a list with the dataset checked stored as checked_dataset, it will have all
#' a column added with the check_id  and a dataframe with the logical check log.
#' @export
#'
#' @examples
#' test_data <- data.frame(uuid = c(1:10) %>% as.character(),
#'                         distance_to_market = rep(c("less_30","more_30"),5),
#'                         access_to_market = c(rep("yes",4), rep("no",6)),
#'                         number_children_05 = c(rep(c(0,1),4),5,6),
#'                         number_children_618 = c(rep(c(0,1),4),5,6))
#' check_list <- data.frame(name = c("logical_xx", "logical_yy", "logical_zz"),
#'                          check = c("distance_to_market == \"less_30\" & access_to_market == \"no\"",
#'                                    "number_children_05 > 3",
#'                                    "rowSums(across(starts_with(\"number\")), na.rm = T) > 9"),
#'                          description = c("distance to market less than 30 and no access",
#'                                          "number of children under 5 seems high",
#'                                          "number of children very high"),
#'                          variables_to_clean = c("distance_to_market, access_to_market",
#'                                                 "number_children_05",
#'                                                 ""))
#' check_for_logical_with_list(test_data,
#'                             uuid_var = "uuid",
#'                             list_of_check = check_list,
#'                             check_id_column = "name",
#'                             check_to_perform_column = "check",
#'                             variables_to_clean_column = "variables_to_clean",
#'                             description_column = "description")
check_for_logical_with_list <- function(.dataset,
                                        uuid_var = "_uuid",
                                        variables_to_add = NULL,
                                        list_of_check,
                                        check_id_column,
                                        check_to_perform_column,
                                        variables_to_clean_column,
                                        description_column,
                                         bind_checks = TRUE) {

  if(any(duplicated(list_of_check[[check_id_column]]))) {
    msg <- glue::glue("The column ", check_id_column, " from the checklist contains duplicated.")
    stop(msg)
  }

  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  #to be used to reduce
  initial_names <- names(.dataset[["checked_dataset"]])

  #split the check in a list and map check_for_logical
  log_of_logical_checks <- list_of_check %>%
    dplyr::group_by(!!sym(check_id_column)) %>%
    dplyr::group_split() %>%
    purrr::map(~check_for_logical(.dataset = .dataset,
                                  uuid_var = uuid_var,
                                  variables_to_add = variables_to_add,
                                  check_id = .[[check_id_column]],
                                  check_to_perform = .[[check_to_perform_column]],
                                  variables_to_clean = .[[variables_to_clean_column]],
                                  description = .[[description_column]])) %>%
    purrr::set_names(list_of_check[[check_id_column]])

  if(bind_checks == FALSE) {
    return(log_of_logical_checks)
  }

  .dataset[["checked_dataset"]] <- log_of_logical_checks %>%
    purrr::map(function(xx) xx[["checked_dataset"]]) %>%
    purrr::reduce(dplyr::left_join, by = initial_names)
  .dataset[["logical_all"]] <- log_of_logical_checks %>%
    purrr::map(~purrr::keep(., names(.) %in% list_of_check[[check_id_column]])) %>%
    purrr::map(dplyr::bind_rows) %>% purrr::reduce(dplyr::bind_rows)

  return(.dataset)
}
