#' check outliers over the dataset
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as
#' "checked_dataset"
#' @param kobo_survey Kobo survey sheet. Default is NULL.
#' @param kobo_choices Kobo choices sheet. Default is NULL.
#' @param uuid_column UUID. Default is uuid
#' @param element_name name of the dataset in list
#' @param cols_to_add_cleaning_log Variables those must be included in the output
#' @param strongness_factor Strongness factor define how strong your outliers will be. The default is 3.
#' @param columns_not_to_check Columns to exclude from the checks even if they are numeric values.
#' @param remove_choice_multiple TRUE (default) will remove choice multiple questions from the output.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @param minimum_unique_value_of_variable Default is NULL, mean this parameter won't be considered. For example 10 means for any variable where number of unique value is less than 10, then the variable won't be considered for outlier checking.
#' @return return a list with the dataset checked stored as checked_dataset and a dataframe with
#' the outliers log
#' @export
#' @examples
#' dataset_outlier <- data.frame(
#'   uuid = paste0("uuid_", 1:100),
#'   one_value = c(round(runif(90, min = 45, max = 55)), round(runif(5)), round(runif(5, 99, 100))),
#'   expense = c(sample(200:500, replace = TRUE, size = 95), c(600, 100, 80, 1020, 1050)),
#'   income = c(c(60, 0, 80, 1020, 1050), sample(20000:50000, replace = TRUE, size = 95)),
#'   yy = c(rep(100, 99), 10)
#' )
#'
#' check_outliers(dataset = dataset_outlier, uuid_column = "uuid")
check_outliers <- function(dataset,
                           uuid_column = "uuid",
                           element_name = "checked_dataset",
                           kobo_survey = NULL,
                           kobo_choices = NULL,
                           cols_to_add_cleaning_log = NULL,
                           strongness_factor = 3,
                           minimum_unique_value_of_variable = NULL,
                           remove_choice_multiple = TRUE,
                           sm_separator = ".",
                           columns_not_to_check = NULL) {
  if (!is.list(dataset)) {
    stop("Input must be a dataframe or list.")
  }
  if (is.null(kobo_survey) & !is.null(kobo_choices)) {
    warning("Ignoring perameter `kobo_choices` as `kobo_survey` is not provided.")
  }
  if (is.null(kobo_choices) & !is.null(kobo_survey)) {
    warning("Ignoring perameter `kobo_survey` as `kobo_choices` is not provided.")
  }

  ######### checking input
  dataframe <- dataset

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


  dataset <- utils::type.convert(dataset, as.is = TRUE, na.string = c("", " ")) |>
    dplyr::rename(
      uuid = !!rlang::sym(uuid_column)
    ) |>
    dplyr::mutate(uuid = as.character(uuid))

  cols_to_add_cleaning_log <- c(cols_to_add_cleaning_log, "uuid") |> unique()



  if (remove_choice_multiple == T) {
    all_select_multiple_parent <- auto_sm_parent_children(dataset, sm_separator = sm_separator)
    all_select_multiple_cols <- all_select_multiple_parent$sm_child
  }

  if (remove_choice_multiple == T) {
    columns_not_to_check <- c(columns_not_to_check, all_select_multiple_cols)
  }

  cols_to_remove <- columns_not_to_check[!columns_not_to_check %in% cols_to_add_cleaning_log]




  if (!is.null(cols_to_remove)) {
    dataset <- dataset %>% dplyr::select(-dplyr::all_of(cols_to_remove))
  }



  if (!is.null(kobo_survey) & !is.null(kobo_choices)) {
    kobo_survey$name <- kobo_survey$name %>% stringr::str_replace_all("-", ".")


    interger_column_in_kobo <- (kobo_survey %>% dplyr::filter(type == "integer") %>%
      dplyr::filter(!grepl("enumerator|_instance_|index", name)))$name

    cols_name_exist_in_loop_kobo <- interger_column_in_kobo[interger_column_in_kobo %in% names(dataset)]
  }

  cols_name_exist_in_loop_numeric <- dataset %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select(-dplyr::starts_with("X")) %>%
    names()
  cols_name_exist_in_loop_int <- dataset %>%
    dplyr::select_if(is.integer) %>%
    dplyr::select(-dplyr::starts_with("X")) %>%
    names()

  if (!is.null(kobo_survey) & !is.null(kobo_choices)) {
    cols_name_exist_in_loop <- c(
      cols_name_exist_in_loop_kobo,
      cols_name_exist_in_loop_numeric, ## added in case of new recoded variables
      cols_name_exist_in_loop_int
    ) %>% unique()
  }




  if (is.null(kobo_survey) | is.null(kobo_choices)) {
    cols_name_exist_in_loop <- c(
      cols_name_exist_in_loop_numeric,
      cols_name_exist_in_loop_int
    ) %>% unique()
  }

  outlier_checks <- list()

  for (x in cols_name_exist_in_loop) {
    print(paste0("checking_", x))

    dataset[[x]] <- dataset[[x]] %>% as.numeric()
    variable_value <- dataset[[x]]

    variable_value <- variable_value[!is.na(variable_value) & !is.null(variable_value) & !is.infinite(variable_value)]

    if (!is.null(minimum_unique_value_of_variable)) {
      outliers_tf_nr <- (abs(variable_value - mean(variable_value)) > strongness_factor * stats::sd(variable_value)) &
        (length(unique(variable_value)) > minimum_unique_value_of_variable)
    }


    if (is.null(minimum_unique_value_of_variable)) {
      outliers_tf_nr <- abs(variable_value - mean(variable_value)) > strongness_factor * stats::sd(variable_value)
    }



    outliers_value <- variable_value[outliers_tf_nr] %>% unique()



    outlier_checks[[x]] <- dataset %>%
      dplyr::mutate(
        issue = dplyr::case_when(dataset[[x]] %in% outliers_value ~ "outlier (normal distribution)"),
      ) %>%
      dplyr::filter(issue == "outlier (normal distribution)") %>%
      dplyr::select(dplyr::all_of(cols_to_add_cleaning_log), issue, dplyr::all_of(x)) %>%
      tidyr::pivot_longer(cols = paste0(x), names_to = "question", values_to = "old_value")



    #### log checks ####

    dataset[["log"]] <- log(dataset[[x]] + 1)


    log_variable <- dataset[["log"]]
    log_variable <- log_variable[!is.na(log_variable) & !is.null(log_variable) & !is.infinite(log_variable)]

    if (!is.null(minimum_unique_value_of_variable)) {
      outliers_tf <- abs(log_variable - mean(log_variable)) > strongness_factor * stats::sd(log_variable) &
        length(unique(log_variable)) > minimum_unique_value_of_variable
    }

    if (is.null(minimum_unique_value_of_variable)) {
      outliers_tf <- abs(log_variable - mean(log_variable)) > strongness_factor * stats::sd(log_variable)
    }



    outliers_value_log <- log_variable[outliers_tf] %>% unique()



    outlier_checks[[paste0("log_", x)]] <- dataset %>%
      dplyr::mutate(
        issue = dplyr::case_when(dataset[["log"]] %in% outliers_value_log ~ "outlier (log distribution)"),
      ) %>%
      dplyr::filter(issue == "outlier (log distribution)") %>%
      dplyr::select(dplyr::all_of(cols_to_add_cleaning_log), issue, dplyr::all_of(x)) %>%
      tidyr::pivot_longer(cols = paste0(x), names_to = "question", values_to = "old_value")
  }

  outliers_cl <- do.call(rbind, outlier_checks)

  outliers_cl <- outliers_cl %>% dplyr::distinct(!!!rlang::syms(cols_to_add_cleaning_log), question, old_value, .keep_all = T)


  potential_outliers <- outliers_cl %>% dplyr::filter(!question %in% columns_not_to_check)


  ## Append the list
  if (is.data.frame(dataframe)) {
    checked_dataset <- dataframe
    return(list(
      checked_dataset = checked_dataset,
      potential_outliers = potential_outliers
    ))
  }

  if (!is.data.frame(dataframe)) {
    list_Df <- list(potential_outliers = potential_outliers)

    return(append(dataframe, list_Df))
  }
}
