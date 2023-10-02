#' Checks for survey similarities - Soft Duplicates
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as "checked_dataset"
#'  stored as "checked_dataset"
#' @param kobo_survey Kobo survey sheet.
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param idnk_value String character for the value of the "I don't know" value
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @param log_name Name of the log dataframe flagged in the end of the function
#' @param threshold flag all entries less or equal a specified threshold. The default is 7.
#' @param return_all_results By default, the function will return only the values that are under the
#' threshold. Default is FALSE.
#'
#' @return return a list with the dataset checked stored as checked_dataset and
#' a dataframe with the soft duplicate log
#' @export
#'
#' @examples
#' soft_duplicates <- check_soft_duplicates(
#'   dataset = cleaningtools_raw_data,
#'   kobo_survey = cleaningtools_survey,
#'   uuid_column = "X_uuid",
#'   idnk_value = "dont_know",
#'   sm_separator = ".",
#'   log_name = "soft_duplicate_log",
#'   threshold = 7
#' )
#'
#' group_by_enum_raw_data <- cleaningtools_raw_data %>%
#'   dplyr::group_by(enumerator_num)
#'
#' soft_per_enum <- group_by_enum_raw_data %>%
#'   dplyr::group_split() %>%
#'   purrr::map(~ check_soft_duplicates(
#'     dataset = .,
#'     kobo_survey = cleaningtools_survey,
#'     uuid_column = "X_uuid", idnk_value = "dont_know",
#'     sm_separator = ".",
#'     log_name = "soft_duplicate_log",
#'     threshold = 7,
#'     return_all_results = TRUE
#'   ))
#' soft_per_enum %>%
#'   purrr::map(~ .[["soft_duplicate_log"]]) %>%
#'   purrr::map2(
#'     .y = dplyr::group_keys(group_by_enum_raw_data) %>% unlist(),
#'     ~ dplyr::mutate(.x, enum = .y)
#'   ) %>%
#'   do.call(dplyr::bind_rows, .)
check_soft_duplicates <- function(dataset,
                                  kobo_survey,
                                  uuid_column = "uuid",
                                  idnk_value = "idnk",
                                  sm_separator = ".",
                                  log_name = "soft_duplicate_log",
                                  threshold = 7,
                                  return_all_results = FALSE) {
  if (is.data.frame(dataset)) {
    dataset <- list(checked_dataset = dataset)
  }

  if (!"checked_dataset" %in% names(dataset)) {
    stop("Cannot identify the dataset in the list.")
  }

  if (!uuid_column %in% names(dataset[["checked_dataset"]])) {
    msg <- glue::glue("Cannot find ", uuid_column, " in the names of the dataset")
    stop(msg)
  }

  df <- dataset$checked_dataset
  # 1) store UUIDs
  uuids <- df[[uuid_column]]

  # 2) convert all columns to character and tolower
  df <- dplyr::mutate_all(df, as.character)
  df <- dplyr::mutate_all(df, tolower)

  # 3) remove columns that are naturally different in each survey:
  # - columns of type = "start", "end", etc.
  # - columns starting with "X_"
  # - option columns for the select multiple -> keeping only the concatenation column
  # - columns starting with special characters
  types_to_remove <- c(
    "start", "end", "today", "deviceid", "date", "geopoint", "audit",
    "note", "calculate", "begin_group", "end_group"
  )
  sm_parents <- auto_detect_sm_parents(df, sm_separator)
  sm_columns <- df %>%
    dplyr::select(dplyr::starts_with(glue::glue("{sm_parents}{sm_separator}"))) %>%
    colnames()
  cols_to_keep <- data.frame(column = colnames(df)) %>%
    dplyr::left_join(dplyr::select(kobo_survey, name, type), by = c("column" = "name")) %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter((!(type %in% types_to_remove) & !stringr::str_starts(column, "X_") &
      !(stringr::str_starts(column, "[[:punct:]]")) & !(column %in% sm_columns)))
  df <- df[, cols_to_keep$column]

  # 4) remove columns with all NA; convert remaining NA to "NA"; convert all columns to factor
  df <- df[, colSums(is.na(df)) < nrow(df)]
  df[is.na(df)] <- "NA"
  df <- df %>% dplyr::mutate_if(is.character, factor)
  error.message <- "NAs detected, remove them before proceeding (it can happen when converting to factor)"
  if (sum(is.na(df)) > 0) stop(error.message)

  # 5) calculate gower distance
  gower_dist <- cluster::daisy(df, metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
  gower_mat <- as.matrix(gower_dist)

  # 6) convert distance to number of differences and determine closest matching survey
  r <- unlist(lapply(1:nrow(df), function(i) {
    srv1 <- sort(gower_mat[i, ] * ncol(df))[2]
    return(srv1)
  }))

  # 7) add relevant columns
  outdata <- dataset$checked_dataset[, uuid_column]
  outdata[["num_cols_not_NA"]] <- rowSums(df != "NA")
  outdata[["total_columns_compared"]] <- ncol(df)
  outdata[[paste0("num_cols_", idnk_value)]] <- rowSums(df == idnk_value)
  outdata[["id_most_similar_survey"]] <- uuids[as.numeric(names(r))]
  outdata[["number_different_columns"]] <- as.numeric(r)
  outdata[["issue"]][outdata[["number_different_columns"]] <= threshold] <- glue::glue("Less than ", threshold, " differents options")
  outdata <- outdata %>%
    dplyr::arrange(number_different_columns, !!rlang::sym(uuid_column))

  names(outdata)[names(outdata) == uuid_column] <- "uuid"

  if (!return_all_results) {
    outdata <- outdata %>%
      dplyr::filter(!is.na(issue))
  }

  if(nrow(outdata) == 0) {
    outdata <- outdata %>%
      dplyr::select(uuid, issue)
  }

  dataset[[log_name]] <- as.data.frame(outdata)
  return(dataset)
}
