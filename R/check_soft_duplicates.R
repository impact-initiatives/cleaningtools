#' Checks for survey similarities - Soft Duplicates
#'
#' @param dataset RawData to be checked as a dataframe or a list with dataframe
#'  stored as "checked_dataset"
#' @param kobo_survey Kobo Survey Sheet
#' @param uuid String character for the uuid column. By default "uuid"
#' @param idnk_value String character for the value of the "I don't know" value
#' @param sm_seperator Separator for choice multiple questions. The default is "."
#' @param log_name Name of the log dataframe flagged in the end of the function
#'
#' @return return a list with the dataset checked stored as checked_dataset and
#' a dataframe with the soft duplicate log
#' @export
#'
#' @examples
#' soft_duplicates <- check_soft_duplicates(dataset = cleaningtools_raw_data,
#'                                          kobo_survey = cleaningtools_survey,
#'                                          uuid = "X_uuid",
#'                                          idnk_value = "dont_know",
#'                                          sm_seperator = ".",
#'                                          log_name = "soft_duplicate_log")

check_soft_duplicates <- function(dataset,
                                  kobo_survey,
                                  uuid="uuid",
                                  idnk_value="idnk",
                                  sm_seperator = ".",
                                  log_name = "soft_duplicate_log"){

  if(is.data.frame(dataset)){
    dataset <- list(checked_dataset = dataset)
  }

  if(!"checked_dataset" %in% names(dataset)) {
    stop("Cannot identify the dataset in the list.")
  }

  if(!uuid %in% names(dataset[["checked_dataset"]])){
    msg <- glue::glue("Cannot find ", uuid, " in the names of the dataset")
    stop(msg)
  }

  data <- dataset$checked_dataset
  # 1) store UUIDs
  uuids <- data[[uuid]]

  # 2) convert all columns to character and tolower
  data <- dplyr::mutate_all(data, as.character)
  data <- dplyr::mutate_all(data, tolower)

  # 3) remove columns that are naturally different in each survey:
  # - columns of type = "start", "end", etc.
  # - columns starting with "_"
  # - option columns for the select multiple -> keeping only the concatenation column
  types_to_remove <- c("start", "end", "today", "deviceid", "date", "geopoint", "audit",
                       "note", "calculate","begin_group","end_group")
  sm_parents<-auto_detect_sm_parents(data, sm_seperator)
  sm_columns <- data %>%
    dplyr::select(dplyr::starts_with(glue::glue("{sm_parents}{sm_seperator}"))) %>%
    colnames()
  cols_to_keep <- data.frame(column=colnames(data)) %>%
    dplyr::left_join(select(kobo_survey, name, type), by=c("column"="name"))%>%
    dplyr::filter((!(type %in% types_to_remove) & !stringr::str_starts(column, "_") & !(column %in% sm_columns)))
  data <- data[, cols_to_keep$column]

  # 4) remove columns with all NA; convert remaining NA to "NA"; convert all columns to factor
  data <- data[, colSums(is.na(data))<nrow(data)]
  data[is.na(data)] <- "NA"
  data <- data %>% dplyr::mutate_if(is.character, factor)
  error.message <- "NAs detected, remove them before proceeding (it can happen when converting to factor)"
  if (sum(is.na(data))>0) stop(error.message)

  # 5) calculate gower distance
  gower_dist <- cluster::daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)

  # 6) convert distance to number of differences and determine closest matching survey
  r <- unlist(lapply(1:nrow(data), function(i){
    srv1 <- sort(gower_mat[i,]*ncol(data))[1]
    srv2 <- sort(gower_mat[i,]*ncol(data))[2]
    if (names(srv1)==as.character(i)) return(srv2)
    else return(srv1)
  }))

  # 7) add relevant columns
  outdata <- dataset$checked_dataset[,uuid]
  outdata[["num_cols_not_NA"]] <- rowSums(data!="NA")
  outdata[[paste0("num_cols_", idnk_value)]] <- rowSums(data==idnk_value)
  outdata[[uuid]] <- uuids
  outdata[["id_most_similar_survey"]] <- uuids[as.numeric(names(r))]
  outdata[["number_different_columns"]] <- as.numeric(r)
  outdata <- outdata %>% dplyr::arrange(number_different_columns, !!rlang::sym(uuid))
  dataset[[log_name]] <- as.data.frame(outdata)

  return(dataset)
}


