#' Read all audit files from a zip
#'
#' @param audit_zip_path location to .zip file. It must contain .zip
#' @param .dataset dataset to (optional)
#' @param uuid uuid column if a dataset is provided. It will only read the uuid
#' present in the dataset. It will only be used if a dataset is provided.
#'
#' @return A list with all the audits file read as they are.
#' @export
#'
#' @examples
#' \dontrun{
#' create_audit_list("audit_path.zip")
#' }
create_audit_list <- function(audit_zip_path,
                              .dataset = NULL,
                              uuid = "_uuid") {
  list_of_files <- unzip(audit_zip_path, list = TRUE) %>%
    dplyr::rename(path = Name) %>%
    dplyr::filter(stringr::str_detect(path, pattern = "audit.csv"))

  locatation_audit <- list_of_files %>%
    dplyr::pull(path) %>%
    stringr::str_split("/") %>%
    purrr::set_names(list_of_files$path) %>%
    purrr:::map_dbl(~ which(stringr::str_detect(.x, "audit.csv"))) %>%
    unique()

  if (length(locatation_audit) != 1) {
    stop("Please check the audit zip, some folders are not following the same structure.")
  }

  all_uuid_df <- list_of_files %>%
    dplyr::select(path) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(uuid = unlist(stringr::str_split(path, "/"))[[locatation_audit - 1]]) %>%
    dplyr::ungroup()

  if (!is.null(.dataset)) {
    if (uuid %in% names(.dataset) != 1) {
      msg <- glue::glue("The variable ", uuid, " cannot be identified in the dataset provided.")
      stop(msg)
    }
    look_up_vector <- .dataset[[uuid]]

    if (any(!all_uuid_df$uuid %in% look_up_vector)) {
      msg <- glue::glue(nrow(all_uuid_df) - length(look_up_vector), " audit files are found but not in the dataset. They won't be read.")
      warning(msg)
    }

    all_uuid_df <- all_uuid_df %>%
      dplyr::filter(uuid %in% look_up_vector)
  }

  list_of_audits <- all_uuid_df$path %>%
    purrr::map(~ read.table(unz(audit_zip_path, filename = .x), header = TRUE, quote = "\"", sep = ",")) %>%
    purrr::set_names(all_uuid_df$uuid)

  if (!is.null(.dataset)) {
    if (length(list_of_audits) < length(look_up_vector)) {
      to_add_vector <- look_up_vector[!look_up_vector %in% names(list_of_audits)]
      msg <- glue::glue(length(to_add_vector), " audit files were not found. They were added as empty dataframes.")
      warning(msg)
      to_add <- purrr::map(to_add_vector, function(xx) {
        list_of_audits[[1]] %>% dplyr::filter(event == "IWANTANEMPTYAUDIT")
      }) %>%
        purrr::set_names(to_add_vector)

      list_of_audits <- append(list_of_audits, to_add)
    }
  }

  return(list_of_audits)
}
#' Calculate duration from audit between 2 questions
#'
#' The function will calculate time difference between the start of the
#' start_question and the end of the end_question.
#'
#' In case the node appear several time (if the value was changed or with a
#' select multiple) it will take the minimum for the start and the maximum for
#' the end.
#'
#' If a value is missing (skip logic or question not found), it will return -Inf
#'
#' @param audit_file a dataframe with a single audit file,
#' it needs start, end, node column
#' @param start_question character vector use for the starting question
#' @param end_question character vector use for the ending question
#'
#' @return A dataframe with the duration in ms and duration in minutes.
#' @export
#'
#' @examples
#' some_audit <- data.frame(event = c("form start", rep("question", 5)),
#'                          node = c("", paste0("/xx/question", 1:5)),
#'                          start = c(1661415887295,1661415887301,1661415890819,
#'                          1661415892297,1661415893529,1661415894720),
#'                          end = c(NA, 1661415890790, 1661415892273,
#'                          1661415893506, 1661415894703, 1661415896452))
#' create_duration_from_audit_with_start_end(some_audit, "question1", "question3")
#'
create_duration_from_audit_with_start_end <- function(audit_file, start_question, end_question) {
  df_start_subset <- audit_file[grepl(paste0("\\/(?:",start_question,")$"), audit_file$node),]
  start_question_df <- df_start_subset %>% dplyr::select(start)
  start_question_df<-min(start_question_df$start)

  df_end_subset <- audit_file[grepl(paste0("\\/(?:",end_question,")$"), audit_file$node),]
  end_question_df <- df_end_subset %>% dplyr::select(end)
  end_question_df<-max(end_question_df$end)

  duration_ms<-end_question_df-start_question_df
  duration_minutes<- round(duration_ms/1000/60,1)

  data.frame(duration_ms=duration_ms,
             duration_minutes= duration_minutes)
}

#' Calculate duration from audit summing all time
#'
#' Duration is the difference between the sum of all start and the sum of all end.
#' It ignores all node that are empty such as form_start, end screen, form save, etc.
#'
#' @param audit_file a dataframe with a single audit file,
#' it needs start, end, node column
#'
#' @return A dataframe with the duration in ms and duration in minutes.
#' @export
#'
#' @examples
#' some_audit <- data.frame(event = c("form start", rep("question", 5)),
#'                          node = c("", paste0("/xx/question", 1:5)),
#'                          start = c(1661415887295,1661415887301,1661415890819,
#'                          1661415892297,1661415893529,1661415894720),
#'                          end = c(NA, 1661415890790, 1661415892273,
#'                          1661415893506, 1661415894703, 1661415896452))
#' create_duration_from_audit_sum_all(some_audit)

create_duration_from_audit_sum_all <- function(audit_file) {
  audit_file <- audit_file %>% filter(node != "")
  duration_ms <- sum(audit_file$end - audit_file$start)
  duration_minutes <- round(duration_ms/1000/60,1)
  data.frame(duration_ms=duration_ms,
             duration_minutes= duration_minutes)
}

#' Adds duration from the audit file
#'
#' Wrapper around create_duration_from_audit_with_start_end and
#' create_duration_from_audit_sum_all to add the duration to the dataset.
#'
#' @param .dataset a dataframe to add the duration.
#' @param col_name_prefix string character to be used for the prefix of new columns
#' @param uuid_var string character for the uuid variable name in the dataset
#' @param audit_list list of dataframe that are the audit file with the uuid of
#' each interview as name of the dataframe.
#' @param start_question character vector use for the starting question (optional)
#' @param end_question character vector use for the ending question (optional)
#' @param sum_all TRUE or FALSE if to add the time with sum all duration
#'
#' @return return the dataset with durations column added
#' @export
#'
#' @examples
#' list_audit <- list(uuid1 = data.frame(event = c("form start", rep("question", 5)),
#'                                       node = c("", paste0("/xx/question", 1:5)),
#'                                       start = c(1661415887295, 1661415887301,
#'                                                 1661415890819, 1661415892297,
#'                                                 1661415893529, 1661415894720),
#'                                       end = c(NA, 1661415890790, 1661415892273,
#'                                               1661415893506, 1661415894703,
#'                                               1661415896452)),
#'                    uuid2 = data.frame(event = c("form start", rep("question", 5)),
#'                                       node = c("", paste0("/xx/question", 1:5)),
#'                                       start = c(1661415887295,1661415887301,1661415890819,
#'                                                 1661415892297,1661415893529,1661415894720),
#'                                       end = c(NA, 1661415890790, 1661415892273,
#'                                       1661415893506, 1661415894703, 1661415896452)))
#' some_dataset <- data.frame(X_uuid = c("uuid1", "uuid2"),
#'                            question1 = c("a","b"),
#'                            question2 = c("a","b"),
#'                            question3 = c("a","b"),
#'                            question4 = c("a","b"),
#'                            question5 = c("a","b"))
#' add_duration_from_audit(some_dataset, uuid_var = "X_uuid", audit_list = list_audit)
#' add_duration_from_audit(some_dataset, uuid_var = "X_uuid", audit_list = list_audit,
#'                         start_question = "question1",
#'                         end_question = "question3",
#'                         sum_all = FALSE)
#' add_duration_from_audit(some_dataset, uuid_var = "X_uuid", audit_list = list_audit,
#'                         start_question = "question1",
#'                         end_question = "question3",
#'                         sum_all = TRUE)

add_duration_from_audit <- function(.dataset,
                                    col_name_prefix = "duration_audit",
                                    uuid_var = "_uuid",
                                    audit_list,
                                    start_question = NULL,
                                    end_question = NULL,
                                    sum_all = TRUE) {
  #checks input
  if(is.null(start_question) & !is.null(end_question)) {
    stop("start_question is missing")
  }
  if(!is.null(start_question) & is.null(end_question)) {
    stop("end_question is missing")
  }
  if(!is.null(start_question) & !is.null(end_question)) {
    new_names_start_end <- paste0(col_name_prefix, c("_start_end_ms", "_start_end_minutes"))
    if(any(new_names_start_end %in% names(.dataset))) {
      msg <- glue::glue(col_name_prefix, " seems to be already used as name in your dataset.")
      stop(msg)
    }
  }

  if(sum_all) {
    new_names_sum_all <- paste0(col_name_prefix, c("_sum_all_ms", "_sum_all_minutes"))
    if(any(new_names_sum_all %in% names(.dataset))) {
      msg <- glue::glue(col_name_prefix, " seems to be already used as name in your dataset.")
      stop(msg)
    }
  }

  #calculate duration for each audit file
  if(!is.null(start_question) & !is.null(end_question)) {
  duration_with_start_end <- audit_list %>%
    purrr::map(~create_duration_from_audit_with_start_end(.x,
                                                          start_question = start_question,
                                                          end_question = end_question)) %>%
    purrr::set_names(names(audit_list)) %>%
    do.call(rbind,.) %>%
    `names<-`(new_names_start_end) %>%
    dplyr::mutate(uuid = row.names(.))
  }

  if(!uuid_var %in% names(.dataset)) {
    msg <- glue::glue(uuid_var, " variable cannot be found in the dataset.")
    stop(msg)
  }

  if(all(!names(audit_list) %in% .dataset[[uuid_var]])) {
    stop("It seems no uuid are found as name of any data frame of audit list, make sure the data frame are saved with the uuid as name.")
  }

  audit_check <- audit_list %>%
    purrr::map_lgl(function(xx) {all(c("node", "start", "end", "event") %in% names(xx) )}) %>%
    all()
  if(!audit_check) {
    stop("Some columns are missing in the audits, please make sure to have at least event, node, start, end")
  }

  if(sum_all) {
  duration_with_sum_all <- audit_list %>%
    purrr::map(create_duration_from_audit_sum_all) %>%
    purrr::set_names(names(audit_list)) %>%
    do.call(rbind,.) %>%
    `names<-`(new_names_sum_all) %>%
    dplyr::mutate(uuid = row.names(.))
  }

  if(exists("duration_with_sum_all")) {
    .dataset <- .dataset %>%
      left_join(duration_with_sum_all, by = setNames("uuid",uuid_var))
  }

  if(exists("duration_with_start_end")) {
    .dataset <- .dataset %>%
      left_join(duration_with_start_end, by = setNames("uuid",uuid_var))
  }

  return(.dataset)

}

#' Check if duration is outside of a range
#'
#' Check if value is strictly inferior of the lower threshold or strictly
#' superior of the higher threshold.
#'
#' @param .dataset a dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param .col_to_check string character with the name of the duration column
#' @param uuid_var character string of the uuid variable
#' @param name_log character string with name to give to the log
#' @param lower_bound lower value of the range (strictly inferior to)
#' @param higher_bound higher value of the range (strictly superior to)
#'
#' @return return a list with the dataset checked stored as checked_dataset and
#' a dataframe with the duration log
#' @export
#'
#' @examples
#' testdata <- data.frame(
#'   uuid = c(letters[1:7]),
#'   duration_audit_start_end_ms = c(2475353, 375491, 2654267, 311585, 817270,
#'                                   2789505, 8642007),
#'   duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
#' ) %>%
#'   dplyr::rename(`_uuid` = uuid)
#'
#' check_duration(testdata,
#'                .col_to_check = "duration_audit_start_end_minutes")
#'
#' check_duration(
#'   testdata,
#'   .col_to_check = "duration_audit_start_end_ms",
#'   lower_bound = 375490,
#'   higher_bound = 8642000
#' )
#'
#' testdata %>% check_duration(.col_to_check = "duration_audit_start_end_minutes") %>%
#'   check_duration(
#'     .col_to_check = "duration_audit_start_end_ms",
#'     name_log = "duration_in_ms",
#'     lower_bound = 375490,
#'     higher_bound = 8642000
#'   )

check_duration <- function(.dataset,
                           .col_to_check,
                           uuid_var = "_uuid",
                           name_log = "duration_log",
                           lower_bound = 25,
                           higher_bound = 90) {

  if (is.data.frame(.dataset)) {
    .dataset <- list(checked_dataset = .dataset)
  }
  if (!("checked_dataset" %in% names(.dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (!(.col_to_check %in% names(.dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", .col_to_check, " in the names of the dataset")
    stop(msg)
  }

  log <- .dataset[["checked_dataset"]] %>%
    dplyr::mutate(duration_check = !!rlang::sym(.col_to_check) < lower_bound |
                    !!rlang::sym(.col_to_check) > higher_bound) %>%
    dplyr::filter(duration_check) %>%
    dplyr::select(all_of(c(uuid_var, .col_to_check))) %>%
    dplyr::mutate(
      question = .col_to_check,
      issue = "Duration is lower or higher than the thresholds"
    ) %>%
    dplyr::rename(old_value = !!rlang::sym(.col_to_check),
                  uuid = !!rlang::sym(uuid_var))

  .dataset[[name_log]] <- log
  return(.dataset)

}
