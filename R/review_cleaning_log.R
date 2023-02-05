
#' Check deletion log
#' @param raw_data Raw dataset
#' @param raw_data_uuid Unique ID column name of raw dataset
#' @param clean_data Clean dataset
#' @param clean_data_uuid Unique ID column name of clean dataset
#' @param deletion_log deletion log
#' @param deletion_log_uuid Unique ID column name of deletion log
#' @return Discrepancy in deletion log and data sets
#' @export


check_deletion_log <- function(raw_data,
                               clean_data,
                               deletion_log,
                               raw_data_uuid,
                               clean_data_uuid,
                               deletion_log_uuid) {
  deletion_log_uuid_character <- deletion_log[[deletion_log_uuid]] %>% unique()


  ### entry found in clean data


  discrepancy_in_deletion_log <- clean_data[[clean_data_uuid]][clean_data[[clean_data_uuid]] %in% deletion_log_uuid_character]

  issue_in_deletion_log_hh <- deletion_log %>%
    dplyr::filter(!!sym(deletion_log_uuid) %in% discrepancy_in_deletion_log) %>%
    mutate(
      comment = "Survey(s) (HH) are not removed from the clean data"
    )
  ### no entry found in raw data
  discrepancy_in_deletion_log_raw_data <- deletion_log_uuid_character[!deletion_log_uuid_character %in% raw_data[[raw_data_uuid]]]

  issue_in_deletion_log_raw_data <- deletion_log %>%
    dplyr::filter({{ deletion_log_uuid }} %in% discrepancy_in_deletion_log_raw_data) %>%
    mutate(
      comment = " UUID is not found in raw data"
    )

  issue <- issue_in_deletion_log_hh %>% bind_rows(issue_in_deletion_log_raw_data)

  issue <- issue %>% rename(
    uuid = !!sym(deletion_log_uuid)
  )

  if (nrow(issue) > 0) {
    print(paste0(nrow(issue), " issue(s) found in deletation log"))
  }


  if (nrow(issue) == 0) {
    print("No Issue found in deletation log")
  }

  return(issue)
}

####################################################################################################################### 33


#' Compare cleaning logs
#' @param raw_data Raw dataset
#' @param raw_data_uuid Unique ID column name of raw dataset
#' @param clean_data Clean dataset
#' @param clean_data_uuid Unique ID column name of clean dataset
#' @param cleaning_log Cleaning log
#' @param cleaning_log_uuid Unique ID column name of cleaning log
#' @param cleaning_log_question_name Column name of cleaning log storing the dataset column name
#' @param cleaning_log_new_value Cleaning log new value
#' @param cleaning_log_old_value Cleaning log old value
#' @param deletion_log deletion log
#' @param deletion_log_uuid Unique ID column name of deletion log
#' @param stop_comparing_raw_value TRUE to stop the checks for discrepancy between old_value and raw value
#' @return Discrepancy in cleaning log
#' @export


cleaning_log_check <- function(raw_data = raw_data_hh_2,
                               raw_data_uuid = "X_id",
                               clean_data = clean_data_hh,
                               clean_data_uuid = "X_id",
                               cleaning_log = cleaning_log_only,
                               cleaning_log_uuid = "id",
                               cleaning_log_old_value = "old_value",
                               cleaning_log_new_value = "new_value",
                               cleaning_log_question_name = "question.name",
                               deletion_log = deletion_log_hh,
                               deletion_log_uuid = "id",
                               stop_comparing_raw_value = T) {
  raw_data <- raw_data %>%
    mutate_all(as.character) %>%
    rename(uuid = !!sym(raw_data_uuid))
  clean_data <- clean_data %>%
    mutate_all(as.character) %>%
    rename(uuid = !!sym(clean_data_uuid))
  cleaning_log <- cleaning_log %>%
    mutate_all(as.character) %>%
    rename(uuid = !!sym(cleaning_log_uuid))


  #### fixing cleaning log ##########
  cleaning_log <- cleaning_log %>% filter(uuid %in% deletion_log[[deletion_log_uuid]] == F) ## Here I am filtering out all the removed survey.

  cleaning_log <- cleaning_log %>% mutate(
    log_id = cur_group_rows()
  )

  ################################

  cleaning_log_issue <- list()

  cleaning_log_issue[["question_not_found"]] <- cleaning_log %>%
    dplyr::filter(!!sym(cleaning_log_question_name) %in% names(clean_data) == F) %>%
    mutate(
      comment = "question not found in the dataset."
    )


  uuid_clean_data_and_deletion_log <- c(clean_data$uuid, deletion_log[[deletion_log_uuid]])


  uuid_not_found_in_clean_data_deletion_log <- cleaning_log %>%
    dplyr::filter(uuid %in% uuid_clean_data_and_deletion_log == F) %>%
    mutate(comment = "UUID not found nither in clean data or deletation log") ### Here I am not checking the removed survey, rather checking the uuids in the cleaning log. So if we got something here, it must be in the deletion log (currently can not be found) or the entry is wrong

  cleaning_log_issue[["uuid_not_found_in_clean_data_deletion_log"]] <- uuid_not_found_in_clean_data_deletion_log %>%
    dplyr::mutate(comment = case_when(
      uuid %in% raw_data$uuid == F ~ "UUID not found nither in raw_data, clean data or deletation log",
      T ~ comment
    )) ## this must be wrong entry



  cl_not_finding_issue <- do.call("bind_rows", cleaning_log_issue)

  #### cleaning log to check the changes

  cleaning_log_to_check <- cleaning_log %>% filter(!log_id %in% cl_not_finding_issue$log_id)


  cleaning_log_question_name_in_cl <- cleaning_log_to_check[[cleaning_log_question_name]] %>% unique()

  raw_df_cl <- raw_data %>%
    select(c("uuid", all_of(cleaning_log_question_name_in_cl))) %>%
    pivot_longer(
      cols = all_of(cleaning_log_question_name_in_cl),
      names_to = cleaning_log_question_name,
      values_to = "raw_value"
    )

  clean_df_cl <- clean_data %>%
    select(c("uuid", all_of(cleaning_log_question_name_in_cl))) %>%
    pivot_longer(
      cols = all_of(cleaning_log_question_name_in_cl),
      names_to = cleaning_log_question_name,
      values_to = "cleaned_value"
    )


  cleaning_log_to_check3 <- cleaning_log_to_check %>%
    left_join(raw_df_cl, by = c("uuid", cleaning_log_question_name)) %>%
    left_join(clean_df_cl, by = c("uuid", cleaning_log_question_name))

  compare_cl_with_datasets <- cleaning_log_to_check3 %>%
    mutate(
      compare_raw_value = case_when(
        is.na(!!sym(cleaning_log_old_value)) & is.na(raw_value) ~ T,
        !!sym(cleaning_log_old_value) == raw_value ~ T,
        T ~ F
      ),
      compare_new_value = case_when(
        is.na(!!sym(cleaning_log_new_value)) & is.na(cleaned_value) ~ T,
        !!sym(cleaning_log_new_value) == cleaned_value ~ T,
        T ~ F
      ),
    ) %>%
    mutate(
      comment = case_when(
        compare_new_value == F & compare_raw_value == F ~ "Both new value and raw value of cleaning log are not matching with the cleaned and raw data",
        compare_new_value == F ~ "New value of cleaning log is not matching with cleaned data",
        compare_raw_value == F ~ "Old value of cleaning log is not matching with raw data"
      )
    ) %>%
    dplyr::filter(!is.na(comment))


  if (stop_comparing_raw_value == T) {
    compare_cl_with_datasets <- compare_cl_with_datasets %>% dplyr::filter(comment != "Old value of cleaning log is not matching with raw data")
  }



  output <- bind_rows(compare_cl_with_datasets, cl_not_finding_issue) %>%
    rename(
      df.raw_value = raw_value,
      df.clean_value = cleaned_value
    ) %>%
    select(c(uuid, everything(), "df.raw_value", "df.clean_value", "comment")) %>%
    select(-log_id)

  return(output)
}

######################### function both check ###########################33




#' Compare cleaning and deletion logs
#' @param raw_data Raw dataset
#' @param raw_data_uuid Unique ID column name of raw dataset
#' @param clean_data Clean dataset
#' @param clean_data_uuid Unique ID column name of clean dataset
#' @param cleaning_log Cleaning log
#' @param cleaning_log_uuid Unique ID column name of cleaning log
#' @param cleaning_log_question_name Column name of cleaning log storing the dataset column name
#' @param cleaning_log_new_value Cleaning log new value
#' @param cleaning_log_old_value Cleaning log old value
#' @param deletion_log deletion log
#' @param deletion_log_uuid Unique ID column name of deletion log
#' @param stop_comparing_raw_value TRUE to stop the checks for discrepancy between old_value and raw value
#' @return Discrepancy in cleaning and deletion log
#' @export



check_deletion_and_cleaning_log <- function(raw_data = raw_data_hh_2,
                                            raw_data_uuid = "X_id",
                                            clean_data = clean_data_hh,
                                            clean_data_uuid = "X_id",
                                            cleaning_log = cleaning_log_only,
                                            cleaning_log_uuid = "id",
                                            cleaning_log_old_value = "old_value",
                                            cleaning_log_new_value = "new_value",
                                            cleaning_log_question_name = "question.name",
                                            deletion_log = deletion_log_hh,
                                            deletion_log_uuid = "id",
                                            stop_comparing_raw_value = T) {
  check <- list()

  check[["check_deletion"]] <- check_deletion_log(
    raw_data = raw_data, clean_data = clean_data,
    deletion_log = deletion_log, raw_data_uuid = raw_data_uuid,
    clean_data_uuid = clean_data_uuid, deletion_log_uuid = deletion_log_uuid
  )



  check[["check_cleaning_log"]] <- cleaning_log_check(
    raw_data = raw_data,
    raw_data_uuid = raw_data_uuid,
    clean_data = clean_data,
    clean_data_uuid = clean_data_uuid,
    cleaning_log = cleaning_log,
    cleaning_log_uuid = cleaning_log_uuid,
    cleaning_log_question_name = cleaning_log_question_name,
    deletion_log = deletion_log,
    deletion_log_uuid = deletion_log_uuid,
    cleaning_log_new_value = cleaning_log_new_value,
    cleaning_log_old_value = cleaning_log_old_value,
    stop_comparing_raw_value = T
  )


  return(check)
}
