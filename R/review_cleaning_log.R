#' Generates cleaning log
#' @param raw_data Raw dataset
#' @param raw_data_uuid Unique ID column name of raw dataset
#' @param clean_data Clean dataset
#' @param clean_data_uuid Unique ID column name of clean dataset
#' @param check_for_deletion_log TRUE to flag the removed survey
#' @param check_for_variable_name TRUE to flag the removed variables
#' @param  cols_not_to_check List of columns those are not to be checked
#' @return Cleaning log
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' create_cleaning_log(
#' raw_data = cleaningtools::cleaningtools_raw_data, raw_data_uuid = "X_uuid",
#' clean_data = cleaningtools::cleaningtools_clean_data, clean_data_uuid = "X_uuid",
#' check_for_deletion_log = TRUE, check_for_variable_name = TRUE
#' )}




# generate cleaning log ---------------------------------------------------
create_cleaning_log <- function(raw_data = raw_data,
                                raw_data_uuid = "X_id",
                                clean_data = clean_data,
                                clean_data_uuid = "X_id",
                                check_for_deletion_log = T,
                                cols_not_to_check = NULL,
                                check_for_variable_name = T) {
  my_bind_row <- get("bind_rows",asNamespace("dplyr"))

  raw_data <- raw_data %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(trimws)
  clean_data <- clean_data %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(trimws)

  raw_data$uuid <- raw_data[[raw_data_uuid]]
  clean_data$uuid <- clean_data[[clean_data_uuid]]

  # log list
  log <- list()



  # create deletaion log  ---------------------------------------------------
  if (check_for_deletion_log == T & !all(unique(raw_data[[raw_data_uuid]]) %in% clean_data[[clean_data_uuid]])) {
    deleted_uuid <- raw_data$uuid[!raw_data$uuid %in% clean_data$uuid]
    log[["deletaion_log"]] <- data.frame(
      uuid = deleted_uuid,
      change_type = "remove_survey",
      comment = "No matching uuid in the cleaned dataset"
    )
  }

  # create deletaion log [number of clean data is grater than number of row data]  ---------------------------------------------------
  if (check_for_deletion_log == T & !all(unique(clean_data[[clean_data_uuid]]) %in% raw_data[[raw_data_uuid]])) {
    deleted_uuid <- clean_data$uuid[!clean_data$uuid %in% raw_data$uuid]
    log[["added_survey"]] <- data.frame(
      uuid = deleted_uuid,
      change_type = "added_survey",
      comment = "Survey added to the clean dataset."
    )
  }



  # check variable name [removed from clean data] ---------------------------

  if (check_for_variable_name == T) {
    ## variable removed

    removed_variable_name <- names(raw_data)[!names(raw_data) %in% names(clean_data)]

    if (length(removed_variable_name) > 0) {
      log[["variable_removed"]] <- data.frame(
        uuid = "all",
        question_name = removed_variable_name,
        change_type = "variable_removed",
        comment = "variable removed from the clean dataset"
      )
    }


    ## variable added

    added_variable_name <- names(clean_data)[!names(clean_data) %in% names(raw_data)]

    if (length(added_variable_name) > 0) {
      log[["variable_added"]] <- data.frame(
        uuid = "all",
        question_name = added_variable_name,
        change_type = "variable_added",
        comment = "variable added to the clean dataset"
      )
    }
  } # end check for variable


  # create log for change_response ------------------------------------------

  uuidlist <- clean_data$uuid[clean_data$uuid %in% raw_data$uuid]

  varlist <- names(raw_data)[names(raw_data) %in% names(clean_data)]

  varlist <- varlist[!varlist %in% unique(c(
    "uuid", "start", "end", "X_index", "_index", "index",
    "X_status", "_status", "status", "today",
    "X_submitted_by", "_submitted_by", "submitted_by",
    "X_submission_time", "_submission_time", "submission_time",
    raw_data_uuid,cols_not_to_check
  ))]


  ############################# change_response ############################################
  log[["change_response"]] <- lapply(varlist, function(x, clean_data, raw_data) {
    check <- merge(clean_data[c("uuid", x)], raw_data[c("uuid", x)], by = "uuid", all.x = T)

    index <- which(check[, 2] != check[, 3])

    if (length(index) != 0) {
      check <- check[index, ]
      names(check) <- c("uuid", "new_value", "old_value")
      check$question_name <- x
      check$change_type <- "change_response"
      check$comment <- "An alteration was performed"
      return(check)
    }
    message(x)
  }, clean_data = clean_data, raw_data = raw_data) %>% do.call(rbind, .)



  ############################# NA_to_change_response ############################################

  log[["NA_to change_response"]] <- lapply(varlist, function(x, clean_data, raw_data) {
    check <- merge(clean_data[c("uuid", x)], raw_data[c("uuid", x)], by = "uuid", all.x = T)

    index <- which(!is.na(check[, 2]) & is.na(check[, 3]))

    if (length(index) != 0) {
      check <- check[index, ]
      names(check) <- c("uuid", "new_value", "old_value")
      check$question_name <- x
      check$change_type <- "change_respose"
      check$comment <- "NA changed to value"
      return(check)
    }
    message(x)
  }, clean_data = clean_data, raw_data = raw_data) %>% do.call(rbind, .)






  ############################# blank_response ############################################
  log[["blank_response"]] <- lapply(varlist, function(x, clean_data, raw_data) {
    check <- merge(clean_data[c("uuid", x)], raw_data[c("uuid", x)], by = "uuid", all.x = T)

    index <- which(is.na(check[, 2]) & !is.na(check[, 3]))

    if (length(index) != 0) {
      check <- check[index, ]
      names(check) <- c("uuid", "new_value", "old_value")
      check$question_name <- x
      check$change_type <- "blank_response"
      check$comment <- "changed to NA"
      return(check)
    }
    message(x)
  }, clean_data = clean_data, raw_data = raw_data) %>% do.call(rbind, .)

  all_log <- do.call("my_bind_row", log)
  if(nrow(all_log) > 0) {all_log <- all_log |> dplyr::select(dplyr::any_of(c(
    "uuid", "question_name", "change_type", "new_value",
    "old_value", "comment"
  )))}

  return(all_log)
}


# Review cleaning logs ---------------------------------------------------

#' Review cleaning logs
#' @param raw_data Raw dataset
#' @param raw_data_uuid Unique ID column name of raw dataset
#' @param clean_data Clean dataset
#' @param clean_data_uuid Unique ID column name of clean dataset
#' @param cleaning_log Cleaning log
#' @param cleaning_log_uuid Unique ID column name of cleaning log
#' @param cleaning_log_change_type_column Change type column of cleaning log
#' @param cleaning_log_question_name Column name of cleaning log storing the dataset column name
#' @param cleaning_log_new_value Cleaning log new value
#' @param cleaning_log_old_value Cleaning log old value
#' @param cleaning_log_added_survey Value for change type column which defines for new surveys
#' @param cleanning_log_no_action_value Value for change type column which defines for no action needed
#' @param deletion_log deletion log
#' @param deletion_log_uuid Unique ID column name of deletion log
#' @param check_for_deletion_log TRUE to flag the removed survey
#' @return Discrepancy in cleaning log
#' @export
#' @examples
#' \dontrun{
#'  deletaion_log <- cleaningtools::cleaning_log |> dplyr::filter(change_type == "remove_survey")
#'  cleaning_log2 <- cleaningtools::cleaning_log |> dplyr::filter(change_type != "remove_survey")
#'
#' review_cleaning_log(raw_data = cleaningtools::raw_data,raw_data_uuid = "X_uuid",
#' clean_data = cleaningtools::clean_data,clean_data_uuid = "X_uuid",
#' cleaning_log = cleaning_log2,cleaning_log_uuid = "X_uuid",
#' cleaning_log_question_name = "questions",
#' cleaning_log_new_value =  "new_value",
#' cleaning_log_old_value = "old_value",
#' deletion_log = deletaion_log,
#' deletion_log_uuid = "X_uuid",
#' check_for_deletion_log =T)
#' }



review_cleaning_log <- function(raw_data = raw_data_hh,
                                raw_data_uuid = "X_id",
                                clean_data = clean_data_hh,
                                clean_data_uuid = "X_id",
                                cleaning_log = cleaning_log_only,
                                cleaning_log_uuid = "id",
                                cleaning_log_change_type_column = "change_type",
                                cleaning_log_question_name = "question",
                                cleaning_log_new_value = "new_value",
                                cleaning_log_old_value = "old_value",
                                cleaning_log_added_survey = "added_survey",
                                cleanning_log_no_action_value = c("no_action","no_change"),
                                deletion_log = NULL,
                                deletion_log_uuid = NULL,
                                check_for_deletion_log = T) {

  ## creating list to store missing values
  missing_in_cleaning_log <- list()


  if(check_for_deletion_log == T){
    if(is.null(deletion_log)) {stop("Please provide deletion log")}
    if(is.null(deletion_log_uuid)) {stop("Please provide deletion log uuid")}
    if(!deletion_log_uuid %in% names(deletion_log)){stop("Deletion log uuid not found")}}

  if(!clean_data_uuid %in% names(clean_data)){stop("Clean data uuid not found")}
  if(!raw_data_uuid %in% names(raw_data)){stop("Raw data uuid not found")}
  if(!cleaning_log_change_type_column %in% names(cleaning_log)){stop("Cleaning log change type column is not found")}
  if(!cleaning_log_question_name %in% names(cleaning_log)){stop("Cleaning log question column is not found")}
  if(!cleaning_log_new_value %in% names(cleaning_log)){stop("Cleaning log new value column is not found")}
  if(!cleaning_log_old_value %in% names(cleaning_log)){stop("Cleaning log old value column is not found")}




  my_bind_row <- get("bind_rows",asNamespace("dplyr"))

  # if(any(cl))

  clean_data <- clean_data %>% dplyr::mutate_all(as.character)
  raw_data <- raw_data %>% dplyr::mutate_all(as.character)

  ### unifying cleaning and deletation log
  cleaning_log <- cleaning_log %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(trimws)
  cleaning_log <- cleaning_log %>% dplyr::rename(
    uuid = !!rlang::sym(cleaning_log_uuid)
  )
  cleaning_log$uniqe_row_id <- paste0(cleaning_log$uuid, "_", cleaning_log[[cleaning_log_question_name]])
  cleaning_log$uniqe_row_id <- cleaning_log$uniqe_row_id %>% tolower()

  if(check_for_deletion_log ==T){
    deletion_log <- deletion_log %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(trimws)
    deletion_log <- deletion_log %>% dplyr::rename(
      uuid = !!rlang::sym(deletion_log_uuid))


    to_remove_from_cl_uuid <-  cleaning_log$uuid[cleaning_log$uuid %in% deletion_log$uuid] |> unique()

    missing_in_cleaning_log[["cleaning_log_deletion_log_duplication"]] <- cleaning_log |>
      dplyr::filter(uuid %in% to_remove_from_cl_uuid ) |> dplyr::mutate(
        comment = "UUID found in deletion log. Please remove the entry.")  |>
      dplyr::select(dplyr::any_of(c("uuid",
                                  cleaning_log_change_type_column,
                                  cleaning_log_question_name,
                                  cleaning_log_new_value,
                                  cleaning_log_old_value,
                                  "comment"))) |> dplyr::rename(
                                    df.change_type = !!rlang::sym(cleaning_log_change_type_column),
                                    df.question_name = !!rlang::sym(cleaning_log_question_name),
                                  )

    cleaning_log <- cleaning_log |> dplyr::filter(!uuid %in% deletion_log$uuid)

  }

  if(nrow(cleaning_log)>0){
  ## removing no action
  cleaning_log_no_action <- cleaning_log |>
    dplyr::filter(!!rlang::sym(cleaning_log_change_type_column) %in% cleanning_log_no_action_value )}

  if(nrow(cleaning_log)>0){
  if(nrow(cleaning_log_no_action)>0){
  ## log for no action check
  missing_in_cleaning_log[["no_action_issue"]] <- cleaning_log_no_action |> dplyr::filter(
    !(!!rlang::sym(cleaning_log_new_value)) == (!!rlang::sym(cleaning_log_old_value)) |
      ((is.na(!!rlang::sym(cleaning_log_new_value))) & (!is.na(!!rlang::sym(cleaning_log_old_value))))|
      ((!is.na(!!rlang::sym(cleaning_log_new_value))) & (is.na(!!rlang::sym(cleaning_log_old_value))))
  ) |> dplyr::mutate(
    comment = "No action with different value in new value column."
  ) |> dplyr::rename(
    df.change_type = !!rlang::sym(cleaning_log_change_type_column),
    df.question_name = !!rlang::sym(cleaning_log_question_name),
  )
}

}

if(nrow(cleaning_log)>0){
  cleaning_log <- cleaning_log |>
    dplyr::filter(!(!!rlang::sym(cleaning_log_change_type_column)) %in% cleanning_log_no_action_value )}




  ### generating cleaning log from clean and raw data
  cleaning_log_create <- create_cleaning_log(
    raw_data = raw_data,
    raw_data_uuid = raw_data_uuid,
    clean_data = clean_data,
    clean_data_uuid = clean_data_uuid,
    check_for_variable_name = F
  ) %>% dplyr::select(-dplyr::any_of("comment"))

  names(cleaning_log_create) <- paste0("df.", names(cleaning_log_create))




  #########################################################################################################
  ##################### Starts changes not applied ########################################################
  #########################################################################################################
  #
  clean_data_to_join <- clean_data %>% tidyr::pivot_longer(cols = !dplyr::all_of(clean_data_uuid),names_to = "question",values_to = "df.new_value") %>%
    dplyr::mutate(uniqe_row_id = paste0(!!rlang::sym(clean_data_uuid), "_",question)) %>% dplyr::select("uniqe_row_id","df.new_value")
  clean_data_to_join$uniqe_row_id <-  clean_data_to_join$uniqe_row_id  %>% tolower()


  raw_data_to_join <- raw_data %>% tidyr::pivot_longer(cols = !dplyr::all_of(raw_data_uuid),names_to = "question",
                                                       values_to = "df.old_value") %>%
    dplyr::mutate(uniqe_row_id = paste0(!!rlang::sym(raw_data_uuid), "_",question)) %>% dplyr::select("uniqe_row_id","df.old_value")
  raw_data_to_join$uniqe_row_id <-  raw_data_to_join$uniqe_row_id  %>% tolower()


  missing_in_cleaning_log[["cleaning_log_no_applied"]] <- cleaning_log %>%
    dplyr::left_join(clean_data_to_join,by = "uniqe_row_id") %>%
    dplyr::filter(((!!rlang::sym(cleaning_log_new_value)!=df.new_value)==T )|
                    (!is.na(!!rlang::sym(cleaning_log_new_value))& is.na(df.new_value))|
                    (is.na(!!rlang::sym(cleaning_log_new_value))& !is.na(df.new_value))
                  ) %>% dplyr::mutate(
      comment = "Changes were not applied"
    )  %>% dplyr::left_join(raw_data_to_join,by = "uniqe_row_id") %>% dplyr::rename(
      df.question_name = !!rlang::sym(cleaning_log_question_name),
      # cl.old_value = !!rlang::sym(cleaning_log_old_value),
      df.change_type = !!rlang::sym(cleaning_log_change_type_column),
      # cl.new_value = !!rlang::sym(cleaning_log_new_value)
    ) %>% dplyr::select(-"uniqe_row_id")




  ######################################################################################################
  #################### end:: changes not applied ########################################################
  ######################################################################################################

  ## checking if all the deleted survey is in deletion log
  if (check_for_deletion_log == T) {

    if(nrow(cleaning_log_create)>0){

    created_deletion_log <- cleaning_log_create[cleaning_log_create$df.change_type == "remove_survey", "df.uuid"] %>% unique()
    missing_in_deletion_log <- created_deletion_log[!created_deletion_log %in% deletion_log$uuid]

    if (length(missing_in_deletion_log) != 0) {
      missing_in_cleaning_log[["missing_in_deletion_log"]] <- data.frame(
        uuid = missing_in_deletion_log,
        df.change_type = "remove_survey",
        comment = "This survey was removed but currently missing in cleaning log"
      )
    }
    }
    ### check if all the uuid in deletion log are removed or not

    deletion_log_uuid_only <- deletion_log[[deletion_log_uuid]] %>% unique()
    deletion_log_not_applied <- deletion_log_uuid_only[deletion_log_uuid_only %in% clean_data[[clean_data_uuid]]]
    deletion_log_not_applied <- deletion_log_not_applied[deletion_log_not_applied %in% raw_data[[raw_data_uuid]]]


    if (length(deletion_log_not_applied) != 0) {
      missing_in_cleaning_log[["deletion_log_not_applied"]] <- data.frame(
        uuid = deletion_log_not_applied,
        df.change_type = "remove_survey",
        comment = "This survey should be deleted from the clean dataset but it was not deleted"
      )
    }

    ## deletation log check agnist raw data
    deletion_log_not_found <- deletion_log$uuid[!deletion_log$uuid %in% raw_data[[raw_data_uuid]]]

    if(length(deletion_log_not_found) >0){
      missing_in_cleaning_log[["deletion_log_not_found_in_raw_data"]] <- data.frame(
        uuid = deletion_log_not_found,
        df.change_type = NA_character_,
        comment = "This survey should be removed from deletion log as it doesn't exist in the raw data."
      )
    }

  }

  ### check if added_survey was actually added in the dataset
if(nrow(cleaning_log)>0){
  added_survey_df <- cleaning_log |> dplyr::filter(!!rlang::sym(cleaning_log_change_type_column) %in% cleaning_log_added_survey)
  added_survey_df_uuid <- added_survey_df$uuid


if(length(added_survey_df_uuid) >0){
  missing_in_cleaning_log[["not_added_in_the_cl"]] <- data.frame(
    uuid = added_survey_df_uuid[!added_survey_df_uuid %in% clean_data[[clean_data_uuid]]]
  ) |> dplyr::mutate(
    comment = "This survey should be added in the clean dataset but its missing now",
    df.change_type = "added_survey",
    df.new_value = NA_character_,
    df.question_name = NA_character_
  )
}
}



  ## checking duplicate entry in cleaning log
  cleaning_log_short <- cleaning_log[, c("uniqe_row_id", cleaning_log_new_value)]
  cleaning_log_short <- apply(cleaning_log_short, MARGIN = c(1, 2), tolower) %>% as.data.frame() ### make everything to lower to compare the new_value
  duplicate_id_long_list <- cleaning_log_short$uniqe_row_id[duplicated(cleaning_log_short$uniqe_row_id)]


  duplicate_entry_in_cleaning_log <- cleaning_log_short[cleaning_log_short$uniqe_row_id %in% duplicate_id_long_list, ]

  ## to check
  final_duplicated <-duplicate_entry_in_cleaning_log[!(duplicated(duplicate_entry_in_cleaning_log) |
                                                         duplicated(duplicate_entry_in_cleaning_log,
                                                                    fromLast = TRUE)), ]




  final_duplicate_df <- cleaning_log[cleaning_log$uniqe_row_id %in% final_duplicated$uniqe_row_id, ] |> dplyr::mutate(
    comment = "Duplicated entry with different value, please recheck and keep one")
  final_duplicate_df$df.question_name <- final_duplicate_df[[cleaning_log_question_name]]


  if(nrow(final_duplicate_df)>0){
    missing_in_cleaning_log[["duplicated_entry_with_different_value"]] <-
      final_duplicate_df[, c(
        "uuid", "df.question_name", cleaning_log_old_value, cleaning_log_new_value,
        "comment"
      )]
  }



  ## check for change log
  if(nrow(cleaning_log_create)>0){
    cleaning_log_create_change_response <- cleaning_log_create[!cleaning_log_create$df.change_type %in% c("variable_removed" ,"remove_survey"),]

    cleaning_log_create_change_response <- cleaning_log_create_change_response |>
      dplyr::mutate(uniqe_row_id = tolower(paste0(cleaning_log_create_change_response$df.uuid,"_",
                                                  cleaning_log_create_change_response$df.question_name)))
    cl_to_add <- cleaning_log[,c("uniqe_row_id",cleaning_log_new_value)]


  if( nrow(cleaning_log_create_change_response)>0){
    missing_in_cleaning_log[["value_check"]] <- cleaning_log_create_change_response %>% dplyr::mutate(
      check_in_given_log = uniqe_row_id %in% cl_to_add$uniqe_row_id
    ) %>% dplyr::left_join(cl_to_add,multiple = "all",by = "uniqe_row_id") %>% dplyr::mutate(
      new_value_check = dplyr::case_when(
        is.na(df.new_value) & is.na(!!rlang::sym(cleaning_log_new_value)) ~ T ,
        (df.new_value == !!rlang::sym(cleaning_log_new_value)) ==T ~T,
        T~F)
    ) %>% dplyr::mutate(
      comment = dplyr::case_when(check_in_given_log ==F ~ "Entry missing in cleaning log",
                                 new_value_check == F ~ "New value in cleaning log and value in clean dataset not matching")
    ) %>%
      dplyr::filter(!is.na(comment)) %>%
      dplyr::rename(
        uuid = df.uuid
      )
  }
  }

  ###
  lookup <- c(cl.old_value = cleaning_log_old_value,
              cl.new_value = cleaning_log_new_value)

  cleaning_log_issue <- do.call("my_bind_row", missing_in_cleaning_log) %>%
    dplyr::select(dplyr::any_of(
      c(
        "uuid", "df.question_name", "df.change_type", "df.new_value",
        cleaning_log_new_value,
        "df.old_value",
        cleaning_log_old_value,
        "comment"
      ))
    ) %>%
    dplyr::rename(
      dplyr::any_of(lookup)
    )


  return(cleaning_log_issue |> dplyr::mutate(
    comment = dplyr::case_when(df.change_type == "added_survey" &
                                 comment == "Entry missing in cleaning log" ~ "Survey missing in the raw data",
                               T~ comment)
  ))
}

