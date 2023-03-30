  #' implement cleaning log on raw data set.
  #' @param df raw data (data frame or list)
  #' @param df_uuid column in raw data with uuid
  #' @param cl cleaning + deletion log (data.frame).
  #' @param cl_change_type_col column in cleaning log which specifies change type to be made
  #' @param values_for_change_response values in change type column which should be changed to a new value.
  #' @param values_for_blank_response values in change type column which should be blank (NA).
  #' @param values_for_no_change values in change type column which should NOT be changed to a new value.
  #' @param values_for_remove_survey values in change type column which should be deleted from the data.
  #' @param cl_change_col column in cleaning log which specifies data set column to change
  #' @param cl_uuid uuid in cleaning log
  #' @param cl_new_val cleaning log column specifying the new correct value
  #' @return clean data set
  #' @export
  #' @examples
  #' cleaning_log_test <- data.frame(
  #' uuid = paste0("uuid",1:4),
  #' question= c("age","gender","pop_group","strata"),
  #' change_type = c("blank_response","no_change","Delete","change_res"),
  #'new_value = c(NA_character_,NA_character_,NA_character_,"st-a")
  #')
  #'test_data <- data.frame(
  #'uuid =  paste0("uuid",1:4),
  #'age = c(180,23,45,67),
  #'gender = c("male","female","male","female"),
  #'pop_group = c("idp","refugee","host","idp"),
  #'strata = c("a","b","c","d")
  #')
  #'
  #'
  #'check_cleaning_log(df = test_data,df_uuid = "uuid",cl =cleaning_log_test,
  #'                 cl_change_type_col = "change_type",
  #'                 values_for_change_response =  "change_res",
  #'                 cl_change_col = "question",cl_uuid = "uuid",cl_new_val = "new_value")
  #'
  #'create_clean_data(df = test_data,df_uuid = "uuid",cl = cleaning_log_test,
  #'                cl_change_type_col =  "change_type",
  #'                values_for_change_response = "change_res",
  #'                values_for_blank_response = "blank_response",
  #'                values_for_no_change = "no_change",
  #'                values_for_remove_survey = "Delete",
  #'                cl_change_col =  "question",
  #'                cl_uuid = "uuid",
  #'                cl_new_val = "new_value" )



create_clean_data <- function(df,
                                df_uuid,
                                cl,
                                cl_change_type_col,
                                values_for_change_response = "change_response",
                                values_for_blank_response = "blank_response",
                                values_for_no_change = "no_action",
                                values_for_remove_survey = "remove_survey",
                                cl_change_col,
                                cl_uuid,cl_new_val){

    df <- df %>% dplyr::mutate_all(as.character)
    cl <- cl %>% dplyr::mutate_all(as.character)



    assertthat::assert_that( cl_change_type_col %in% names(cl),
                             msg="cl_change_type_col column not found in cleaning log")

    all_type <- c(values_for_change_response,values_for_blank_response,values_for_no_change,values_for_remove_survey)

    cl <- cl |> dplyr::mutate(
      change_type_created_f = case_when(!!rlang::sym(cl_change_type_col) %in% values_for_change_response ~ "change_response",
                                        !!rlang::sym(cl_change_type_col) %in% values_for_blank_response ~ "blank_response",
                                        !!rlang::sym(cl_change_type_col) %in% values_for_no_change ~ "no_action",
                                        !!rlang::sym(cl_change_type_col) %in% values_for_remove_survey ~ "remove_survey"))

    assertthat::assert_that(any(!is.na(cl[[cl_change_type_col]])),
                            msg="You have NAs in change_type option(s)")


    check_not_entry_value_in_change_type <- unique(cl[[cl_change_type_col]])[!unique(cl[[cl_change_type_col]]) %in% all_type]
   if(length(check_not_entry_value_in_change_type) > 0){
     print(check_not_entry_value_in_change_type)
     stop("Missing values in change_type")}

    assertthat::assert_that(all(cl[[cl_change_type_col]] %in% all_type),
                            msg="You have missing change_type option(s)")


    # cl[[cl_change_type_col]]<-cl[[cl_change_type_col]] %>% tolower()

    cl[[cl_change_col]]<-cl[[cl_change_col]] %>% trimws()
    cl[[cl_new_val]]<-cl[[cl_new_val]] %>% trimws()
    cl_change_type_options<- c("change_response",  "remove_survey", "blank_response","no_action")
    # cl_change_response<- cl %>% filter(!!sym(cl_change_type_col) %in% c( cl_change_type_options[1]))
    cl_change_response<- cl %>% dplyr::filter(change_type_created_f %in% c( cl_change_type_options[1],cl_change_type_options[3]))
    cl_change_response<- cl_change_response %>%
      dplyr::mutate(
        !!cl_new_val:=ifelse(change_type_created_f==cl_change_type_options[3],NA,!!rlang::sym(cl_new_val))
      )
    # cl_change_type_options[3]
    cl_remove_survey<- cl %>% dplyr::filter(change_type_created_f == cl_change_type_options[2])
    # cl_blank_response<- cl %>% filter(!!sym(cl_change_type_col))

    if(all(cl_change_response[[cl_change_col]] %in% colnames(df))==F){
      problem_question_in_cl<-cl_change_response[[cl_change_col]][cl_change_response[[cl_change_col]] %in% colnames(df)==FALSE]
      print(paste0(problem_question_in_cl,": Not found in the dataset"))
    }

    if(all(cl[[cl_uuid]] %in% df[[df_uuid]] ==F)){
      problem_uuid_in_cl<-cl[[cl_uuid]][cl[[cl_uuid]] %in% c(df[[df_uuid]],"all_data")==FALSE]
      print(problem_uuid_in_cl)
      print("Not found in the datase")
    }
    assertthat::assert_that(all(cl_change_response[[cl_change_col]] %in% colnames(df)),
                msg="Make sure all names in cl_change_col values in the cleaning log are in dataset")
    assertthat::assert_that(all(cl[[cl_uuid]] %in% df[[df_uuid]]),
                msg="Make sure all uuids in cleaing log are in data set")

    if(nrow(cl_change_response)>0){
      for(i in 1:nrow(cl_change_response)){
        print(cl_change_response[[cl_change_col]][i])
        cl_uuid_temp<-cl_change_response[[cl_uuid]][i]
        cl_question_temp<-cl_change_response[[cl_change_col]][i]
        cl_new_val_temp<-cl_change_response[[cl_new_val]][i]

        if(cl_uuid_temp!="all_data"){
          df[df[[df_uuid]]==cl_uuid_temp,cl_question_temp]<-cl_new_val_temp}

        if(cl_uuid_temp=="all_data"){
          df[,cl_question_temp]<-cl_new_val_temp}
      }
    }
    else{ print("no change_response in log")}
    if(nrow(cl_remove_survey)>0){
      df<- df %>% dplyr::filter(!!rlang::sym(df_uuid) %in% cl_remove_survey[[cl_uuid]]==FALSE)
    }else{print("no surveys to remove in log")}

    return(df %>% type.convert(as.is = T))
  }







  #' check cleaning log
  #' @param df raw data (data.frame)
  #' @param df_uuid column in raw data with uuid
  #' @param cl cleaning log (data.frame)
  #' @param cl_change_type_col column in cleaning log which specifies change type to be made
  #' @param values_for_change_response values in change type column which should be changed to a new value.
  #' @param cl_change_col column in cleaning log which specifies data set column to change
  #' @param cl_uuid uuid in cleaning log
  #' @param cl_new_val cleaning log column specifying the new correct value
  #' @return cleaning log with only problematic entries and note specifying problem
  #' @export


  check_cleaning_log <- function(df,
                                 df_uuid,
                                 cl,
                                 cl_change_type_col,
                                 values_for_change_response = "change_response",
                                 cl_change_col,
                                 cl_uuid,cl_new_val){
    cl[[cl_change_col]]<-cl[[cl_change_col]] %>% trimws()
    cl[[cl_new_val]]<-cl[[cl_new_val]] %>% trimws()


    assertthat::assert_that( cl_change_type_col %in% names(cl),
                 msg="cl_change_type_col column not found in cleaning log")

    assertthat::assert_that(all(values_for_change_response %in% cl[[cl_change_type_col]]),
                msg="Value in values_for_change_response not found")

    cl_change_col_prob_df<-cl %>%
      dplyr::filter(!!rlang::sym(cl_change_type_col) %in% values_for_change_response) %>%
      dplyr::mutate(cl_prob="question_does_not_exist") %>%
      dplyr::filter(!!rlang::sym(cl_change_col) %in% colnames(df)==FALSE) %>%
      dplyr::select(cl_prob,dplyr::everything())

    cl_uuid_prob_df<-cl %>%
      dplyr::filter(!!rlang::sym(cl_uuid) %in% c(df[[df_uuid]],"all_data")==FALSE)%>%
      dplyr::mutate(cl_prob="uuid_does_not_exist") %>%
      dplyr::filter(!!sym(cl_uuid) %in% df[[df_uuid]]==FALSE) %>%
      dplyr::select(cl_prob,dplyr::everything())

    na_change_type_prob_df<-cl %>%
      dplyr::filter(is.na(!!rlang::sym(cl_change_type_col))) %>%
      dplyr::mutate(cl_prob="na_in_change_type") %>%
      dplyr::select(cl_prob,dplyr::everything())

    cl_problems_df<-dplyr::bind_rows(get0("cl_change_col_prob_df"), get0("cl_uuid_prob_df")) |>
      bind_rows(get0("na_change_type_prob_df"))

    if(nrow(cl_problems_df)>0){
      print("cleaning log has issues, see output table")
    }
    else{
      cl_problems_df<-"no issues in cleaning log found"

    }
    return(cl_problems_df)

  }
