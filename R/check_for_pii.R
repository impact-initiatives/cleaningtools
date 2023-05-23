#' Checks for potential PII
#' @param df Data set
#' @param uuid  Column name defining the uuid
#' @param element_name If the input is a list file, please specify the element name that contains the dataset
#' @param words_to_look Specify the words that might be the PIIs
#' @return dataset with potential PII
#' @import dplyr tibble snakecase
#' @export

check_for_pii<- function(df,element_name =NULL,uuid ="X_uuid",words_to_look = NULL){

  dataframe <- df

  if(is.data.frame(df) & !is.null(element_name)){warning("Input is a dataframe, ignoring element_name")}

  if(!is.data.frame(df) & is.list(df) ){
    if(is.null(element_name)){stop("element_name is missing")}
    if(!element_name %in% names(df)){stop("element_name not found")}
  }

  if(!is.data.frame(df) & is.list(df)){df <- df[[element_name]]}


  if(uuid %in% names(df)==FALSE){stop("uuid not found in the dataset")}


  cols_to_look_for <- c("telephone","contact","name","gps","neighbourhood","latitude","longitude","phone",
                        "contact number", "geo location","geo",
                        "contact","nom","gps","voisinage")

  cols_to_look_for <- c(words_to_look,cols_to_look_for) %>% to_snake_case()
  cols_to_look_for <-   paste0(cols_to_look_for,collapse = "|")

  select_multiple_to_ignore <- auto_sm_parent_children(df)
  ignore <- c(select_multiple_to_ignore$sm_child, select_multiple_to_ignore$sm_parent) %>% unique()

  potential_PII <- tibble(
    uuid= "all",
    question = names(df)
  ) %>% dplyr::filter(!question %in% ignore) %>% dplyr::mutate(
    snkae_case_cols = snakecase::to_snake_case(question)
  ) %>% dplyr::filter((grepl(cols_to_look_for,snkae_case_cols)) |(grepl(cols_to_look_for,question))) %>% dplyr::mutate(
    issue = "Potential PII"
  ) %>% dplyr::select(-snkae_case_cols)
  ## Append the list
  if(is.data.frame(dataframe)){
    checked_dataset <- df
    return(list(checked_dataset = checked_dataset,
                potential_PII=potential_PII))}

  if(!is.data.frame(dataframe)){
    list_Df <- list(potential_PII =potential_PII)

    return(append(dataframe,list_Df))
    }






}
