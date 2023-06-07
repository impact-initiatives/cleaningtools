#' check outliers over the dataset
#'
#' @param df data frame or a list
#' @param kobo_survey Kobo survey sheet. Default is NULL.
#' @param kobo_choices Kobo choices sheet. Default is NULL
#' @param uuid_col_name UUID. Default is uuid
#' @param element_name name of the dataset in list
#' @param cols_to_add_cleaning_log Variables those must be included in the output
#' @param strongness_factor Strongness factor define how strong your outliers will be. The default is 3.
#' @param columns_to_remove Variables those must not consider for outlier checks even though they are numeric
#' @param remove_choice_multiple TRUE (default) will remove choice multiple questions from the output.
#' @param sm_sep Separator for choice multiple questions. The default is "."
#' @param minimum_unique_value_of_variable Default is NULL, mean this parameter won't be considered. For example 10 means for any variable where number of unique value is less than 10, then the variable won't be considered for outlier checking.
#' @return Outliers
#' @export
#' @examples
#' df_outlier<- data.frame(
#'  uuid = paste0("uuid_", 1:100),
#'  one_value = c(round(runif(90, min = 45,max =55)), round(runif(5)), round(runif(5,99,100))),
#'  expense = c(sample(200:500,replace = TRUE,size = 95),c(600,100,80,1020,1050)),
#'  income = c(c(60,0,80,1020,1050),sample(20000:50000,replace = TRUE,size = 95)),
#'  yy = c(rep(100,99),10)
#' )
#'
#' check_outliers(df = df_outlier,uuid_col_name = "uuid")


check_outliers <- function(df,
                           uuid_col_name = "uuid",
                           element_name = "checked_dataset",
                           kobo_survey= NULL,
                           kobo_choices = NULL,
                           cols_to_add_cleaning_log = NULL,
                           strongness_factor = 3,
                           minimum_unique_value_of_variable =NULL,
                           remove_choice_multiple = TRUE,
                           sm_sep = ".",
                           columns_to_remove= NULL){

  if(!is.list(df)){stop("Input must be a dataframe or list.")}
  if(is.null(kobo_survey) & !is.null(kobo_choices)){warning("Ignoring perameter `kobo_choices` as `kobo_survey` is not provided.")}
  if(is.null(kobo_choices) & !is.null(kobo_survey)){warning("Ignoring perameter `kobo_survey` as `kobo_choices` is not provided.")}

  ######### checking input
  dataframe <- df

  # if(is.data.frame(df) & !is.null(element_name)){warning("Input is a dataframe, ignoring element_name")}

  if(!is.data.frame(df) & is.list(df) ){
    if(is.null(element_name)){stop("element_name is missing")}
    if(!element_name %in% names(df)){stop("element_name not found")}
  }

  if(!is.data.frame(df) & is.list(df)){df <- df[[element_name]]}

  #######################


  df <- type.convert(df, as.is = TRUE,na.string= c(""," ")) |> dplyr::rename(
    uuid = !!rlang::sym(uuid_col_name)
  )

  cols_to_add_cleaning_log <- c(cols_to_add_cleaning_log,"uuid") |> unique()



  if(remove_choice_multiple == T ){
    all_select_multiple_parent <- auto_sm_parent_children(df,sm_sep = sm_sep)
    all_select_multiple_cols <-all_select_multiple_parent$sm_child
  }

  if(remove_choice_multiple == T ){columns_to_remove <- c(columns_to_remove,all_select_multiple_cols)}

  cols_to_remove<- columns_to_remove[!columns_to_remove %in% cols_to_add_cleaning_log]




  if(!is.null(cols_to_remove)){
    df <- df %>% dplyr::select(-dplyr::all_of(cols_to_remove))
  }



  if(!is.null(kobo_survey) & !is.null(kobo_choices)) {

    kobo_survey$name <- kobo_survey$name %>% stringr::str_replace_all("-",".")


    interger_column_in_kobo <- (kobo_survey %>% dplyr::filter(type == "integer") %>%
                                  dplyr::filter( !grepl('enumerator|_instance_|index', name)))$name

    cols_name_exist_in_loop_kobo <- interger_column_in_kobo[interger_column_in_kobo %in% names(df)]

  }

  cols_name_exist_in_loop_numeric <- df %>% dplyr::select_if(is.numeric) %>% dplyr::select(-starts_with("X"))%>% names()
  cols_name_exist_in_loop_int <- df %>% dplyr::select_if(is.integer) %>% dplyr::select(-starts_with("X"))%>% names()

  if(!is.null(kobo_survey) & !is.null(kobo_choices)) {
    cols_name_exist_in_loop <- c(cols_name_exist_in_loop_kobo,
                                 cols_name_exist_in_loop_numeric, ## added in case of new recoded variables
                                 cols_name_exist_in_loop_int) %>% unique()

  }




  if(is.null(kobo_survey) | is.null(kobo_choices)) {
    cols_name_exist_in_loop <- c(cols_name_exist_in_loop_numeric,
                                 cols_name_exist_in_loop_int) %>% unique()

  }

  outlier_checks <- list()

  for (x in cols_name_exist_in_loop) {
    print(paste0("checking_",x))

    df[[x]] <- df[[x]] %>% as.numeric()
    variable_value <- df[[x]]

    variable_value <- variable_value[!is.na(variable_value) & !is.null(variable_value) & !is.infinite(variable_value)]

    if(!is.null(minimum_unique_value_of_variable)){

      outliers_tf_nr <- (abs(variable_value - mean(variable_value)) > strongness_factor * sd(variable_value)) &
        (length(unique(variable_value)) > minimum_unique_value_of_variable)
    }


    if(is.null(minimum_unique_value_of_variable)){

      outliers_tf_nr <- abs(variable_value - mean(variable_value)) > strongness_factor * sd(variable_value)
    }



    outliers_value <- variable_value[outliers_tf_nr] %>% unique()



    outlier_checks[[x]]  <-  df %>% dplyr::mutate(
      issue = dplyr::case_when(df[[x]] %in% outliers_value ~"outlier (normal distribution)"),
    ) %>% dplyr::filter(issue == "outlier (normal distribution)") %>% dplyr::select(all_of(cols_to_add_cleaning_log),issue,all_of(x)) %>%
      tidyr::pivot_longer(cols = paste0(x),names_to ="question",values_to= "old_value")



    #### log checks ####

    df[["log"]] <- log(df[[x]]+1)


    log_variable <- df[["log"]]
    log_variable <- log_variable[!is.na(log_variable) & !is.null(log_variable) & !is.infinite(log_variable)]

    if(!is.null(minimum_unique_value_of_variable)){
      outliers_tf <- abs(log_variable - mean(log_variable)) > strongness_factor * sd(log_variable) &
        length(unique(log_variable)) > minimum_unique_value_of_variable
    }

    if(is.null(minimum_unique_value_of_variable)){
      outliers_tf <- abs(log_variable - mean(log_variable)) > strongness_factor * sd(log_variable)
    }



    outliers_value_log <- log_variable[outliers_tf] %>% unique()



    outlier_checks[[paste0("log_",x)]] <-  df %>% dplyr::mutate(
      issue = dplyr::case_when(df[["log"]] %in%  outliers_value_log ~ "outlier (log distribution)"),
    ) %>% dplyr::filter(issue == "outlier (log distribution)") %>% dplyr::select(all_of(cols_to_add_cleaning_log),issue,all_of(x)) %>%
      tidyr::pivot_longer(cols = paste0(x),names_to ="question",values_to= "old_value")


  }

  outliers_cl <- do.call(rbind,outlier_checks)

  outliers_cl <- outliers_cl %>% dplyr::distinct(!!!rlang::syms(cols_to_add_cleaning_log),question,old_value,.keep_all = T)


  potential_outliers <- outliers_cl %>% dplyr::filter(!question %in% columns_to_remove)


  ## Append the list
  if(is.data.frame(dataframe)){
    checked_dataset <- dataframe
    return(list(checked_dataset = checked_dataset,
                potential_outliers=potential_outliers))}

  if(!is.data.frame(dataframe)){
    list_Df <- list(potential_outliers =potential_outliers)

    return(append(dataframe,list_Df))
  }

}
