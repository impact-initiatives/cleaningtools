#' @name auto_detect_sm_parents
#' @rdname auto_detect_sm_parents
#' @title Detect select multiple parent columns
#' @description `auto_detect_sm_parents` is mean to detect select multiple parent columns in a way that does
#' not rely on the XLSForm as the input
#' @param df a survey object or dataframe
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a list of select multiple parent columns in data set.
#' @export
auto_detect_sm_parents<- function(df, sm_sep="."){
  sm_parents<-sub(glue::glue('.[^\\{sm_sep}]*$'), '', colnames(df))
  sm_parents<-data.frame(col_names=sm_parents[sm_parents!=""])
  select_multiple_detected<-sm_parents %>%
    dplyr::group_by(col_names) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::filter(n>1) %>%
    dplyr::select(col_names)
  return(as.character(select_multiple_detected$col_names))

}
#' @name auto_sm_parent_child
#' @rdname auto_sm_parent_child
#' @title detect and group together select multiple parent and children columns
#' @description `auto_sm_parent_children` is mean to detect select multiple parent columns & children columns in a way that does
#' not rely on the XLSForm as the input
#' @param df a survey object or dataframe
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a data frame containing the the child select multiple columns alongside there parents
#' @export


auto_sm_parent_children<- function(df, sm_sep="."){
  sm_parents<-auto_detect_sm_parents(df, sm_sep)
  sm_child<- df %>%
    dplyr::select(dplyr::starts_with(glue::glue("{sm_parents}{sm_sep}"))) %>%
    colnames()
  dplyr::tibble(
    sm_parent=sub(glue::glue('.[^\\{sm_sep}]*$'),'',sm_child),
    sm_child
  )

}



#' This function recreates the concerted columns for select multiple questions
#' @param df data frame
#' @param uuid Unique ID column(Default is uuid).
#' @param kobo_survey_sheet Kobo survey tab
#' @param kobo_choices_sheet Kobo choices tab
#' @param sm_sep Separator between question and choice. Default is ..
#' @export
#' @examples
#' test_data <- dplyr::tibble(
#' uuid = paste0("uuid_",1:6),
#' gender = rep(c("male","female"),3),
#' reason = c("xx,yy","xx,zy",
#'           "zy","xx,xz,zy",
#'           NA_character_,"xz"),
#' reason.xx = c(0,1,0,1,0,0),
#' reason.yy = c(1,0,0,0,1,0),
#' reason.xz = c(0,0,0,1,0,1),
#' reason.zy = c(0,1,1,1,0,0),
#' reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_))
#' recreate_parent_column(df = test_data, uuid = "uuid", sm_sep = ".")
#'


recreate_parent_column <- function(df,
                                   uuid = "uuid",
                                   kobo_survey_sheet =NULL,
                                   kobo_choices_sheet =NULL,
                                   sm_sep= "."){

checked_data <- df

initial_order <- names(df)

if(is.null(kobo_survey_sheet)){
old_name <- names(df)
number_of_separator <- names(df) |> stringr::str_count(pattern = paste0("\\",sm_sep)) |> max(na.rm =T)
for (i in 1:number_of_separator) {
  names(df) <- sub(paste0("(\\",sm_sep,".*?)\\",sm_sep), "\\1_", names(df))
  }

cols_order <- df %>% names()


difference_df <- dplyr::tibble(
  old_name = old_name,
  new_name = cols_order
) |> dplyr::filter(old_name != new_name)

if(nrow(difference_df) > 0){
  warning("Column(s) names are renamed as multiple separators are found in dataset column names. Please see the above table with the new name.")

  print(difference_df)
}


select_multiple <- auto_sm_parent_children(df,sm_sep =sm_sep )
}

if(!is.null(kobo_survey_sheet)){
  choice_to_join <- kobo_choices_sheet |> dplyr::select(list_name,name)

  select_multiple <- kobo_survey_sheet|> dplyr::filter(grepl("select_multiple",type)) |> dplyr::select(type,name) |> dplyr::mutate(
    type = stringr::str_replace_all(type,"select_multiple ","")
  ) |> rename(
    list_name = type,
    sm_parent = name  ) |> dplyr::left_join(choice_to_join,multiple = "all",by ="list_name") |> mutate(
      sm_child = paste0(sm_parent,sm_sep,name)
    ) |> dplyr::select(sm_parent,sm_child)

  missing_column <- select_multiple$sm_child [!select_multiple$sm_child %in% names(df)]

  if(length(missing_column)>0){
    print(missing_column)
    warning(paste0("Ignoring the above column(s) as they do not exist in the dataset."))
}
  select_multiple <- select_multiple |> dplyr::filter(sm_child %in% names(df))

  }


if(nrow(select_multiple) > 0){
    select_multiple_list <- list()

    for (i in select_multiple$sm_parent) {
      select_multi_single <- select_multiple %>% dplyr::filter(sm_parent == i)
      concat_col <- select_multi_single$sm_parent %>% unique()
      choice_cols <- select_multi_single$sm_child %>% unique()

      df_only_cols <- df %>% dplyr::select(dplyr::all_of(choice_cols),dplyr::all_of(uuid))

      pivot_long <-  df_only_cols %>% dplyr::mutate_at(names(df_only_cols),as.character)

      final_df <- pivot_long %>% tidyr::pivot_longer(cols = !uuid,names_to = "cols" ,values_to = "value") %>%
        filter(value == 1 | value == TRUE | value == "1" | value == "TRUE") %>% dplyr::group_by(!!sym(uuid)) %>%
        dplyr::summarise(
          !!rlang::sym(concat_col):= paste0(cols,collapse = " ")
        )

      final_df[[concat_col]] <- final_df[[concat_col]] %>% stringr::str_replace_all(paste0(concat_col,"."),"")

      select_multiple_list[[concat_col]] <- final_df
    }

    final_df_for_export<- purrr::reduce(select_multiple_list, dplyr::full_join, by = uuid)
    concat_col_names_from_fina_export <-  final_df_for_export %>% dplyr::select(-dplyr::all_of(uuid)) %>% names()

    data_with_fix_concat <- df %>% dplyr::select(-dplyr::all_of(concat_col_names_from_fina_export)) %>%
      dplyr::left_join(final_df_for_export,by = uuid) ### added by uuid

    if(is.null(kobo_survey_sheet)){data_with_fix_concat <- data_with_fix_concat %>% dplyr::select(dplyr::all_of(cols_order))}
    if(!is.null(kobo_survey_sheet)){data_with_fix_concat <- data_with_fix_concat %>% dplyr::select(dplyr::all_of(initial_order))}

    change_log <- create_cleaning_log(raw_data = checked_data,
                                      raw_data_uuid = uuid,
                                      clean_data = data_with_fix_concat,
                                      clean_data_uuid = uuid
                                        )
    if("comment" %in% names(change_log)) {
      change_log <- change_log %>%
        dplyr::mutate(comment = gsub("An alteration was performed",
                                     "Parent column changed to match children columns",
                                     comment))
    }

    return(list(data_with_fix_concat = data_with_fix_concat,
                change_log =change_log))
  }

  if(nrow(select_multiple)== 0){
    return(list(data_with_fix_concat= checked_data,
           change_log = "No choice multiple questions/Nothing has changed"))
  }

}



