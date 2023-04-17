#
# # find_parent_and_other column from kobo
#
# sep = "."
# survey_c <- survey |> dplyr::filter(type == "text")
# survey_join <- survey |> dplyr::select(c("type","name")) |> dplyr::filter(grepl("select_one|select_multiple", type))
#
# kobo_tool_tidy <- survey_c |> dplyr::filter(type == "text") |>
#   dplyr::filter(lengths(regmatches(relevant, gregexpr("\\$", relevant)))==1) |>
#   dplyr::select(name,relevant) |> mutate(
#   parent = gsub(".*\\{(.+)\\}.*", "\\1", relevant),
#   other_choice = gsub(".*\\'(.+)\\'.*", "\\1", relevant)) |>
#   dplyr::left_join(survey_join,by = c("parent"="name")) |>
#   dplyr::mutate(type= dplyr::case_when(grepl("_one",type) ~ "select_one",T~"select_multiple")) #|>
#   # dplyr::mutate(question = dplyr::case_when(type == "select_one"~parent,T~paste0(parent,sep,other_choice)))
#
# ####### select one ###############
#
# kobo_select_one <- kobo_tool_tidy |> dplyr::filter(type == "select_one")
#
#
# if(!all(unique(kobo_select_one$parent) %in% names(clean_data))){
#   print(unique(kobo_select_one$parent)[!unique(kobo_select_one$parent)%in% names(clean_data)])
#   warning("The above name were not found in the dataset.The function is ignoring them.")
# }
#
# kobo_select_one <- kobo_select_one |> dplyr::filter(parent %in% names(clean_data))
#
# # jodi name is not null hoy then parent hobe other choice
# # jodi name NA hoy and parent !=other tahole cleaning issue.
# #### select one
# logical_check_df_select_one <- kobo_select_one |> mutate(
#   logic = paste0("is.na(",kobo_select_one$name,") & (", kobo_select_one$parent, "!=",kobo_select_one$other_choice,
#                  " & !is.na(",kobo_select_one$parent ,"))")
# )  |> dplyr::mutate(
#   description = "Other column is NA but the parent column is not the corrospondeing with the relevancy",
#   variables_to_clean_column = parent) |> dplyr::distinct()
#
# #### select multiple
#
#
