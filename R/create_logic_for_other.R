#' Create logical checks for "other" values.
#'
#' @param kobo_survey Kobo survey sheet.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @param compare_with_dataset Default FALSE will not compare the applicability of logics with the
#' dataset whereas TRUE will check the applicability of logics.
#' @param dataset dataset to be checked.
#' @return A dataframe will all logical tests to check "others" value. It should be used in
#' combination of review_others
#' @export
#' @examples
#' create_logic_for_other(
#'   kobo_survey = cleaningtools::cleaningtools_survey,
#'   sm_separator = ".",
#'   dataset = cleaningtools::cleaningtools_clean_data,
#'   compare_with_dataset = TRUE
#' )
#'
create_logic_for_other <- function(kobo_survey,
                                   sm_separator = ".",
                                   compare_with_dataset = FALSE,
                                   dataset = NULL) {
  my_bind_row <- get("bind_rows", asNamespace("dplyr"))



  if (compare_with_dataset == T & is.null(dataset)) {
    stop("Please provide the dataset")
  }

  list_of_logic <- list()


  survey_join <- kobo_survey |>
    dplyr::select(c("type", "name")) |>
    dplyr::filter(grepl("select_one|select_multiple", type))


  kobo_tool_tidy <- kobo_survey |>
    dplyr::filter(type == "text") |>
    dplyr::filter(lengths(regmatches(relevant, gregexpr("\\$", relevant))) == 1) |>
    dplyr::select(name, relevant) |>
    dplyr::mutate(
      parent = gsub(".*\\{(.+)\\}.*", "\\1", relevant),
      other_choice = gsub(".*\\'(.+)\\'.*", "\\1", relevant)
    ) |>
    dplyr::left_join(survey_join, by = c("parent" = "name")) |>
    dplyr::mutate(type = dplyr::case_when(grepl("_one", type) ~ "select_one", T ~ "select_multiple")) #|>

  #### looking for name
  if (compare_with_dataset == T) {
    if (!all(unique(kobo_tool_tidy$parent) %in% names(dataset))) {
      msg <- unique(kobo_tool_tidy$parent)[!unique(kobo_tool_tidy$parent) %in% names(dataset)] |>
        glue::glue_collapse(", ") %>%
        glue::glue("The following parent names: ", ., " were not found in the dataset. The function is ignoring them.")
      warning(msg)
    }
    kobo_tool_tidy <- kobo_tool_tidy |> dplyr::filter(parent %in% names(dataset))

    if (!all(unique(kobo_tool_tidy$name) %in% names(dataset))) {
      other_not_available_in_df <- unique(kobo_tool_tidy$name)[!unique(kobo_tool_tidy$name) %in% names(dataset)]
      kobo_tool_tidy_not_available_df <- kobo_tool_tidy |> dplyr::filter(name %in% other_not_available_in_df)

      list_of_logic[["kobo_tool_tidy_not_available_df_logic"]] <- kobo_tool_tidy_not_available_df |>
        dplyr::mutate(
          logic = dplyr::case_when(
            type == "select_one" ~
              paste0(parent, " == \"", other_choice, "\""),
            T ~ paste0(
              "(`", parent, sm_separator, other_choice, "` == TRUE | `",
              parent, sm_separator, other_choice, "` == 1 ) & !is.na(`", parent, sm_separator, other_choice, "`)"
            )
          )
        ) |>
        dplyr::mutate(
          description = dplyr::case_when(
            type == "select_one" ~ paste0(parent, " is selected but ", name, " is not found in the dataset"),
            T ~ paste0(parent, sm_separator, other_choice, " is selected but ", name, " is not found in the dataset")
          ),
          variables_to_clean_column = dplyr::case_when(
            type == "select_one" ~ paste0(parent), # ",",name),
            T ~ paste0(parent, sm_separator, other_choice) # ,",",name))
          ),
          associate_column_not_found = name
        ) |>
        dplyr::distinct()
    }

    kobo_tool_tidy <- kobo_tool_tidy |> dplyr::filter(name %in% names(dataset))
    ## parent must not_contain other

    parent_must_not_contain_other <- kobo_tool_tidy
  }

  ####### select one ###############

  kobo_select_one <- kobo_tool_tidy |> dplyr::filter(type == "select_one")


  if (nrow(kobo_select_one) > 0) {
    #### select one
    list_of_logic[["select_one_is_not_na"]] <- kobo_select_one |>
      dplyr::mutate(
        logic = paste0(
          "!is.na(", kobo_select_one$name, ") & (", kobo_select_one$parent, "!=",
          "\"", kobo_select_one$other_choice,
          "\"", ")"
        )
      ) |>
      dplyr::mutate(
        description = paste0(name, " has value but the ", parent, " column is not ", other_choice, "(Not matching with kobo relevancy.)"),
        variables_to_clean_column = paste0(parent, ",", name)
      ) |>
      dplyr::distinct()



    ### not other but the _other has value

    list_of_logic[["select_one_is_na"]] <- kobo_select_one |>
      dplyr::mutate(
        logic = paste0("is.na(", kobo_select_one$name, ") & (", kobo_select_one$parent, "==", "\"", kobo_select_one$other_choice, "\"", ")")
      ) |>
      dplyr::mutate(
        description = paste0(name, " is NA but the ", parent, " column is seleted as other/relevent choice(", other_choice, ")"),
        variables_to_clean_column = paste0(parent, ",", name)
      ) |>
      dplyr::distinct()
  }


  #### select multiple

  kobo_select_multiple <- kobo_tool_tidy |> dplyr::filter(type == "select_multiple")

  if (nrow(kobo_select_multiple) > 0) {
    list_of_logic[["select_multiple_is_not_na"]] <- kobo_select_multiple |>
      dplyr::mutate(
        logic = paste0(
          "!is.na(", name, ") & (`", parent, sm_separator, other_choice, "`==0 |`",
          parent, sm_separator, other_choice, "`==FALSE | is.na(`", parent, sm_separator, other_choice, "`))"
        )
      ) |>
      dplyr::mutate(
        description = paste0(name, " is NOT NA but the binary column ( ", parent, sm_separator, other_choice, ") is selected as FALSE/0/NA"),
        variables_to_clean_column = paste0(name, ",", parent, sm_separator, other_choice)
      ) |>
      dplyr::distinct()



    list_of_logic[["select_multiple_is_na"]] <- kobo_select_multiple |>
      dplyr::mutate(
        logic = paste0(
          "is.na(", name, ") & (`", parent, sm_separator, other_choice, "`==1 |`",
          parent, sm_separator, other_choice, "`== TRUE)"
        )
      ) |>
      dplyr::mutate(
        description = paste0(name, " is NA but the binary column (", parent, sm_separator, other_choice, ")is selected as TRUE/1"),
        variables_to_clean_column = paste0(name, ",", parent, sm_separator, other_choice)
      ) |>
      dplyr::distinct()
  }

  all_logic <- do.call("my_bind_row", list_of_logic)


  all_logic |>
    dplyr::mutate(id = paste("id-", dplyr::cur_group_rows())) |>
    dplyr::select(id, logic, description, variables_to_clean_column)
}
