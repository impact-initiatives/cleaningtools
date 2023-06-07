#' Review discrepancy between kobo relevancies and the dataset.
#'
#' @param data dataset
#' @param uuid uuid column
#' @param kobo_survey Kobo survey sheet
#' @param sm_sep Separator for choice multiple
#' @param variables_to_add string vector optional, if to add some information to the log (today,village etc.)
#' @param questions_not_to_check Questions name which you do not want to check (telephone_number etc.. )
#' @return Any discrepancy between kobo relevancies and the dataset.
#' @export
#'
#' @examples
#' review_others(data = cleaningtools::cleaningtools_clean_data,
#' uuid = "X_uuid",kobo_survey = cleaningtools_survey)
#'
review_others <- function(data,
         uuid = "uuid",
         kobo_survey,
         sm_sep = ".",
         variables_to_add =NULL,
         questions_not_to_check =NULL
         ){


  ### variables to add not found
  if(any(!variables_to_add %in% names(data))){

    not_found <- variables_to_add[!variables_to_add %in% names(data)]

    msg <- not_found |> glue::glue_collapse(sep = ", ") %>%
      glue::glue("Following variables: ",., " cannot be found in the dataset.")
    stop(msg)

  }

  ### questions_not_to_check not found [warning]

  if(any(!questions_not_to_check %in% names(data))){

    not_found <- questions_not_to_check[!questions_not_to_check %in% names(data)]

    msg <- not_found |> glue::glue_collapse(sep = ", ") %>%
      glue::glue("Following variables: ",., " cannot be found in the dataset.")
    warning(msg)

  }


  ### UUID checkk error

  if(!uuid %in% names(data)){stop(paste0(uuid, " not found in the dataset."))}


all_logic <- create_logic_for_other(kobo_survey = kobo_survey ,
                                    compare_with_dataset = TRUE,
                                    data = data)



check  <- check_for_logical_with_list(.dataset = data,
                                      uuid_var = uuid,
                                      list_of_check = all_logic,
                                      check_id_column = "id",
                                      check_to_perform_column = "logic",
                                      variables_to_clean_column = "variables_to_clean_column",
                                      variables_to_add =variables_to_add,
                                      description_column = "description",
                                      bind_checks = T )

check$logical_all <- check$logical_all |> dplyr::filter(!check$logical_all$question %in% questions_not_to_check)
check$logical_all |> as.data.frame()
}


