

#' Add information to the dataset
#'
#' @param list A list file containing the cleaning_log and the dataset
#' @param dataset When If a list is provided, the element name represents the dataset; otherwise, it should be a dataframe.
#' @param cleaning_log If a list is provided, the element name represents the cleaning log; otherwise, it is should be a dataframe.
#' @param dataset_primary_key Primary key name from the dataset.(Default is X_uuid)
#' @param cleaning_log_primary_key Primary key name from the Cleaning log.(Default is uuid)
#' @param information_to_add Name of the columns to be added
#'
#' @return A list with cleaning log with added info and the dataset.
#' @export
#'
#' @examples
#' cleaningtools::cleaningtools_raw_data |> check_for_pii() |>
#'   check_duplicate(uuid_col_name = "X_uuid") |>
#'   check_for_value(uuid_col_name = "X_uuid") |>
#'   create_combined_log() |> add_info_to_cleaning_log()



add_info_to_cleaning_log <- function(list =NULL,
                     dataset= "checked_dataset",
                     cleaning_log = "cleaning_log",
                     dataset_primary_key= "X_uuid",
                     cleaning_log_primary_key= "uuid",
                     information_to_add =c("enumerator_num","date_assessment")
                     ){




  if(is.data.frame(list) & !is.null(list)){stop("The variable `list` represents a dataframe and can only take the values of either a list or NULL.")}

  if(!is.null(list)){


    if(!dataset %in% names(list)){stop(glue::glue("The element ", dataset, " is not present within the list."))}
    if(!cleaning_log %in% names(list)){stop(glue::glue("The element ", cleaning_log, " is not present within the list."))}
    dataset <- list[[dataset]]
    cleaning_log <- list[[cleaning_log]]
  }


  if(is.null(list)){


    if(!is.data.frame(dataset)){stop(glue::glue( "As the variable `list` is set to NULL, `dataset` must be a dataframe."))}

    if(!is.data.frame(cleaning_log)){stop(glue::glue("As the variable `list` is set to NULL, `cleaning_log` must be a dataframe."))}


  }





## cleaning_log_info_check

if(any(!cleaning_log_primary_key %in% names(cleaning_log))) { stop(
  cleaning_log_primary_key[!cleaning_log_primary_key %in% names(cleaning_log)]  |> glue::glue_collapse(", ") %>%
    glue::glue(. ," can not be found in the cleaning log."))

}


## dataset info check
if(any(!information_to_add %in% names(dataset))){stop(
  information_to_add[!c(information_to_add,dataset_primary_key) %in% names(dataset)] |> glue::glue_collapse(", ") %>%
    glue::glue(. ," can not be found in the dataset."))
}


dataset_raw <- dataset

dataset <- dataset |> dplyr::select(dplyr::all_of(information_to_add),dplyr::all_of(dataset_primary_key))
cleaning_log <- cleaning_log |> dplyr::select(-dplyr::any_of(information_to_add))


cleaning_log <- merge(cleaning_log, dataset,
                by.x = c(cleaning_log_primary_key),
                by.y = c(dataset_primary_key),
                all.x = TRUE)

output <- list()


output[["checked_dataset"]] <- dataset_raw
output[["cleaning_log"]] <- cleaning_log

output
}





