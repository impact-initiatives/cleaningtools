#'
#' Check for value(s) in the dataset
#'
#' @param df data frame or a list
#' @param uuid_col_name UUID
#' @param element_name Name of the dataset in list
#' @param values_to_look Values to look. Default are- 99,999,999,88,888,888
#' @return A dataframe as cleaning log format
#' @export
#' @examples
#' df <- data.frame(
#' X_uuid = paste0("uuid_",1:100),
#' age = c(sample(18:80,replace = TRUE,size = 96),99,99,98,88),
#' gender = c("99",sample(c("male","female"),
#' replace = TRUE,size = 95),"98","98","88","888"))
#' check_for_value(df = df,uuid_col_name = "X_uuid",
#' element_name = "checked_dataset",
#' values_to_look = c(99,98,88,888))
#'


check_for_value <- function(df,
                            uuid_col_name,
                            element_name = "checked_dataset",
                            values_to_look = c(99,999,999,88,888,888)){

values_to_look <- values_to_look |> as.character()


######### checking input

if(!is.list(df)){stop("Input must be a dataframe or list.")}

checked_dataset <- df

if(!is.data.frame(df) & is.list(df) ){
    if(is.null(element_name)){stop("element_name is missing")}
    if(!element_name %in% names(df)){stop("element_name not found")}}

if(!is.data.frame(df) & is.list(df)){df <- df[[element_name]]}

#######################


df <- df |> rename(uuid = !!rlang::sym(uuid_col_name))
df <- df |> mutate_all(as.character)

df_only_na <- df |> filter_all(any_vars(. %in% values_to_look))

flaged_value <- df_only_na |> tidyr::pivot_longer(cols = !uuid) |> dplyr::filter(value %in% values_to_look) |>
  rename(question = name,
         old_value = value)

## create output
if(is.data.frame(checked_dataset)){

  return(list(checked_dataset = checked_dataset,
              flaged_value=flaged_value))}

if(!is.data.frame(checked_dataset)){
  list_Df <- list(flaged_value =flaged_value)

  return(append(checked_dataset,list_Df))
}


}


