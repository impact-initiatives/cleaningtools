#' Compares the sample frame with the clean data
#'
#' @param sample_frame A sample frame. It mush have a column for strata and another column defining the number of needed survey per strata.
#' @param sample_frame_strata_col Column name for strata in the sample frame
#' @param sample_frame_target_survey_col Column name defining number of survey per strata in sample frame.
#' @param clean_data Clean data
#' @param clean_data_strata_column Column name for strata in clean data
#' @param consent_column Dataset consent column
#' @param value_for_consent_yes Choice for consent yes
#' @return a dataset with the two additional columns with the number of completed surveys and number of remaining survey.
#' @export
#'
#' @examples
#' review_sample_frame_with_dataset(sample_frame = cleaningtools::cleaningtools_sample_frame ,
#' sample_frame_strata_col = "Neighbourhood",
#' sample_frame_target_survey_col ="Total.no.of.HH",
#' clean_data = cleaningtools::cleaningtools_clean_data,
#' clean_data_strata_column = "neighbourhood",
#' consent_column = "consent_remote",
#' value_for_consent_yes = "yes")

review_sample_frame_with_dataset <- function(sample_frame =sample_frame,
                                             sample_frame_strata_col,
                                             sample_frame_target_survey_col = "Total.no.of.HH",
                                             clean_data=clean_data,
                                             clean_data_strata_column,
                                             consent_column = "consent",
                                             value_for_consent_yes = "yes"){

if(!consent_column %in% names(clean_data)){stop(paste0(consent_column, " not found in the clean data"))}

consent_value_not_found <-  value_for_consent_yes[!value_for_consent_yes %in% unique(clean_data[[consent_column]])]
if(length(consent_value_not_found) > 0){stop(paste0(consent_value_not_found, " not found in the consent column"))}


clean_data <-clean_data |> dplyr::filter(!!rlang::sym(consent_column) %in% value_for_consent_yes )


if(!sample_frame_strata_col %in% names(sample_frame)){stop(paste0(sample_frame_strata_col, " not found in the sample frame"))}
if(!sample_frame_target_survey_col %in% names(sample_frame)){stop(paste0(sample_frame_target_survey_col, " not found in the sample frame"))}
if(!clean_data_strata_column %in% names(clean_data)){stop(paste0(clean_data_strata_column, " not found in the clean data"))}
if(any(names(sample_frame) %in% c("Collected","Renaming"))){warning("Dataset has Collected/Renaming column already, which will be replaced.")}

cl_unique_strata <- unique(clean_data[[clean_data_strata_column]])
if(any(!cl_unique_strata %in% sample_frame[[sample_frame_strata_col]])){
  print(cl_unique_strata[!cl_unique_strata %in%  sample_frame[[sample_frame_strata_col]]])
  stop("The above strata was not found in the sample frame.")}

actual_df <- clean_data |> dplyr::group_by(!!rlang::sym(clean_data_strata_column)) |> dplyr::summarise(
    Collected = dplyr::n()
  )

  sample_frame |> dplyr::select(-dplyr::any_of(c("Collected","Remaining"))) |>
    dplyr::left_join(actual_df,by = setNames(clean_data_strata_column, sample_frame_strata_col)) |>
    dplyr::mutate(Collected = dplyr::case_when(is.na(Collected) ~0,T~Collected)) |>
    dplyr::mutate(
      Remaining = !!rlang::sym(sample_frame_target_survey_col) - Collected
    )

}




