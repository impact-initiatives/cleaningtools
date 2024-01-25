#' Compares the sample frame with the clean data
#'
#' @param sample_frame A sample frame. It mush have a column for strata and another column defining the number of needed survey per strata.
#' @param sampling_frame_strata_column Column name for strata in the sample frame
#' @param sampling_frame_target_survey_column Column name defining number of survey per strata in sample frame.
#' @param clean_dataset Clean data
#' @param clean_dataset_strata_column Column name for strata in clean data
#' @param consent_column Dataset consent column
#' @param consent_yes_value Choice for consent yes
#' @return a dataset with the two additional columns with the number of completed surveys and number of remaining survey.
#' @export
#'
#' @examples
#' review_sample_frame_with_dataset(
#'   sample_frame = cleaningtools::cleaningtools_sample_frame,
#'   sampling_frame_strata_column = "Neighbourhood",
#'   sampling_frame_target_survey_column = "Total.no.of.HH",
#'   clean_dataset = cleaningtools::cleaningtools_clean_data,
#'   clean_dataset_strata_column = "neighbourhood",
#'   consent_column = "consent_remote",
#'   consent_yes_value = "yes"
#' )
review_sample_frame_with_dataset <- function(sample_frame,
                                             sampling_frame_strata_column,
                                             sampling_frame_target_survey_column = "Total.no.of.HH",
                                             clean_dataset,
                                             clean_dataset_strata_column,
                                             consent_column = "consent",
                                             consent_yes_value = "yes") {
  if (!consent_column %in% names(clean_dataset)) {
    stop(paste0(consent_column, " not found in the clean data"))
  }

  consent_value_not_found <- consent_yes_value[!consent_yes_value %in% unique(clean_dataset[[consent_column]])]
  if (length(consent_value_not_found) > 0) {
    stop(paste0(consent_value_not_found, " not found in the consent column"))
  }


  clean_dataset <- clean_dataset |> dplyr::filter(!!rlang::sym(consent_column) %in% consent_yes_value)


  if (!sampling_frame_strata_column %in% names(sample_frame)) {
    stop(paste0(sampling_frame_strata_column, " not found in the sample frame"))
  }
  if (!sampling_frame_target_survey_column %in% names(sample_frame)) {
    stop(paste0(sampling_frame_target_survey_column, " not found in the sample frame"))
  }
  if (!clean_dataset_strata_column %in% names(clean_dataset)) {
    stop(paste0(clean_dataset_strata_column, " not found in the clean data"))
  }
  if (any(names(sample_frame) %in% c("Collected", "Renaming"))) {
    warning("Dataset has Collected/Renaming column already, which will be replaced.")
  }

  cl_unique_strata <- unique(clean_dataset[[clean_dataset_strata_column]])
  if (any(!cl_unique_strata %in% sample_frame[[sampling_frame_strata_column]])) {
    print(cl_unique_strata[!cl_unique_strata %in% sample_frame[[sampling_frame_strata_column]]])
    stop("The above strata was not found in the sample frame.")
  }

  actual_df <- clean_dataset |>
    dplyr::group_by(!!rlang::sym(clean_dataset_strata_column)) |>
    dplyr::summarise(
      Collected = dplyr::n()
    )

  sample_frame |>
    dplyr::select(-dplyr::any_of(c("Collected", "Remaining"))) |>
    dplyr::left_join(actual_df, by = stats::setNames(clean_dataset_strata_column, sampling_frame_strata_column)) |>
    dplyr::mutate(Collected = dplyr::case_when(is.na(Collected) ~ 0, T ~ Collected)) |>
    dplyr::mutate(
      Remaining = !!rlang::sym(sampling_frame_target_survey_column) - Collected
    )
}
