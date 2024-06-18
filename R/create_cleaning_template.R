#' Create a project folder with a cleaning template
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#' @param write_template_files TRUE/FALSE if to write template files
#'
#' @return Folder with a copy of the cleaning_template_folder
#'
#' @examples
#' \dontrun{
#' create_cleaning_template("path/to/folder")
#' }
create_cleaning_template <- function(folder_path, write_template_files, ...) {

  from <- system.file("impact_cleaning_template", package = "cleaningtools")
  fs::dir_copy(from, folder_path, overwrite = FALSE)

  if(write_template_files) {
    utils::write.csv(cleaningtools::cleaningtools_survey, paste0(folder_path, "/inputs/kobotool/kobo_survey.csv"), row.names = FALSE)
    utils::write.csv(cleaningtools::cleaningtools_choices, paste0(folder_path, "/inputs/kobotool/kobo_choices.csv"), row.names = FALSE)
    utils::write.csv(cleaningtools::cleaningtools_sample_frame, paste0(folder_path, "/inputs/sampling_frame.csv"), row.names = FALSE)
    utils::write.csv(cleaningtools::cleaningtools_raw_data, paste0(folder_path, "/inputs/cleaningtools_raw_data.csv"), row.names = FALSE)

  }

}
