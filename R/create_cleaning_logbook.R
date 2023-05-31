

#' Create data_exstract, logbook, deletion_log tables and add them to the cleaning logbook template
#'
#' @param raw_data Raw Dataset
#' @param cleaning_log Cleaning Log
#' @param col_enum Enumerator column name in both raw_data and cleaning_log. Must be the same and by default NA
#' @param filename URL link for the location of the updated cleaning logbook
#' @param uuid
#'
#' @return Return a list of 3 tables:
#'         - "template_data_extract"
#'         - "template_logbook"
#'         - "template_deletion_log"
#' And update the cleaning logbook with the new tables and automatically update the enumerator performance table
#' @export
#' @examples
#' \dontrun{
#' create_enumerator_performance(
#' raw_data = cleaningtools::cleaningtools_raw_data,
#' cleaning_log = cleaningtools::cleaningtools_cleaning_log,
#' col_enum = "enumerator_num",
#' filename = "new_cleaning_logbook_draft.xlsx"
#' )}
create_cleaning_logbook <- function(raw_data,
                                          cleaning_log,
                                          uuid = "X_uuid",
                                          col_enum = NA,
                                          # link_cleaning_logbook = NA,
                                          filename = NA){
  ## check if col_enum in raw_data and cleaning_log
  if(!col_enum %in% colnames(raw_data)) stop("Enumerator column not found in raw_data")
  if(!col_enum %in% colnames(cleaning_log)) stop("Enumerator column not found in cleaning_log")

  ## check if uuid in raw_data and cleaning_log
  if(!uuid %in% colnames(raw_data)) stop("uuid column not found in raw_data")
  if(!uuid %in% colnames(cleaning_log)) stop("uuid column not found in cleaning_log")

  ## Create the template_data_extract table
  template_data_extract <- raw_data %>%
    dplyr::select(all_of(uuid), all_of(col_enum)) %>%
    dplyr::rename("enumerator ID"= all_of(col_enum),
                  "uuid" = all_of(uuid)) %>%
    as.data.frame()

  ## create a device id df
  deviceiddf <- raw_data %>%
    dplyr::select(all_of(uuid), deviceid)

  ## Create the template_logbook table
  template_logbook <- cleaning_log %>%
    dplyr::filter(!change_type %in% c("remove_survey")) %>%
    dplyr::left_join(deviceiddf, by = uuid) %>%
    dplyr::mutate(`Type of Issue (Select from dropdown list)` = NA,
                  changed = ifelse(change_type %in% c("change_response","blank_response"),"Yes","No")) %>%
    dplyr::select(all_of(uuid),all_of(col_enum),deviceid,questions,issue,`Type of Issue (Select from dropdown list)`,
                  reason,changed,old_value,new_value) %>%
    dplyr::rename("uuid" = all_of(uuid),
                  "Enumerator ID" = all_of(col_enum),
                  "device ID" = deviceid,
                  "question.name" = questions,
                  "feedback" = reason,
                  "old.value" = old_value, "new.value" = new_value) %>%
    as.data.frame()

  ## Create the template_deletion_log table
  template_deletion_log <- cleaning_log %>%
    dplyr::filter(change_type %in% c("remove_survey")) %>%
    dplyr::left_join(deviceiddf, by = uuid) %>%
    dplyr::mutate(`Type of Issue (Select from dropdown list)` = NA) %>%
    dplyr::select(all_of(uuid),all_of(col_enum),deviceid,issue,`Type of Issue (Select from dropdown list)`,reason) %>%
    dplyr::rename("uuid" = all_of(uuid),
                  "Enumerator ID" = all_of(col_enum),
                  "device ID" = deviceid,
                  "Issue" = issue,
                  "feedback" = reason)%>%
    as.data.frame()

  named_table = list("template_data_extract" = template_data_extract,
                     "template_logbook" = template_logbook,
                     "template_deletion_log" = template_deletion_log)

  ## Writing tables to cleaning_logbook
  wb <- openxlsx::loadWorkbook(system.file("extdata","Research_cycle_data_cleaning_logbook_template_v4_202331_AA.xlsx",package = "cleaningtools"))

  openxlsx::writeData(wb = wb,
                      x = named_table[["template_data_extract"]],
                      sheet = "01_TEMPLATE_data_extract",
                      startRow = 1, startCol = 1)

  openxlsx::writeData(wb = wb,
                           x = named_table[["template_logbook"]],
                           sheet = "02_TEMPLATE_Logbook",
                           startRow = 1,startCol = 1)


  openxlsx::writeData(wb,
                           x = named_table[["template_deletion_log"]],
                           sheet = "03_TEMPLATE_deletion log",
                           startCol = 1,startRow = 1)

  if(is.na(filename)){
    cat("Link to where the new cleaning_logbook should be saved is not provided, new cleaning_logbook will be saved in the same directory of the script.")
    filename <- paste0("data_cleaning_workbook.xlsx")
  }

  openxlsx::saveWorkbook(wb, filename, overwrite = T)

  return(named_table)
}
