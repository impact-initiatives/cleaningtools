

#' Create enumerator performance tables and add them to the cleaning logbook
#'
#' @param raw_data Raw Dataset
#' @param cleaning_log Cleaning Log
#' @param col_enum Enumerator column name in both raw_data and cleaning_log. Must be the same and by default NA
#' @param issue_col Name of the issue column in the cleaning log. By Default "issue"
#' @param link_cleaning_logbook URL link to the RDD cleaning logbook template containing the sheet "04_Enumerator - performance"
#' @param filename URL link for the location of the updated cleaning logbook
#'
#' @return Return a list of 5 named tables:
#'         - "Number of surveys collected by enumerators"
#'         - "Number of changes by enumerators"
#'         - "Number of changes by enumerators filtered by issues"
#'         - "Number of deletions by enumerators"
#'         - "Number of deletions by enumerators"
#' And update the cleaning logbook with the new tables
#' @export
#' @examples
#' \dontrun{
#' create_enumerator_performance(
#' raw_data = cleaningtools::cleaningtools_raw_data,
#' cleaning_log = cleaningtools::cleaningtools_cleaning_log,
#' col_enum = "enumerator_num", issue_col = "issue",
#' link_cleaning_logbook = "Research_cycle_data_cleaning_logbook_template_v3_202208_FINAL.xlsx",
#' filename = "new_cleaning_logbook_draft.xlsx"
#' )}
create_enumerator_performance <- function(raw_data,
                                          cleaning_log,
                                          col_enum = NA,
                                          issue_col = "issue",
                                          link_cleaning_logbook = NA,
                                          filename = NA){
  ## check if col_enum in raw_data, clean_data, and cleaning_log
  if(!col_enum %in% colnames(raw_data)) stop("Enumerator column not found in raw_data")
  if(!col_enum %in% colnames(cleaning_log)) stop("Enumerator column not found in cleaning_log")

  ## check if issue cleaning_log
  if(!issue_col %in% colnames(cleaning_log)) stop("Issue column not found in cleaning_log")

  ## number of surveys collected by enumerators
  count_enum_collected <- raw_data %>%
    group_by(!!sym(col_enum)) %>%
    summarize(Number = n()) %>%
    arrange(col_enum) %>%
    as.data.frame()

  ## number of changes done by enumerators
  count_enum_changes <- cleaning_log %>%
    filter(!change_type %in% c("remove_survey",
                               "no_action")) %>%
    group_by(!!sym(col_enum)) %>%
    summarize(Number = n()) %>%
    arrange(col_enum) %>%
    as.data.frame()

  ## number of changes by type of issue done by enumerators
  count_enum_changes_issue <- cleaning_log %>%
    filter(!change_type %in% c("remove_survey",
                               "no_action")) %>%
    group_by(!!sym(col_enum), !!sym(issue_col)) %>%
    summarize(Number = n()) %>%
    arrange(col_enum) %>%
    as.data.frame()

  ## number of deleted surveys by enumerator
  count_enum_deletion <- cleaning_log %>%
    filter(change_type == "remove_survey") %>%
    group_by(!!sym(col_enum)) %>%
    summarize(Number = n()) %>%
    arrange(col_enum) %>%
    as.data.frame()

  ## number of deleted surveys by issue by enumerator
  count_enum_deletion_issue <- cleaning_log %>%
    filter(change_type == "remove_survey") %>%
    group_by(!!sym(col_enum), !!sym(issue_col)) %>%
    summarize(Number = n()) %>%
    arrange(col_enum) %>%
    as.data.frame()

  named_table = list("Number of surveys collected by enumerators" = count_enum_collected,
                     "Number of changes by enumerators" = count_enum_changes,
                     "Number of changes by enumerators filtered by issues" = count_enum_changes_issue,
                     "Number of deletions by enumerators" = count_enum_deletion,
                     "Number of deletions by enumerators filtered by issues" = count_enum_deletion_issue)

  ## Writing tables to cleaning_logbook
  if(is.na(link_cleaning_logbook)) {
    cat("Link for cleaning_logbook wasn't provided and a new workbook will be created")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "04_Enumerator - performance")
  }else {
    wb <- openxlsx::loadWorkbook(link_cleaning_logbook)
  }

  openxlsx::writeData(wb = wb,
                      x = named_table[["Number of surveys collected by enumerators"]],
                      sheet = "04_Enumerator - performance",
                      startRow = 9, startCol = 1)

  openxlsx::writeData(wb = wb,
                      x = named_table[["Number of changes by enumerators"]],
                      sheet = "04_Enumerator - performance",
                      startRow = 9, startCol = 7)

  openxlsx::writeData(wb = wb,
                      x = named_table[["Number of changes by enumerators filtered by issues"]],
                      sheet = "04_Enumerator - performance",
                      startRow = 9, startCol = 13)

  openxlsx::writeData(wb = wb,
                      x = named_table[["Number of deletions by enumerators"]],
                      sheet = "04_Enumerator - performance",
                      startRow = 9, startCol = 19)

  openxlsx::writeData(wb = wb,
                      x = named_table[["Number of deletions by enumerators filtered by issues"]],
                      sheet = "04_Enumerator - performance",
                      startRow = 9, startCol = 25)
  if(is.na(filename)){
    cat("Link to where the new cleaning_logbook should be saved is not provided, new cleaning_logbook will be saved in the same directory of the script.")
    filename <- paste0("data_cleaning_workbook.xlsx")
  }

  openxlsx::saveWorkbook(wb, filename, overwrite = T)

  return(named_table)
}

