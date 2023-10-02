#' Creates formatted workbook with openxlsx
#'
#' @param write_list List of dataframe
#' @param header_front_color Hexcode for header front color (default is white)
#' @param header_front_size Header front size (default is 12)
#' @param header_fill_color Hexcode for header fill color (default is red)
#' @param header_front Front name for header (default is Arial Narrow)
#' @param body_front Front name for body (default is Arial Narrow)
#' @param body_front_size Front size for body (default is 11)
#' @param column_for_color Column name in the dataframe which should be used for colorizing the cell. The default is null.
#'
#' @return A workbook
#' @export
#'
#' @examples
#' checks_list <- cleaningtools::cleaningtools_raw_data |>
#'   check_pii(uuid_column = "X_uuid") |>
#'   check_duplicate(uuid_column = "X_uuid") |>
#'   check_value(uuid_column = "X_uuid")
#'
#' create_combined_log(list_of_log = checks_list) |>
#'   create_formated_wb()
create_formated_wb <- function(write_list,
                               column_for_color = NULL,
                               header_front_size = 12,
                               header_front_color = "#FFFFFF",
                               header_fill_color = "#ee5859",
                               header_front = "Arial Narrow",
                               body_front = "Arial Narrow",
                               body_front_size = 11) {
  headerStyle <- openxlsx::createStyle(
    fontSize = header_front_size,
    fontColour = header_front_color,
    halign = "center",
    valign = "center",
    fontName = header_front,
    textDecoration = "bold",
    fgFill = header_fill_color,
    border = "TopBottomLeftRight ",
    borderColour = "#fafafa",
    wrapText = T
  )

  bodyStyle <- openxlsx::createStyle(
    fontSize = body_front_size,
    fontName = body_front,
    border = "TopBottomLeftRight ",
    borderColour = "#4F81BD",
    valign = "center",
    halign = "left"
  )

  wb <- openxlsx::createWorkbook()

  number_of_sheet <- length(write_list)

  for (i in 1:number_of_sheet) {
    dataset_name <- names(write_list[i])
    dataset <- write_list[[dataset_name]] |> as.data.frame()

    openxlsx::addWorksheet(wb, dataset_name)
    openxlsx::writeData(wb, sheet = i, dataset, rowNames = F)
    openxlsx::addFilter(wb, sheet = i, row = 1, cols = 1:ncol(dataset))
    openxlsx::freezePane(wb, sheet = i, firstCol = TRUE, firstRow = T)
    openxlsx::addStyle(wb, sheet = i, headerStyle, rows = 1, cols = 1:ncol(dataset), gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet = i, bodyStyle, rows = 1:nrow(dataset) + 1, cols = 1:ncol(dataset), gridExpand = TRUE)
    openxlsx::setColWidths(wb, i, cols = 1:ncol(dataset), widths = 25)
    openxlsx::setRowHeights(wb, i, 1, 20)

    if (!is.null(column_for_color)) {
      u <- unique(dataset[[column_for_color]])

      for (x in u) {
        y <- which(dataset[[column_for_color]] == x)

        random.color <- randomcoloR::randomColor(1, luminosity = "light")

        style <- openxlsx::createStyle(
          fgFill = random.color,
          fontSize = body_front_size,
          fontName = body_front,
          border = "TopBottomLeftRight ",
          borderColour = "#4F81BD",
          valign = "center",
          halign = "left"
        )


        openxlsx::addStyle(wb, sheet = i, style, rows = y + 1, cols = 1:ncol(dataset), gridExpand = TRUE)
      }
    }
  }

  wb
}



#' Creates formatted excel for cleaning log
#'
#' @param write_list List of dataframe
#' @param cleaning_log_name Name for cleaning log from write_list
#' @param change_type_col Change type column name in the cleaning log
#' @param header_front_color Hexcode for header front color (default is white)
#' @param header_front_size Header front size (default is 12)
#' @param header_fill_color Hexcode for header fill color (default is red)
#' @param header_front Front name for header (default is Arial Narrow)
#' @param body_front Front name for body (default is Arial Narrow)
#' @param body_front_size Front size for body (default is 11)
#' @param column_for_color Column name in the dataframe which should be used for colorizing the cell. The default is null.
#' @param use_dropdown Use drop down lists for data validation in the cleaning log output (default is FALSE)
#' @param sm_dropdown_type Dropdown list options for select multiple questions: numerical (1/0) or logical (TRUE/FALSE) - default is logical
#' @param kobo_survey Kobo survey dataframe
#' @param kobo_choices Kobo choices dataframe
#' @param output_path Output path. Default is NULL which will return a workbook instead of an excel file.
#'
#' @return save a .xlsx file or return a workbook object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' checks_list <- cleaningtools::cleaningtools_raw_data |>
#'   check_pii(uuid_column = "X_uuid") |>
#'   check_duplicate(uuid_column = "X_uuid") |>
#'   check_value(uuid_column = "X_uuid")
#' create_combined_log(list_of_log = checks_list) |>
#'   create_xlsx_cleaning_log()
#'
#' logical_check_example <- cleaningtools::cleaningtools_raw_data |>
#'   check_logical(
#'     check_to_perform = 'treat_cook_water == "always_treat"',
#'     uuid_column = "X_uuid",
#'     description = "description",
#'     check_id = "check_4",
#'     columns_to_clean = "treat_cook_water"
#'   )
#' create_combined_log(logical_check_example) |>
#'   create_xlsx_cleaning_log(
#'     output_path = paste0(tempdir(check = TRUE), "/cleaning_log.xlsx"),
#'     cleaning_log_name = "cleaning_log",
#'     change_type_col = "change_type",
#'     kobo_survey = cleaningtools::cleaningtools_survey,
#'     kobo_choices = cleaningtools::cleaningtools_choices,
#'     use_dropdown = TRUE,
#'     sm_dropdown_type = "logical"
#'   )
#' }
create_xlsx_cleaning_log <- function(write_list,
                                     cleaning_log_name = "cleaning_log",
                                     change_type_col = "change_type",
                                     column_for_color = "check_binding",
                                     header_front_size = 12,
                                     header_front_color = "#FFFFFF",
                                     header_fill_color = "#ee5859",
                                     header_front = "Arial Narrow",
                                     body_front = "Arial Narrow",
                                     body_front_size = 11,
                                     use_dropdown = F,
                                     sm_dropdown_type = NULL,
                                     kobo_survey = NULL,
                                     kobo_choices = NULL,
                                     output_path = NULL) {
  if (use_dropdown & (is.null(kobo_survey) | is.null(kobo_choices))) {
    stop(glue::glue("Kobo survey and choices sheets should be provided to use dropdown lists"))
  }
  if (!is.null(kobo_survey) && !verify_valid_survey(kobo_survey)) {
    stop(glue::glue("The Kobo survey dataframe is not valid"))
  }
  if (!is.null(kobo_choices) && !verify_valid_choices(kobo_choices)) {
    stop(glue::glue("The Kobo choices dataframe is not valid"))
  }
  if (!is.null(sm_dropdown_type) && !stringr::str_to_lower(sm_dropdown_type) %in% c("logical", "numerical")) {
    stop(glue::glue("Invalid value for sm_dropdown_type - only 'logical' and 'numerical' are accepted"))
  }
  if (!cleaning_log_name %in% names(write_list)) {
    stop(glue::glue(cleaning_log_name, " not found in the given list."))
  }
  if (!change_type_col %in% names(write_list[[cleaning_log_name]])) {
    stop(glue::glue(change_type_col, " not found in ", cleaning_log_name, "."))
  }
  if ("validation_rules" %in% names(write_list)) {
    stop(glue::glue("The list currently has an element named `validation_rules`. Please consider renaming it."))
  }

  tryCatch(
    if (!is.null(kobo_survey) & !is.null(kobo_choices) & use_dropdown == TRUE) {
      data.val <- create_validation_list(kobo_choices, kobo_survey |> dplyr::filter(!stringr::str_detect(pattern = "(begin|end)(\\s+|_)group", type)))
    },
    error = function(e) {
      warning("Validation list was not created")
    }
  )

  if (!is.null(kobo_survey) & !is.null(kobo_choices) & use_dropdown == TRUE & exists("data.val", inherits = FALSE)) {
    write_list[["validation_rules"]] <- data.val
  } else {
    write_list[["validation_rules"]] <- data.frame(
      change_type_validation = c("change_response", "blank_response", "remove_survey", "no_action")
    )
  }



  write_list[["readme"]] <- data.frame(
    change_type_validation = c("change_response", "blank_response", "remove_survey", "no_action"),
    description = c(
      "Change the response to new.value",
      "Remove and NA the response",
      "Delete the survey",
      "No action to take."
    )
  )

  workbook <- write_list |> create_formated_wb(
    column_for_color = column_for_color,
    header_front_size = header_front_size,
    header_front_color = header_front_color,
    header_fill_color = header_fill_color,
    header_front = header_front,
    body_front = body_front,
    body_front_size = body_front_size
  )


  hide_sheet <- which(names(workbook) == "validation_rules")

  openxlsx::sheetVisibility(workbook)[hide_sheet] <- F


  row_numbers <- 2:(nrow(write_list[[cleaning_log_name]]) + 1)
  col_number <- which(names(write_list[[cleaning_log_name]]) == change_type_col)


  if (!is.null(kobo_survey) & !is.null(kobo_choices) & use_dropdown == TRUE & exists("data.val", inherits = FALSE)) {
    cl <- write_list[[cleaning_log_name]]

    for (r in 1:nrow(cl)) {
      if (cl[r, "question"] %in% colnames(data.val) & as.character(cl[r, "uuid"]) != "all") {
        openxlsx::dataValidation(workbook,
          sheet = cleaning_log_name, cols = which(colnames(cl) == "new_value"),
          rows = r + 1, type = "list",
          value = create_col_range(as.character(cl[r, "question"]), data.val)
        ) %>%
          suppressWarnings()
      } else if ((stringr::str_detect(string = as.character(cl[r, "question"]), pattern = "\\.") |
        (stringr::str_detect(string = as.character(cl[r, "question"]), pattern = "\\/") &
          (stringr::str_detect(string = as.character(cl[r, "question"]), pattern = "/") == 1))) &
        as.character(cl[r, "uuid"]) != "all") {
        if (is.null(sm_dropdown_type) || stringr::str_to_lower(sm_dropdown_type) == "logical") {
          openxlsx::dataValidation(workbook,
            sheet = cleaning_log_name, cols = which(colnames(cl) == "new_value"),
            rows = r + 1, type = "list",
            value = create_col_range("binaries_sm_options_lgl", data.val)
          )
        } else {
          openxlsx::dataValidation(workbook,
            sheet = cleaning_log_name, cols = which(colnames(cl) == "new_value"),
            rows = r + 1, type = "list",
            value = create_col_range("binaries_sm_options_num", data.val)
          )
        }
      }
    }

    openxlsx::dataValidation(workbook,
      sheet = cleaning_log_name, cols = col_number,
      rows = row_numbers, type = "list",
      value = create_col_range("change_type_validation", data.val)
    ) %>%
      suppressWarnings()
  } else {
    openxlsx::dataValidation(workbook,
      sheet = cleaning_log_name, cols = col_number,
      rows = row_numbers, type = "list",
      value = "'validation_rules'!$A$2:$A$5"
    ) %>%
      suppressWarnings()
  }

  if (is.null(output_path)) {
    return(workbook)
  }

  if (!is.null(output_path)) {
    openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)
  }
}
