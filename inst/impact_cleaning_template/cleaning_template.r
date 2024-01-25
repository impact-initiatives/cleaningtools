library(cleaningtools)
library(tidyverse)
source("utils/example_function.R")

# reading files
raw_dataset <- read_csv("inputs/cleaningtools_raw_data.csv")
kobo_survey <- read_csv("inputs/kobotool/kobo_survey.csv")
kobo_choices <- read_csv("inputs/kobotool/kobo_choices.csv")

# Performing Checks
checks_logs <- raw_dataset |>
  check_duplicate(uuid_column = "X_uuid") |>
  check_soft_duplicates(
    kobo_survey = kobo_survey,
    uuid_column = "X_uuid",
    idnk_value = "dont_know",
    sm_separator = "."
  ) |>
  check_pii(uuid_column = "X_uuid") |>
  check_outliers(uuid_column = "X_uuid") |>
  check_value(uuid_column = "X_uuid")

# Extra checks with arguments or new columns

## Adding columns
### Duration

#### Duration from audits

# audits_list <- create_audit_list(audit_zip_path = "path/to/zip/audit")
# checks_logs$checked_dataset <- checks_logs$checked_dataset |>
#   add_duration_from_audit(audit_list = audits_list)

#### Duration from start and end columns with add_duration
# checks_logs$checked_dataset <- checks_logs$checked_dataset |>
#   add_duration(uuid_column  = "X_uuid",
#                start_column = "start",
#                end_column = "end")

#### Duration from start and end columns without add_duration
checks_logs$checked_dataset$duration <- difftime(as.POSIXct(checks_logs$checked_dataset$end),
                                                 as.POSIXct(checks_logs$checked_dataset$`X.U.FEFF.start`),
                                                 units = "mins"
)

### Percentage missing
checks_logs$checked_dataset <- checks_logs$checked_dataset |>
  add_percentage_missing(kobo_survey = kobo_survey)

## Helpers for checks functions

### Others
text_oth <- kobo_survey |>
  dplyr::filter(type == "text", name %in% names(raw_dataset)) |>
  dplyr::pull(name)

### Logical checks
logical_list <- read_csv("inputs/check_list.csv")


## Addtional checks

checks_logs <- checks_logs |>
  check_duration(uuid_column = "X_uuid", column_to_check = "duration") |>
  check_percentage_missing(uuid_column = "X_uuid") |>
  check_others(uuid_column = "X_uuid", columns_to_check = text_oth) |>
  check_logical_with_list(
    uuid_column = "X_uuid",
    list_of_check = logical_list,
    check_id_column = "check_id",
    check_to_perform_column = "check_to_perform",
    columns_to_clean_column = "columns_to_clean",
    description_column = "description"
  )

# Creating the cleaning log

cleaning_log <- checks_logs |>
  create_combined_log() |>
  add_info_to_cleaning_log(
    dataset_uuid_column = "X_uuid",
    information_to_add = c("enumerator_num", "date_assessment")
  )

cleaning_log |>
  create_xlsx_cleaning_log(
    use_dropdown = TRUE,
    sm_dropdown_type = "logical",
    kobo_survey = kobo_survey,
    kobo_choices = kobo_choices,
    output_path = "outputs/cleaning_log.xlsx"
  )

# Reading the filled cleaning log
filled_cleaning_log <- read_csv("outputs/cleaning_log_filled.csv")

# Creating the clean dataset
review_cleaning_log(
  raw_dataset = raw_dataset, raw_data_uuid_column = "X_uuid",
  cleaning_log = filled_cleaning_log,
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_change_type_column = "change_type"
)

clean_data <- create_clean_data(
  raw_dataset = raw_dataset,
  raw_data_uuid_column = "X_uuid",
  cleaning_log = filled_cleaning_log,
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_change_type_column = "change_type"
)


# Correcting select multiple parents

clean_data <- recreate_parent_column(clean_data,
                                     uuid_column = "X_uuid",
                                     kobo_survey = kobo_survey,
                                     kobo_choices = kobo_choices,
                                     sm_separator = ".",
                                     cleaning_log_to_append = filled_cleaning_log
)

# Review of the others
others_review <- review_others(clean_data$data_with_fix_concat,
                               uuid_column = "X_uuid",
                               kobo_survey = kobo_survey,
                               sm_separator = "."
)

# Review of the cleaning
only_deletion <- clean_data$cleaning_log %>%
  filter(change_type == "remove_survey")
only_cleaning <- clean_data$cleaning_log %>%
  filter(
    change_type != "remove_survey",
    !uuid %in% only_deletion$uuid
  )


cleaning_review <- review_cleaning(
  raw_dataset = raw_dataset,
  raw_dataset_uuid_column = "X_uuid",
  clean_dataset = clean_data$data_with_fix_concat,
  clean_dataset_uuid_column = "X_uuid",
  cleaning_log = only_cleaning,
  cleaning_log_change_type_column = "change_type",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_old_value_column = "old_value",
  cleaning_log_no_change_value = "no_action",
  deletion_log = only_deletion,
  deletion_log_uuid_column = "uuid"
)

# Review sampling frame
sampling_frame <- read_csv("inputs/sampling_frame.csv")
sampling_frame_review <- review_sample_frame_with_dataset(sample_frame = sampling_frame,
                                                          sampling_frame_strata_column = "Neighbourhood",
                                                          sampling_frame_target_survey_column = "Total.no.of.HH",
                                                          clean_dataset = clean_data$data_with_fix_concat,
                                                          clean_dataset_strata_column = "neighbourhood",
                                                          consent_column = "consent_remote",
                                                          consent_yes_value = "yes"
)

# Write clean dataset
clean_info_list <- list(dataset = clean_data$data_with_fix_concat,
                   cleaning_log = only_cleaning,
                   deletion_log = only_deletion)
clean_info_list %>%
  writexl::write_xlsx("outputs/clean data.xlsx")
