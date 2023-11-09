# remotes::install_github("thinkr-open/checkhelper")
# checkhelper::print_globals()

globalVariables(unique(c(
  # add_duration_from_audit:
  ".",
  # add_info_to_cleaning_log:
  ".",
  # add_percentage_missing:
  ".", "name", "type",
  # auto_detect_sm_parents:
  "col_names", "n",
  # check_duplicate:
  "duplicate_check", "unique_combination",
  # check_duration:
  "duration_check",
  # check_logical:
  "uuid",
  # check_others:
  "old_value", "uuid",
  # check_outliers:
  "issue", "name", "old_value", "question", "type", "uuid",
  # check_pii:
  "question", "snkae_case_cols",
  # check_soft_duplicates:
  "column", "issue", "name", "number_different_columns", "type", "uuid",
  # check_value:
  ".", "name", "uuid", "value",
  # create_audit_list:
  "Name", "path", "uuid",
  # create_audit_list : <anonymous>:
  "event",
  # create_clean_data:
  "change_type_created_f",
  # create_cleaning_log:
  ".",
  # create_combined_log:
  ".", "question", "uuid",
  # create_duration_from_audit_sum_all:
  "node",
  # create_duration_from_audit_with_start_end:
  "end", "start",
  # create_formatted_choices:
  "choices.label", "list_name", "name", "q.type", "type",
  # create_logic_for_other:
  ".", "description", "id", "logic", "name", "other_choice", "parent", "relevant", "type", "variables_to_clean_column",
  # create_validation_list:
  ".", "name",
  # create_xlsx_cleaning_log:
  "type",
  # detect_variable:
  ".",
  # recreate_parent_column:
  "cols", "list_name", "name", "new_name", "sm_child", "sm_parent", "type", "value",
  # review_cleaning:
  "cleaning_log_only", "df.new_value", "df.uuid", "question", "uniqe_row_id", "uuid",
  # review_cleaning_log:
  "cl_prob",
  # review_others:
  ".",
  # review_sample_frame_with_dataset:
  "Collected",
  # add_ridl:
  "cleanned_data", "date_range_end", "date_range_start", "file_type"
)))
