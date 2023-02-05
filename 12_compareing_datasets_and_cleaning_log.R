rm(list = ls())

library(tidyverse)
library(readxl)
library(illuminate)
options(scipen = 999)
# devtools::load_all()

# read raw data -----------------------------------------------------------

read_sheets("inst/extdata/MSNA_data_04 october_2022.xlsx",na_strings = c(999,"NA","Na","999"))
raw_data <- LBN_MSNA_2022
raw_data_indv <- hh_composition

# read clean data ---------------------------------------------------------
clean_data <- read.csv("outputs/02_clean_data/clean_data.csv",na.strings = c(999)) %>% fix_data_type()

# read cleaning log  ------------------------------------------------------


cleaning_log <- read_excel("data/cleaninglog/final_cleaning_log.xlsx",sheet = 1,guess_max = 500000)
cleaning_log$question.name <- cleaning_log$question.name %>% str_replace_all("/",".")

cleaning_log <- cleaning_log %>%
  mutate(
    change_type = case_when(is.na(rel.index) & change_type == "remove_survey" ~ "remove_survey_hh",
                            !is.na(rel.index) & change_type == "remove_survey" ~ "remove_survey_indv",
                            T~ change_type)
  ) %>%
  mutate(
    question_loop = case_when(question.name %in% names(raw_data) ~ "hh",
                              question.name %in% names(raw_data_indv) ~ "indv",
                              change_type == "remove_survey_hh" ~"hh",
                              change_type == "remove_survey_indv" ~"indv"))



deletion_log <- cleaning_log %>% filter(change_type == "remove_survey_hh")
cleaning_log <- cleaning_log %>% filter(!change_type %in% c("remove_survey_hh","remove_survey_indv","no_action")) %>%
  filter(question_loop == "hh")



#######################################################################################################33

### cleaing the environment
rm(list = ls()[!ls() %in% c("clean_data","raw_data","cleaning_log","deletion_log") ])

#recalling the functions
# source("R/generate_and_compare_cleaning_logs.R")
# source("R/review_cleaning_log.R")



############## Part 1:: This part will check the discrepancies in the cleaning log.
# In my experience, I have seen that Field teams sometimes mess up with the cleaning log and send the cleaning with the following issues-
# 1. Mismatch in question name: The cleaning log questions name can not be found in raw_data/clean_data/deletion log
# 2. Mismatch in deletion log: The number of surveys removed from raw data doesn't match the number of observations in deletion_log.
# 3. Mismatch in UUID: Cleaning log UUID doesn't match with raw data UUID.
# 4. Discrepancy in change value: The new value in the cleaned data and the new value in the cleaning log don't always match.
############## Hence the following functions were created to check the accuracy of cleaning/deletion_log


### check_deletion_log() can be used for checking the accuracy of deletion log.

check_dl_log <- check_deletion_log(raw_data = raw_data,clean_data = clean_data,deletion_log = deletion_log,
                                   raw_data_uuid = "X_id",clean_data_uuid = "X_id",deletion_log_uuid = "id")


### cleaning_log_check() can be used for checking the accuracy of cleaning log.

check_cleaning_log <- cleaning_log_check(raw_data = raw_data,
                                         clean_data = clean_data,
                                         deletion_log = deletion_log,
                                         raw_data_uuid = "X_id",
                                         clean_data_uuid = "X_id",
                                         deletion_log_uuid = "id",
                                         cleaning_log = cleaning_log,cleaning_log_uuid = "id",
                                         cleaning_log_old_value =  "old_value",cleaning_log_new_value = "new_value",
                                         cleaning_log_question_name = "question.name"
                                         )



### for checking both deletion and cleaning log:

check_cleaning_and_deletion_log<- check_deletion_and_cleaning_log(raw_data = raw_data,
                                         clean_data = clean_data,
                                         deletion_log = deletion_log,
                                         raw_data_uuid = "X_id",
                                         clean_data_uuid = "X_id",
                                         deletion_log_uuid = "id",
                                         cleaning_log = cleaning_log,
                                         cleaning_log_uuid = "id",
                                         cleaning_log_old_value =  "old_value",
                                         cleaning_log_new_value = "new_value",
                                         cleaning_log_question_name = "question.name"
)

check_cleaning_and_deletion_log$check_deletion ## for deletion log
check_cleaning_and_deletion_log$check_cleaning_log ## for cleaning log


### The above functions can be useful for field team mostly.


############## Part 2:: Creating cleaning log and and check with given (by the field team) cleaning log and flag if there is any missing.



## To generate the cleaning log
generating_cleaning_log <- generate_cleaning_log(raw_data = raw_data,raw_data_uuid = "X_id",
                                          check_for_deletion_log = F,
                                        clean_data = clean_data,clean_data_uuid = "X_id")



## To compare the datasets with the given cleaning log

missing_entry_in_cleaning_log <- comapre_cl_with_datasets(raw_data = raw_data,
                                  raw_data_uuid = "X_id" ,
                                  clean_data = clean_data,
                                  clean_data_uuid = "X_id",
                                  cleaning_log =cleaning_log ,
                                  cleaning_log_uuid = "id",
                                  cleaning_log_question_name = "question.name",
                                  cleaning_log_new_value = "new_value",
                                  cleaning_log_old_value = "old_value",
                                  deletion_log =deletion_log ,
                                  deletion_log_uuid = "id")


