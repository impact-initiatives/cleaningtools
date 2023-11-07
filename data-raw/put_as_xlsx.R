raww <- cleaningtools::cleaningtools_raw_data

writexl::write_xlsx(raww, here::here("data-raw","data.xlsx"))
raww <- readxl::read_excel(here::here(params$datafolder ,params$rawwl))

#rawwl: "data.xlsx" ## Name of the  raw data file
cleannl: "clean_data.xlsx"
loggl: "cleaning_log.xlsx"
logical_check_list: "logical_check_list.xlsx"
sampling_frame: "sample_frame.xlsx"

sampling_frame <- cleaningtools::cleaningtools_sample_frame
writexl::write_xlsx(sampling_frame, here::here("data-raw","sampling_frame.xlsx"))

cleann <- cleaningtools::cleaningtools_clean_data
writexl::write_xlsx(cleann, here::here("data-raw","clean_data.xlsx"))

logg <- cleaningtools::cleaningtools_cleaning_log
writexl::write_xlsx(logg, here::here("data-raw","cleaning_log.xlsx"))



logical_check_list <- data.frame(
  check_id = c("check_exp", "check_2"),
  description = c("primary_livelihood is rented but expenses less than 500000",
                  "acces water and tank emptied"),
  check_to_perform = c("primary_livelihood == \"employment\" & tot_expenses < 500000",
                       "access_water_enough == \"totally_insufficient\" & tank_emptied == \"about_half\""),
  columns_to_clean = c("primary_livelihood, tot_expenses",
                       "access_water_enough, tank_emptied"
  ))

writexl::write_xlsx(logical_check_list, here::here("data-raw","logical_check_list.xlsx"))

questions <- cleaningtools::cleaningtools_survey
choices <- cleaningtools::cleaningtools_choices


writexl::write_xlsx(list(survey = questions,
                         choices = choices),
                    here::here("data-raw","form.xlsx"))


