
## cleaningtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/impact-initiatives/cleaningtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/impact-initiatives/cleaningtools/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/impact-initiatives/cleaningtools/branch/master/graph/badge.svg?token=SOH3NGXQDU)](https://codecov.io/gh/impact-initiatives/cleaningtools)

<!-- badges: end -->

## Overview

The `cleaningtools` package focuses on cleaning, and has three
components:
<p>

**1. Check**, which includes a set of functions that flag values, such
as check_outliers and check_logical. <br> **2. Create**, which includes
a set of functions to create different items for use in cleaning, such
as the cleaning log from the checks, clean data, and enumerator
performance. <br> **3. Review**, which includes a set of functions to
review the cleaning, such as reviewing the cleaning.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("impact-initiatives/cleaningtools")
```

## Examples

### Example:: Check for duplicates

``` r
library(cleaningtools)
testdata <- data.frame(uuid = c(letters[1:4], "a", "b", "c"),
                       col_a = runif(7),
                       col_b = runif(7)) %>%
 dplyr::rename(`_uuid` = uuid)
testdata
#>   _uuid      col_a     col_b
#> 1     a 0.63250723 0.2358740
#> 2     b 0.13943250 0.2704222
#> 3     c 0.64100899 0.1669847
#> 4     d 0.01949786 0.3981684
#> 5     a 0.16612703 0.2340511
#> 6     b 0.98727167 0.1892251
#> 7     c 0.81351810 0.7959703
check_duplicate(testdata)
#> $checked_dataset
#>   _uuid      col_a     col_b
#> 1     a 0.63250723 0.2358740
#> 2     b 0.13943250 0.2704222
#> 3     c 0.64100899 0.1669847
#> 4     d 0.01949786 0.3981684
#> 5     a 0.16612703 0.2340511
#> 6     b 0.98727167 0.1892251
#> 7     c 0.81351810 0.7959703
#> 
#> $duplicate_log
#>   uuid value variable           issue
#> 1    a     a    _uuid duplicated uuid
#> 2    b     b    _uuid duplicated uuid
#> 3    c     c    _uuid duplicated uuid
```

### Example:: Creating cleaning log from raw data and clean data

`create_cleaning_log` function takes raw data and clean data as inputs
and its identify any changes bwtween them and finally provide the output
as claning log format.

``` r
cleaning_log <- create_cleaning_log(
  raw_data = raw_data, raw_data_uuid = "X_uuid",
  clean_data = clean_data, clean_data_uuid = "X_uuid",
  check_for_deletion_log = T, check_for_variable_name = T
)
```

### Example:: Comparing cleaning log with clean data and raw data

`compare_cl_with_datasets` function takes raw data, clean data and
cleaning log as inputs, and it first creates the cleaning log by
comparing raw data and clean data, then compares it with the
user-provided cleaning log. Finally, flagged the discrepancies between
them (if any).

``` r
compare_cl_with_datasets(
  raw_data = raw_data, raw_data_uuid = "X_uuid",
  clean_data = clean_data, clean_data_uuid = "X_uuid",
  cleaning_log = cleaning_log2, cleaning_log_uuid = "X_uuid",
  cleaning_log_question_name = "questions",
  cleaning_log_new_value = "new_value", cleaning_log_old_value = "old_value",
  deletion_log = deletaion_log, deletion_log_uuid = "X_uuid",
  check_for_deletion_log = T, check_for_variable_name = T
)
```

### Example:: Check of PII

`check_for_pii()` function takes raw data (input can be dataframe or
list. However incase of list, you must specify the element name in
`element_name` parameter!) and looks for potential PII in the dataset.
By default, the function will look for following words but you can also
add additional words to look by using `words_to_look`
parameter.`c("telephone","contact","name","gps","neighbourhood","latitude","logitude","contact","nom","gps","voisinage")`.
The function will give a list with two element. One will be the data and
second one will be the list of potential PII

- Using dataframe as input

``` r
output_from_data <- check_for_pii(df = raw_data,words_to_look = "date")
output_from_data$potential_PII
#> # A tibble: 7 × 3
#>   uuid  question                              issue        
#>   <chr> <chr>                                 <chr>        
#> 1 all   date_assessment                       Potential PII
#> 2 all   neighbourhood                         Potential PII
#> 3 all   return_date                           Potential PII
#> 4 all   water_supply_rest_neighbourhood       Potential PII
#> 5 all   water_supply_other_neighbourhoods     Potential PII
#> 6 all   water_supply_other_neighbourhoods_why Potential PII
#> 7 all   consent_telephone_number              Potential PII
```

- Using list as input

``` r
### from list
df_list <- list(raw_data=raw_data)
output_from_list <- check_for_pii(df = df_list,element_name = "raw_data",words_to_look = "date")
output_from_list$potential_PII
#> # A tibble: 7 × 3
#>   uuid  question                              issue        
#>   <chr> <chr>                                 <chr>        
#> 1 all   date_assessment                       Potential PII
#> 2 all   neighbourhood                         Potential PII
#> 3 all   return_date                           Potential PII
#> 4 all   water_supply_rest_neighbourhood       Potential PII
#> 5 all   water_supply_other_neighbourhoods     Potential PII
#> 6 all   water_supply_other_neighbourhoods_why Potential PII
#> 7 all   consent_telephone_number              Potential PII
```

### Example:: Check of duration from audits

#### Reading the audits files

It will read only the compressed file.

``` r
my_audit_list <- create_audit_list(audit_zip_path = "audit_for_tests_100.zip")
```

#### Adding the duration to the dataset

Once you have read your audit file from the zip, you will get a list of
audit. You can use this list to calculate and add the duration. You have
2 options with a start and end question or summing all the durations.

``` r
list_audit <- list(uuid1 = data.frame(event = c("form start", rep("question", 5)),
                                      node = c("", paste0("/xx/question", 1:5)),
                                      start = c(1661415887295, 1661415887301,
                                                1661415890819, 1661415892297,
                                                1661415893529, 1661415894720),
                                      end = c(NA, 1661415890790, 1661415892273,
                                              1661415893506, 1661415894703,
                                              1661415896452)),
                   uuid2 = data.frame(event = c("form start", rep("question", 5)),
                                      node = c("", paste0("/xx/question", 1:5)),
                                      start = c(1661415887295, 1661415887301, 1661415890819, 1661415892297, 1661415893529, 1661415894720),
                                      end = c(NA, 1661415890790, 1661415892273, 1661415893506, 1661415894703, 1661415896452)))
some_dataset <- data.frame(X_uuid = c("uuid1", "uuid2"),
                          question1 = c("a","b"),
                          question2 = c("a","b"),
                          question3 = c("a","b"),
                          question4 = c("a","b"),
                          question5 = c("a","b"))
```

If you want to sum all the duration.

``` r
add_duration_from_audit(some_dataset, uuid_var = "X_uuid", audit_list = list_audit)
#>   X_uuid question1 question2 question3 question4 question5
#> 1  uuid1         a         a         a         a         a
#> 2  uuid2         b         b         b         b         b
#>   duration_audit_sum_all_ms duration_audit_sum_all_minutes
#> 1                      9058                            0.2
#> 2                      9058                            0.2
```

If you want to use calculate duration between 2 questions.

``` r
add_duration_from_audit(some_dataset, uuid_var = "X_uuid", audit_list = list_audit,
                         start_question = "question1",
                         end_question = "question3",
                         sum_all = F)
#>   X_uuid question1 question2 question3 question4 question5
#> 1  uuid1         a         a         a         a         a
#> 2  uuid2         b         b         b         b         b
#>   duration_audit_start_end_ms duration_audit_start_end_minutes
#> 1                        6205                              0.1
#> 2                        6205                              0.1
```

If you want to do both.

``` r
add_duration_from_audit(some_dataset, uuid_var = "X_uuid", audit_list = list_audit,
                         start_question = "question1",
                         end_question = "question3",
                         sum_all = T)
#>   X_uuid question1 question2 question3 question4 question5
#> 1  uuid1         a         a         a         a         a
#> 2  uuid2         b         b         b         b         b
#>   duration_audit_sum_all_ms duration_audit_sum_all_minutes
#> 1                      9058                            0.2
#> 2                      9058                            0.2
#>   duration_audit_start_end_ms duration_audit_start_end_minutes
#> 1                        6205                              0.1
#> 2                        6205                              0.1
```

#### checking the duration of the dataset

Once you have added the duration to the dataset, you can check if
duration are between the threshold you are looking for.

``` r
testdata <- data.frame(
  uuid = c(letters[1:7]),
  duration_audit_start_end_ms = c(2475353, 375491, 2654267, 311585, 817270,
                                  2789505, 8642007),
  duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
) %>%
  dplyr::rename(`_uuid` = uuid)
check_duration(testdata,
               .col_to_check = "duration_audit_start_end_minutes")
#> $checked_dataset
#>   _uuid duration_audit_start_end_ms duration_audit_start_end_minutes
#> 1     a                     2475353                               41
#> 2     b                      375491                                6
#> 3     c                     2654267                               44
#> 4     d                      311585                                5
#> 5     e                      817270                               14
#> 6     f                     2789505                               46
#> 7     g                     8642007                              144
#> 
#> $duration_log
#>   uuid value                         variable
#> 1    b     6 duration_audit_start_end_minutes
#> 2    d     5 duration_audit_start_end_minutes
#> 3    e    14 duration_audit_start_end_minutes
#> 4    g   144 duration_audit_start_end_minutes
#>                                             issue
#> 1 Duration is lower or higher than the thresholds
#> 2 Duration is lower or higher than the thresholds
#> 3 Duration is lower or higher than the thresholds
#> 4 Duration is lower or higher than the thresholds
check_duration(
  testdata,
  .col_to_check = "duration_audit_start_end_ms",
  lower_bound = 375490,
  higher_bound = 8642000
)
#> $checked_dataset
#>   _uuid duration_audit_start_end_ms duration_audit_start_end_minutes
#> 1     a                     2475353                               41
#> 2     b                      375491                                6
#> 3     c                     2654267                               44
#> 4     d                      311585                                5
#> 5     e                      817270                               14
#> 6     f                     2789505                               46
#> 7     g                     8642007                              144
#> 
#> $duration_log
#>   uuid   value                    variable
#> 1    d  311585 duration_audit_start_end_ms
#> 2    g 8642007 duration_audit_start_end_ms
#>                                             issue
#> 1 Duration is lower or higher than the thresholds
#> 2 Duration is lower or higher than the thresholds
testdata %>% check_duration(.col_to_check = "duration_audit_start_end_minutes") %>%
  check_duration(
    .col_to_check = "duration_audit_start_end_ms",
    name_log = "duration_in_ms",
    lower_bound = 375490,
    higher_bound = 8642000
  )
#> $checked_dataset
#>   _uuid duration_audit_start_end_ms duration_audit_start_end_minutes
#> 1     a                     2475353                               41
#> 2     b                      375491                                6
#> 3     c                     2654267                               44
#> 4     d                      311585                                5
#> 5     e                      817270                               14
#> 6     f                     2789505                               46
#> 7     g                     8642007                              144
#> 
#> $duration_log
#>   uuid value                         variable
#> 1    b     6 duration_audit_start_end_minutes
#> 2    d     5 duration_audit_start_end_minutes
#> 3    e    14 duration_audit_start_end_minutes
#> 4    g   144 duration_audit_start_end_minutes
#>                                             issue
#> 1 Duration is lower or higher than the thresholds
#> 2 Duration is lower or higher than the thresholds
#> 3 Duration is lower or higher than the thresholds
#> 4 Duration is lower or higher than the thresholds
#> 
#> $duration_in_ms
#>   uuid   value                    variable
#> 1    d  311585 duration_audit_start_end_ms
#> 2    g 8642007 duration_audit_start_end_ms
#>                                             issue
#> 1 Duration is lower or higher than the thresholds
#> 2 Duration is lower or higher than the thresholds
```
