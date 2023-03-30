
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
#>   _uuid     col_a     col_b
#> 1     a 0.4538906 0.7103299
#> 2     b 0.9191159 0.7513698
#> 3     c 0.1192595 0.3035998
#> 4     d 0.4505862 0.1720152
#> 5     a 0.7491040 0.6199332
#> 6     b 0.8666592 0.5408469
#> 7     c 0.7987114 0.1625316
check_duplicate(testdata)
#> $checked_dataset
#>   _uuid     col_a     col_b
#> 1     a 0.4538906 0.7103299
#> 2     b 0.9191159 0.7513698
#> 3     c 0.1192595 0.3035998
#> 4     d 0.4505862 0.1720152
#> 5     a 0.7491040 0.6199332
#> 6     b 0.8666592 0.5408469
#> 7     c 0.7987114 0.1625316
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

#### Example:: Check of PII

`check_for_pii()` function takes raw data (input can be data frame or
list. However in case of list, you must specify the element name in
`element_name` parameter!) and looks for potential PII in the data set.
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

#### Example:: Check outliers

`check_outliers()` takes raw data set and look for potential outlines.
It can both data frame or list. However you must specify the element
name (name of your data set in the given list) in `element_name`
parameter!

``` r

### from list
df_outlier<- data.frame(
  uuid = paste0("uuid_", 1:100),
  one_value = c(round(runif(90, min = 45,max =55)), round(runif(5)), round(runif(5,99,100))),
  expense = c(sample(200:500,replace = T,size = 95),c(600,100,80,1020,1050)),
  income = c(c(60,0,80,1020,1050),sample(20000:50000,replace = T,size = 95)),
  yy = c(rep(100,99),10)
)
outliers <- check_outliers(df = df_outlier,uuid_col_name = "uuid")
#> [1] "checking_one_value"
#> [1] "checking_expense"
#> [1] "checking_income"
#> [1] "checking_yy"
outliers$potential_outliers
#> # A tibble: 18 × 4
#>    uuid     issue                         question  old_value
#>    <chr>    <chr>                         <chr>         <dbl>
#>  1 uuid_91  outlier (normal distribution) one_value         1
#>  2 uuid_92  outlier (normal distribution) one_value         1
#>  3 uuid_93  outlier (normal distribution) one_value         1
#>  4 uuid_94  outlier (normal distribution) one_value         1
#>  5 uuid_95  outlier (normal distribution) one_value         1
#>  6 uuid_96  outlier (normal distribution) one_value        99
#>  7 uuid_97  outlier (normal distribution) one_value        99
#>  8 uuid_98  outlier (normal distribution) one_value        99
#>  9 uuid_99  outlier (normal distribution) one_value       100
#> 10 uuid_100 outlier (normal distribution) one_value        99
#> 11 uuid_99  outlier (normal distribution) expense        1020
#> 12 uuid_100 outlier (normal distribution) expense        1050
#> 13 uuid_97  outlier (log distribution)    expense         100
#> 14 uuid_98  outlier (log distribution)    expense          80
#> 15 uuid_1   outlier (log distribution)    income           60
#> 16 uuid_2   outlier (log distribution)    income            0
#> 17 uuid_3   outlier (log distribution)    income           80
#> 18 uuid_100 outlier (normal distribution) yy               10
```

#### Example:: Create clean data

`create_clean_data()` function applies cleaning log to the raw data set
and returns a clean data set.

``` r
cleaning_log_test <- data.frame(
  uuid = paste0("uuid",1:4),
  question= c("age","gender","pop_group","strata"),
  change_type = c("blank_response","no_change","Delete","change_res"),
  new_value = c(NA_character_,NA_character_,NA_character_,"st-a")
)
test_data <- data.frame(
  uuid =  paste0("uuid",1:4),
  age = c(180,23,45,67),
  gender = c("male","female","male","female"),
  pop_group = c("idp","refugee","host","idp"),
  strata = c("a","b","c","d")
)

clean_dataset <- create_clean_data(df = test_data,df_uuid = "uuid",cl = cleaning_log_test,
                                   cl_change_type_col =  "change_type",
                                   values_for_change_response = "change_res",
                                   values_for_blank_response = "blank_response",
                                   values_for_no_change = "no_change",
                                   values_for_remove_survey = "Delete",
                                   cl_change_col =  "question",
                                   cl_uuid = "uuid",
                                   cl_new_val = "new_value" )
#> [1] "age"
#> [1] "strata"
```

#### Example:: Check for value

`check_for_value()` function look for specified value in the given data
set and return in a cleaning log format. The function can take a data
frame or a list as input.

``` r
df <- data.frame(
    X_uuid = paste0("uuid_",1:100),
    age = c(sample(18:80,replace = T,size = 96),99,99,98,88),
    gender = c("99",sample(c("male","female"),replace = T,size = 95),"98","98","88","888"))

output <- check_for_value(df = df,uuid_col_name = "X_uuid",element_name = "checked_dataset",values_to_look = c(99,98,88,888))

output$flaged_value
#> # A tibble: 9 × 3
#>   uuid     question old_value
#>   <chr>    <chr>    <chr>    
#> 1 uuid_1   gender   99       
#> 2 uuid_97  age      99       
#> 3 uuid_97  gender   98       
#> 4 uuid_98  age      99       
#> 5 uuid_98  gender   98       
#> 6 uuid_99  age      98       
#> 7 uuid_99  gender   88       
#> 8 uuid_100 age      88       
#> 9 uuid_100 gender   888
```
