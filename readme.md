
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
#> 1     a 0.51180325 0.0261426
#> 2     b 0.06155909 0.3344346
#> 3     c 0.85009177 0.5312682
#> 4     d 0.23079823 0.9663387
#> 5     a 0.67914406 0.3713654
#> 6     b 0.55266005 0.3072747
#> 7     c 0.22372945 0.6293074
check_duplicate(testdata)
#> $checked_dataset
#>   _uuid      col_a     col_b
#> 1     a 0.51180325 0.0261426
#> 2     b 0.06155909 0.3344346
#> 3     c 0.85009177 0.5312682
#> 4     d 0.23079823 0.9663387
#> 5     a 0.67914406 0.3713654
#> 6     b 0.55266005 0.3072747
#> 7     c 0.22372945 0.6293074
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
