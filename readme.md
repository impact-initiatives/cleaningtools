
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
#> 1     a 0.1404859 0.1195845
#> 2     b 0.8148428 0.4891062
#> 3     c 0.8135898 0.4397823
#> 4     d 0.3403768 0.2267128
#> 5     a 0.8096986 0.3841612
#> 6     b 0.2356780 0.5779422
#> 7     c 0.9435896 0.5115062
check_duplicate(testdata)
#> $checked_dataset
#>   _uuid     col_a     col_b
#> 1     a 0.1404859 0.1195845
#> 2     b 0.8148428 0.4891062
#> 3     c 0.8135898 0.4397823
#> 4     d 0.3403768 0.2267128
#> 5     a 0.8096986 0.3841612
#> 6     b 0.2356780 0.5779422
#> 7     c 0.9435896 0.5115062
#> 
#> $duplicate_log
#>   uuid old_value question            issue
#> 1    a         a    _uuid duplicated _uuid
#> 2    b         b    _uuid duplicated _uuid
#> 3    c         c    _uuid duplicated _uuid
```

Or you can check duplicate for a specific variable or combination of
variables.

``` r
testdata2 <- data.frame(
  uuid = letters[c(1:7)],
  village = paste("village", c(1:3,1:3,4)),
  ki_identifier = paste0("xx_", c(1:5,3,4))
  ) %>%
  dplyr::rename(`_uuid` = uuid)
check_duplicate(testdata2, .col_to_check = "village")
#> $checked_dataset
#>   _uuid   village ki_identifier
#> 1     a village 1          xx_1
#> 2     b village 2          xx_2
#> 3     c village 3          xx_3
#> 4     d village 1          xx_4
#> 5     e village 2          xx_5
#> 6     f village 3          xx_3
#> 7     g village 4          xx_4
#> 
#> $duplicate_log
#> # A tibble: 3 × 4
#>   uuid  question old_value issue             
#>   <chr> <chr>    <chr>     <glue>            
#> 1 d     village  village 1 duplicated village
#> 2 e     village  village 2 duplicated village
#> 3 f     village  village 3 duplicated village
check_duplicate(testdata2, .col_to_check = c("village", "ki_identifier"), uuid = "_uuid")
#> $checked_dataset
#>   _uuid   village ki_identifier
#> 1     a village 1          xx_1
#> 2     b village 2          xx_2
#> 3     c village 3          xx_3
#> 4     d village 1          xx_4
#> 5     e village 2          xx_5
#> 6     f village 3          xx_3
#> 7     g village 4          xx_4
#> 
#> $duplicate_log
#> # A tibble: 2 × 4
#>   uuid  question      old_value issue                               
#>   <chr> <chr>         <chr>     <glue>                              
#> 1 f     village       village 3 duplicated village ~/~ ki_identifier
#> 2 f     ki_identifier xx_3      duplicated village ~/~ ki_identifier
```

### Example:: Creating cleaning log from raw data and clean data

`create_cleaning_log` function takes raw data and clean data as inputs
and its identify any changes between them and finally provide the output
as cleaning log format.

``` r
cleaning_log <- create_cleaning_log(
  raw_data = raw_data, raw_data_uuid = "X_uuid",
  clean_data = clean_data, clean_data_uuid = "X_uuid",
  check_for_deletion_log = T, check_for_variable_name = T
)
cleaning_log
#>                                        uuid
#> ...1   dcf2753a-6ea2-40f5-b493-3527931ef96c
#> ...2   8790ce5c-1c35-41a2-b3c0-538f937d5397
#> ...3   bb818e04-9c40-408e-919f-6b40ff1fdbb3
#> ...4   28b90cbb-2cf0-41c5-9ee1-1c719c0d4c02
#> ...5   7f2a0c6a-529b-481f-963f-a96dca2ec034
#> ...6   b4f92064-12ea-4970-b0f5-fd309de1dda3
#> ...7   f5ce0576-bfe5-4d1b-a247-26d31c7e7a86
#> ...8   6c9e9356-5810-43d2-bf5c-a2044ecef219
#> ...9   0b74b52d-d221-4b81-a5e6-9f9db48b1f86
#> ...10  67e7482b-a543-40f5-856d-29d5b1c60f9a
#> ...11  a8d89047-792f-4d95-9adf-25e2e42ff66d
#> ...12  a52c231b-20c5-4520-bf73-d86410b0f26c
#> ...13  987dbeee-d809-4a62-b2ed-15a1673f4f18
#> ...14  8c7568cb-0815-4b9a-a19e-89b3db792441
#> ...15  248f7774-319a-4368-91d2-a1f8dec9972d
#> ...16  2777683b-aa86-40b7-8da1-77dcc2e4d553
#> ...17  f1175d29-ce1f-43a7-b3d1-ee26cd1b8cdb
#> ...18  3438f313-24e6-46fc-9597-cec440a737ce
#> ...19  e9f8b44c-c507-45a1-8d76-66d886437b8f
#> ...20  d7714f1e-0ed2-46ea-a94d-54a6c6423e39
#> ...21  2498556b-4165-4a85-a7d9-c6ce75032747
#> ...22  d75a0458-16bc-4b0c-af7a-5b22ab73dfb7
#> ...23  50be30c5-86b8-4fd1-bd59-be31d8bd128c
#> ...24  31742119-37a2-4a4c-a688-15efa7914aff
#> ...25  683e97a1-a6df-4442-b1ba-effe04612b86
#> ...26  3fbf82e7-8009-44b7-bb09-eabd9e38b18c
#> ...27  a25896b2-49e6-4187-972a-519101968647
#> ...28  994a60b8-e640-425c-9774-160651d7af04
#> ...29  d9ff616f-b3ac-4eae-88dc-601946260d82
#> ...30  c3a52fa9-63f5-4d02-b2b0-6eebbef944a7
#> ...31  17bad766-fc25-49fe-a041-c6503e05941d
#> ...32  308b7de7-7f09-4578-8373-8c81f74480f4
#> ...33  bd2a7c9f-4eed-4943-b39a-659d5c1d9a31
#> ...34  7f7a6714-a334-4079-bfe4-e590667f4aee
#> ...35  1a80239e-a815-4fcd-8549-294fb190c27f
#> ...36  bcd16469-8f6e-4d88-94c9-6fd700e352fa
#> ...37  bdcca634-6e9d-434a-bac0-5d17540848e8
#> ...38  6cb6e1dc-3d68-4d49-9d4a-29e3cb992f37
#> ...39  dd3a9983-0bcd-4fcd-933b-6c2dea5ea0ca
#> ...40  90e8ef05-9604-471e-8c1a-94fa5592cc46
#> ...41  f0fd8ae4-a149-45a7-9cfa-d51e30e7a55c
#> ...42  9dac8c9e-a5a5-435d-b28c-82d5f43b2f7f
#> ...43  a37512fa-f4ea-4c6c-a873-851f69df6c93
#> ...44  1b473a2c-71bd-4bcc-8040-621dde1475dc
#> ...45  f45d4ca7-adce-416d-b096-04ab6ff79d12
#> ...46  2b148a6e-90fc-496b-9d03-061377df2290
#> ...47  5205540c-6019-4137-9c18-fb597d5af35c
#> ...48  1f97f421-40a5-4c4d-9ec5-569d58718c5f
#> ...49  c928be0f-0f0e-4a56-8603-501f533f2fc6
#> ...50  bb7cb050-5b65-43c9-b7bb-2de4f56ecdda
#> ...51  cc73c878-71e9-44b1-9156-49c33d86731b
#> ...52  5c80617f-88b4-441d-8990-cdb3c33559e0
#> ...53  2b49e747-84e8-429a-95a0-8d25f6448aa0
#> ...54  c1c69337-eb3e-4427-a578-26f94bf3aeaf
#> ...55                                   all
#> ...56                                   all
#> 7      0349b616-aeb9-49e4-a3dc-e8e7ca9ac9cf
#> 10     03f9bc62-8a14-49eb-8b24-02992fc4dc31
#> 38     0d4fafd8-9243-4a05-be2a-2100c2e60185
#> 42     101e7fa9-eacd-40a8-bab0-4630488b8e1d
#> 43     11067dc4-45b5-4b35-8d43-5dec9f7276c5
#> 52     1475ab22-557c-4c93-ba46-68427f29375a
#> 78     1fcf8417-5cd9-4c65-9090-83e828881939
#> 84     222d5a7f-1962-4516-9127-6f5c2e574f96
#> 114    34cba4d1-371e-425b-a189-cb1cb3519ade
#> 216    5f5b64bc-c53b-40fb-98cd-a615205b5954
#> 250    6c7176cc-dfcf-495e-bc45-90c9e1be5b85
#> 260    6fb6b60c-70cf-4940-8f3d-603cf7d7c6c2
#> 269    7309b048-b6d5-4250-bf12-c7648dee943e
#> 287    7c71081a-1ba9-43fd-8ba7-eb46c43dc79f
#> 300    81ae2bfd-6c29-4846-b171-9512ec8eda41
#> 302    842b1321-7ab5-4494-b98b-29ae93fc654e
#> 309    87d64595-1a2f-4cd2-99b3-723134f45ff6
#> 349    9ea03a5e-aad9-46b3-99bc-7c0786d1abce
#> 355    a11f3cee-428d-4cbd-8c9b-41fd9a5d2697
#> 380    ab5f789b-a3a4-4ee6-a781-53b3366bbd62
#> 402    b427827c-949b-4524-a802-7432285b5ced
#> 410    b67bf5ec-a2b3-4c34-8bb3-25410ee92082
#> 433    c50e380e-64a4-4cee-84f2-daaebdbb78db
#> 436    c746e682-cb56-4604-95df-b200a98d3a8e
#> 446    c9b3a3df-4775-4ba3-bc1c-1b8237f714c4
#> 458    cfb721ab-e7b5-4b01-ac3a-c536c5fe8517
#> 471    d4142ede-2082-4091-accc-0989c34d6516
#> 495    dc499548-3308-4207-ac8f-3a14fc92baf0
#> 519    eb3b9935-fa0b-4d54-8058-3b629f1421ad
#> 569    fcc6b9e8-c72a-4228-8dc0-6e803114b6d6
#> 4581   cfb721ab-e7b5-4b01-ac3a-c536c5fe8517
#> 135    3dbe6df8-c2d7-449d-8082-c2b92738ce4d
#> 185    51324001-970c-4fb5-bacf-de696e1b7eaa
#> 477    d5d26992-388b-4c71-a461-18a63cac8738
#> 130    3b9732e2-a0d7-4dc3-962d-12efcd2ded15
#> 268    728e4de0-7356-4bb5-9db4-9479b3ffe098
#> 1301   3b9732e2-a0d7-4dc3-962d-12efcd2ded15
#> 2681   728e4de0-7356-4bb5-9db4-9479b3ffe098
#> 14     04e28a1d-fc2a-4384-b889-e56c498bc485
#> 45     1238747b-6616-4c8e-9a1f-571c2927d66d
#> 145    40d8ee6f-8e47-497a-9005-fde107b209b8
#> 205    5a2d6c59-3675-4da0-8af5-eeeda3142981
#> 275    745ce148-192b-42bf-9dc1-63d6860fed81
#> 324    90e04250-0e94-42a2-8a3d-676df3dd7edb
#> 462    d04074f2-cbbe-48be-9837-ccf3dc8e186e
#> 542    f28e9f8d-f9a4-4039-9ddf-2491518d2af3
#> 5691   fcc6b9e8-c72a-4228-8dc0-6e803114b6d6
#> 238    686846a0-5733-420f-99d5-b690233045d6
#> 351    9eeec67f-6bf7-4948-900e-2101b6346fc1
#> 139    3ef4cdde-bdce-4378-8e12-b7488ff68180
#> 470    d3a9b6cb-fb33-46f0-b140-308331a1fa7e
#> 4      03183d24-0275-43fe-8976-d076f29de590
#> ...109 407984dd-075f-49b9-b808-2772fc5399ec
#> ...110 489b4d13-7eaf-4732-af22-28d6cfd77466
#> ...111 d752c413-dce7-48aa-a2b2-3de90c1bb218
#>                                question_name     change_type   new_value
#> ...1                                    <NA>   remove_survey        <NA>
#> ...2                                    <NA>   remove_survey        <NA>
#> ...3                                    <NA>   remove_survey        <NA>
#> ...4                                    <NA>   remove_survey        <NA>
#> ...5                                    <NA>   remove_survey        <NA>
#> ...6                                    <NA>   remove_survey        <NA>
#> ...7                                    <NA>   remove_survey        <NA>
#> ...8                                    <NA>   remove_survey        <NA>
#> ...9                                    <NA>   remove_survey        <NA>
#> ...10                                   <NA>   remove_survey        <NA>
#> ...11                                   <NA>   remove_survey        <NA>
#> ...12                                   <NA>   remove_survey        <NA>
#> ...13                                   <NA>   remove_survey        <NA>
#> ...14                                   <NA>   remove_survey        <NA>
#> ...15                                   <NA>   remove_survey        <NA>
#> ...16                                   <NA>   remove_survey        <NA>
#> ...17                                   <NA>   remove_survey        <NA>
#> ...18                                   <NA>   remove_survey        <NA>
#> ...19                                   <NA>   remove_survey        <NA>
#> ...20                                   <NA>   remove_survey        <NA>
#> ...21                                   <NA>   remove_survey        <NA>
#> ...22                                   <NA>   remove_survey        <NA>
#> ...23                                   <NA>   remove_survey        <NA>
#> ...24                                   <NA>   remove_survey        <NA>
#> ...25                                   <NA>   remove_survey        <NA>
#> ...26                                   <NA>   remove_survey        <NA>
#> ...27                                   <NA>   remove_survey        <NA>
#> ...28                                   <NA>   remove_survey        <NA>
#> ...29                                   <NA>   remove_survey        <NA>
#> ...30                                   <NA>   remove_survey        <NA>
#> ...31                                   <NA>   remove_survey        <NA>
#> ...32                                   <NA>   remove_survey        <NA>
#> ...33                                   <NA>   remove_survey        <NA>
#> ...34                                   <NA>   remove_survey        <NA>
#> ...35                                   <NA>   remove_survey        <NA>
#> ...36                                   <NA>   remove_survey        <NA>
#> ...37                                   <NA>   remove_survey        <NA>
#> ...38                                   <NA>   remove_survey        <NA>
#> ...39                                   <NA>   remove_survey        <NA>
#> ...40                                   <NA>   remove_survey        <NA>
#> ...41                                   <NA>   remove_survey        <NA>
#> ...42                                   <NA>   remove_survey        <NA>
#> ...43                                   <NA>   remove_survey        <NA>
#> ...44                                   <NA>   remove_survey        <NA>
#> ...45                                   <NA>   remove_survey        <NA>
#> ...46                                   <NA>   remove_survey        <NA>
#> ...47                                   <NA>   remove_survey        <NA>
#> ...48                                   <NA>   remove_survey        <NA>
#> ...49                                   <NA>   remove_survey        <NA>
#> ...50                                   <NA>   remove_survey        <NA>
#> ...51                                   <NA>   remove_survey        <NA>
#> ...52                                   <NA>   remove_survey        <NA>
#> ...53                                   <NA>   remove_survey        <NA>
#> ...54                                   <NA>   remove_survey        <NA>
#> ...55                     socio_eco_division  variable_added        <NA>
#> ...56                           water_sector  variable_added        <NA>
#> 7                                return_date change_response  2017-01-01
#> 10                               return_date change_response  2018-07-01
#> 38                               return_date change_response  2018-03-01
#> 42                               return_date change_response  2017-01-01
#> 43                               return_date change_response  2018-03-01
#> 52                               return_date change_response  2018-03-01
#> 78                               return_date change_response  2019-01-20
#> 84                               return_date change_response  2019-07-15
#> 114                              return_date change_response  2018-03-01
#> 216                              return_date change_response  2018-04-19
#> 250                              return_date change_response  2019-05-01
#> 260                              return_date change_response  2017-12-10
#> 269                              return_date change_response  2018-08-01
#> 287                              return_date change_response  2018-07-12
#> 300                              return_date change_response  2018-04-06
#> 302                              return_date change_response  2018-01-26
#> 309                              return_date change_response  2021-03-01
#> 349                              return_date change_response  2018-01-01
#> 355                              return_date change_response  2018-10-01
#> 380                              return_date change_response  2018-01-01
#> 402                              return_date change_response  2017-12-21
#> 410                              return_date change_response  2018-10-18
#> 433                              return_date change_response  2018-11-11
#> 436                              return_date change_response  2018-02-17
#> 446                              return_date change_response  2018-06-09
#> 458                              return_date change_response  2018-02-01
#> 471                              return_date change_response  2018-01-01
#> 495                              return_date change_response  2020-03-01
#> 519                              return_date change_response  2018-02-27
#> 569                              return_date change_response  2018-02-01
#> 4581                          num_hh_hosting change_response           7
#> 135                             tot_expenses change_response      350000
#> 185                   water_sources.borehole change_response        TRUE
#> 477                   water_sources.borehole change_response        TRUE
#> 130                        treat_drink_water change_response never_treat
#> 268                        treat_drink_water change_response never_treat
#> 1301   treat_drink_water_how.expose_sunlight change_response       FALSE
#> 2681            treat_drink_water_how.filter change_response       FALSE
#> 14                                 spend_tap change_response       FALSE
#> 45                                 spend_tap change_response       FALSE
#> 145                                spend_tap change_response       FALSE
#> 205                                spend_tap change_response       FALSE
#> 275                                spend_tap change_response       FALSE
#> 324                                spend_tap change_response       FALSE
#> 462                                spend_tap change_response       FALSE
#> 542                                spend_tap change_response       FALSE
#> 5691                    water_tank_litres_nb change_response        7000
#> 238                             number_pumps change_response           1
#> 351                           air_coolers_nb change_response           2
#> 139                   connection_fees_amount change_response       45000
#> 470                   connection_fees_amount change_response       50000
#> 4                   pay_water_charges_amount change_response       10000
#> ...109                           return_date  blank_response        <NA>
#> ...110                           return_date  blank_response        <NA>
#> ...111                           return_date  blank_response        <NA>
#>           old_value                                 comment
#> ...1           <NA> No matching uuid in the cleaned dataset
#> ...2           <NA> No matching uuid in the cleaned dataset
#> ...3           <NA> No matching uuid in the cleaned dataset
#> ...4           <NA> No matching uuid in the cleaned dataset
#> ...5           <NA> No matching uuid in the cleaned dataset
#> ...6           <NA> No matching uuid in the cleaned dataset
#> ...7           <NA> No matching uuid in the cleaned dataset
#> ...8           <NA> No matching uuid in the cleaned dataset
#> ...9           <NA> No matching uuid in the cleaned dataset
#> ...10          <NA> No matching uuid in the cleaned dataset
#> ...11          <NA> No matching uuid in the cleaned dataset
#> ...12          <NA> No matching uuid in the cleaned dataset
#> ...13          <NA> No matching uuid in the cleaned dataset
#> ...14          <NA> No matching uuid in the cleaned dataset
#> ...15          <NA> No matching uuid in the cleaned dataset
#> ...16          <NA> No matching uuid in the cleaned dataset
#> ...17          <NA> No matching uuid in the cleaned dataset
#> ...18          <NA> No matching uuid in the cleaned dataset
#> ...19          <NA> No matching uuid in the cleaned dataset
#> ...20          <NA> No matching uuid in the cleaned dataset
#> ...21          <NA> No matching uuid in the cleaned dataset
#> ...22          <NA> No matching uuid in the cleaned dataset
#> ...23          <NA> No matching uuid in the cleaned dataset
#> ...24          <NA> No matching uuid in the cleaned dataset
#> ...25          <NA> No matching uuid in the cleaned dataset
#> ...26          <NA> No matching uuid in the cleaned dataset
#> ...27          <NA> No matching uuid in the cleaned dataset
#> ...28          <NA> No matching uuid in the cleaned dataset
#> ...29          <NA> No matching uuid in the cleaned dataset
#> ...30          <NA> No matching uuid in the cleaned dataset
#> ...31          <NA> No matching uuid in the cleaned dataset
#> ...32          <NA> No matching uuid in the cleaned dataset
#> ...33          <NA> No matching uuid in the cleaned dataset
#> ...34          <NA> No matching uuid in the cleaned dataset
#> ...35          <NA> No matching uuid in the cleaned dataset
#> ...36          <NA> No matching uuid in the cleaned dataset
#> ...37          <NA> No matching uuid in the cleaned dataset
#> ...38          <NA> No matching uuid in the cleaned dataset
#> ...39          <NA> No matching uuid in the cleaned dataset
#> ...40          <NA> No matching uuid in the cleaned dataset
#> ...41          <NA> No matching uuid in the cleaned dataset
#> ...42          <NA> No matching uuid in the cleaned dataset
#> ...43          <NA> No matching uuid in the cleaned dataset
#> ...44          <NA> No matching uuid in the cleaned dataset
#> ...45          <NA> No matching uuid in the cleaned dataset
#> ...46          <NA> No matching uuid in the cleaned dataset
#> ...47          <NA> No matching uuid in the cleaned dataset
#> ...48          <NA> No matching uuid in the cleaned dataset
#> ...49          <NA> No matching uuid in the cleaned dataset
#> ...50          <NA> No matching uuid in the cleaned dataset
#> ...51          <NA> No matching uuid in the cleaned dataset
#> ...52          <NA> No matching uuid in the cleaned dataset
#> ...53          <NA> No matching uuid in the cleaned dataset
#> ...54          <NA> No matching uuid in the cleaned dataset
#> ...55          <NA>     variable added to the clean dataset
#> ...56          <NA>     variable added to the clean dataset
#> 7        2021-07-10             An alteration was performed
#> 10       2021-07-14             An alteration was performed
#> 38       2021-07-11             An alteration was performed
#> 42       2021-07-09             An alteration was performed
#> 43       2021-07-14             An alteration was performed
#> 52       2021-07-08             An alteration was performed
#> 78       2021-07-13             An alteration was performed
#> 84       2021-07-14             An alteration was performed
#> 114      2021-07-13             An alteration was performed
#> 216      2021-07-07             An alteration was performed
#> 250      2021-07-06             An alteration was performed
#> 260      2021-07-08             An alteration was performed
#> 269      2021-07-12             An alteration was performed
#> 287      2021-07-07             An alteration was performed
#> 300      2021-07-07             An alteration was performed
#> 302      2021-07-06             An alteration was performed
#> 309      2021-07-12             An alteration was performed
#> 349      2021-07-14             An alteration was performed
#> 355      2021-07-11             An alteration was performed
#> 380      2021-07-11             An alteration was performed
#> 402      2021-07-14             An alteration was performed
#> 410      2021-07-06             An alteration was performed
#> 433      2021-07-08             An alteration was performed
#> 436      2021-07-07             An alteration was performed
#> 446      2021-07-06             An alteration was performed
#> 458      2021-07-09             An alteration was performed
#> 471      2021-07-06             An alteration was performed
#> 495      2021-07-14             An alteration was performed
#> 519      2021-07-08             An alteration was performed
#> 569      2021-07-06             An alteration was performed
#> 4581             10             An alteration was performed
#> 135           35000             An alteration was performed
#> 185           FALSE             An alteration was performed
#> 477           FALSE             An alteration was performed
#> 130    always_treat             An alteration was performed
#> 268    always_treat             An alteration was performed
#> 1301           TRUE             An alteration was performed
#> 2681           TRUE             An alteration was performed
#> 14                0             An alteration was performed
#> 45                0             An alteration was performed
#> 145               0             An alteration was performed
#> 205               0             An alteration was performed
#> 275               0             An alteration was performed
#> 324               0             An alteration was performed
#> 462               0             An alteration was performed
#> 542               0             An alteration was performed
#> 5691          70000             An alteration was performed
#> 238               7             An alteration was performed
#> 351              18             An alteration was performed
#> 139              45             An alteration was performed
#> 470              50             An alteration was performed
#> 4                10             An alteration was performed
#> ...109   2021-07-08                           changed to NA
#> ...110   2021-07-13                           changed to NA
#> ...111   2021-07-13                           changed to NA
```

### Example:: Comparing cleaning log with clean data and raw data

`review_cleaning_log` function takes raw data, clean data and cleaning
log as inputs, and it first creates the cleaning log by comparing raw
data and clean data, then compares it with the user-provided cleaning
log. Finally, flagged the discrepancies between them (if any).

``` r
compared_df <- review_cleaning_log(
  raw_data = raw_data, raw_data_uuid = "X_uuid",
  clean_data = clean_data, clean_data_uuid = "X_uuid",
  cleaning_log = cleaning_log2, cleaning_log_uuid = "X_uuid",
  cleaning_log_question_name = "questions",
  cleaning_log_new_value = "new_value", cleaning_log_old_value = "old_value",
  deletion_log = deletaion_log, deletion_log_uuid = "X_uuid",
  check_for_deletion_log = T
)

compared_df
#> # A tibble: 635 × 8
#>    uuid   df.question_name df.change_type df.new_value cl.new_value df.old_value
#>    <chr>  <chr>            <chr>          <chr>        <chr>        <chr>       
#>  1 1a802… air_coolers_nb   change_respon… <NA>         4            <NA>        
#>  2 a3751… air_coolers_nb   change_respon… <NA>         2            <NA>        
#>  3 bdcca… air_coolers_nb   no_action      <NA>         <NA>         <NA>        
#>  4 dd3a9… connection_fees… no_action      <NA>         <NA>         <NA>        
#>  5 9dac8… connection_fees… no_action      <NA>         <NA>         <NA>        
#>  6 2b148… days_available_… no_action      <NA>         <NA>         <NA>        
#>  7 bd2a7… hours_available… no_action      <NA>         <NA>         <NA>        
#>  8 6cb6e… inc_employment_… no_action      <NA>         <NA>         <NA>        
#>  9 52055… level_service_w… no_action      <NA>         <NA>         <NA>        
#> 10 7f7a6… pay_water_charg… no_action      <NA>         <NA>         <NA>        
#> # ℹ 625 more rows
#> # ℹ 2 more variables: cl.old_value <chr>, comment <chr>
```

### Example:: Check of PII

`check_for_pii()` function takes raw data (input can be dataframe or
list. However incase of list, you must specify the element name in
`element_name` parameter!) and looks for potential PII in the dataset.
By default, the function will look for following words but you can also
add additional words to look by using `words_to_look` parameter.The
default words
are-`c("telephone","contact","name","gps","neighbourhood","latitude","logitude","contact","nom","gps","voisinage")`.
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
#>   uuid old_value                         question
#> 1    b         6 duration_audit_start_end_minutes
#> 2    d         5 duration_audit_start_end_minutes
#> 3    e        14 duration_audit_start_end_minutes
#> 4    g       144 duration_audit_start_end_minutes
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
#>   uuid old_value                    question
#> 1    d    311585 duration_audit_start_end_ms
#> 2    g   8642007 duration_audit_start_end_ms
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
#>   uuid old_value                         question
#> 1    b         6 duration_audit_start_end_minutes
#> 2    d         5 duration_audit_start_end_minutes
#> 3    e        14 duration_audit_start_end_minutes
#> 4    g       144 duration_audit_start_end_minutes
#>                                             issue
#> 1 Duration is lower or higher than the thresholds
#> 2 Duration is lower or higher than the thresholds
#> 3 Duration is lower or higher than the thresholds
#> 4 Duration is lower or higher than the thresholds
#> 
#> $duration_in_ms
#>   uuid old_value                    question
#> 1    d    311585 duration_audit_start_end_ms
#> 2    g   8642007 duration_audit_start_end_ms
#>                                             issue
#> 1 Duration is lower or higher than the thresholds
#> 2 Duration is lower or higher than the thresholds
```

#### Example:: Check outliers

`check_outliers()` takes raw data set and look for potential outlines.
It can both data frame or list. However you must specify the element
name (name of your data set in the given list) in `element_name`
parameter!

``` r
set.seed(122)
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
#>  2 uuid_92  outlier (normal distribution) one_value         0
#>  3 uuid_93  outlier (normal distribution) one_value         0
#>  4 uuid_94  outlier (normal distribution) one_value         0
#>  5 uuid_95  outlier (normal distribution) one_value         0
#>  6 uuid_96  outlier (normal distribution) one_value       100
#>  7 uuid_97  outlier (normal distribution) one_value        99
#>  8 uuid_98  outlier (normal distribution) one_value       100
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
set.seed(122)

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

#### Example:: Recreate parent column for choice multiple

`recreate_parent_column()` recreates the concerted columns for select
multiple questions

``` r
test_data <- dplyr::tibble(
  uuid = paste0("uuid_",1:6),
  gender = rep(c("male","female"),3),
  reason = c("xx,yy","xx,zy",
             "zy","xx,xz,zy",
             NA_character_,"xz"),
  reason.x.x. = c(0,1,0,1,0,0),
  reason.yy = c(1,0,0,0,1,0),
  reason.x.z = c(0,0,0,1,0,1),
  reason.zy = c(0,1,1,1,0,0),
  reason_zy = c(NA_character_,"A","B","C",NA_character_,NA_character_))

recreate_parent_column(df = test_data,uuid = "uuid",sm_sep = ".")
#> Warning in recreate_parent_column(df = test_data, uuid = "uuid", sm_sep = "."):
#> Column(s) names are renamed as multiple separators are found in dataset column
#> names. Please see the above table with the new name.
#> # A tibble: 2 × 2
#>   old_name    new_name   
#>   <chr>       <chr>      
#> 1 reason.x.x. reason.x_x_
#> 2 reason.x.z  reason.x_z
#> # A tibble: 6 × 8
#>   uuid   gender reason      reason.x_x_ reason.yy reason.x_z reason.zy reason_zy
#>   <chr>  <chr>  <chr>             <dbl>     <dbl>      <dbl>     <dbl> <chr>    
#> 1 uuid_1 male   yy                    0         1          0         0 <NA>     
#> 2 uuid_2 female x_x_ zy               1         0          0         1 A        
#> 3 uuid_3 male   zy                    0         0          0         1 B        
#> 4 uuid_4 female x_x_ x_z zy           1         0          1         1 C        
#> 5 uuid_5 male   yy                    0         1          0         0 <NA>     
#> 6 uuid_6 female x_z                   0         0          1         0 <NA>
```

#### Example:: Recreate parent column for choice multiple

`review_sample_frame_with_dataset()` compares the sample frame with
dataset and provide the overview of completed and remaining surveys.

``` r
review_output<- review_sample_frame_with_dataset(sample_frame = cleaningtools::sample_frame ,
                                                 sample_frame_strata_col = "Neighbourhood",
                                                 sample_frame_target_survey_col ="Total.no.of.HH",
                                                 clean_data = cleaningtools::clean_data,
                                                 clean_data_strata_column = "neighbourhood",
                                                 consent_column = "consent_remote",
                                                 value_for_consent_yes = "yes")
review_output |> head()
#>   Managed.by Governorate Neighbourhood Total.no.of.HH Collected Remaining
#> 1   Talafar       Ninewa    al_askary1             22        22         0
#> 2   Talafar       Ninewa    al_askary2             19        19         0
#> 3   Talafar       Ninewa    al_askary3              5         5         0
#> 4   Talafar       Ninewa    al_askary4              6         6         0
#> 5   Talafar       Ninewa   al_jazeera1             12        12         0
#> 6   Talafar       Ninewa   al_jazeera2             15        15         0
```

#### Example:: Logical checks

``` r
test_data <- data.frame(uuid = c(1:10) %>% as.character(),
                        today = rep("2023-01-01", 10),
                        location = rep(c("villageA", "villageB"),5),
                        distance_to_market = c(rep("less_30", 5), rep("more_30",5)),
                        access_to_market = c(rep("yes",4), rep("no",6)),
                        number_children_05 = c(rep(c(0,1),4),5,6))
check_for_logical(test_data,
                  uuid_var = "uuid",
                  check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
                   variables_to_clean = "distance_to_market, access_to_market",
                   description = "distance to market less than 30 and no access")
#> $checked_dataset
#>    uuid      today location distance_to_market access_to_market
#> 1     1 2023-01-01 villageA            less_30              yes
#> 2     2 2023-01-01 villageB            less_30              yes
#> 3     3 2023-01-01 villageA            less_30              yes
#> 4     4 2023-01-01 villageB            less_30              yes
#> 5     5 2023-01-01 villageA            less_30               no
#> 6     6 2023-01-01 villageB            more_30               no
#> 7     7 2023-01-01 villageA            more_30               no
#> 8     8 2023-01-01 villageB            more_30               no
#> 9     9 2023-01-01 villageA            more_30               no
#> 10   10 2023-01-01 villageB            more_30               no
#>    number_children_05 logical_xx
#> 1                   0      FALSE
#> 2                   1      FALSE
#> 3                   0      FALSE
#> 4                   1      FALSE
#> 5                   0       TRUE
#> 6                   1      FALSE
#> 7                   0      FALSE
#> 8                   1      FALSE
#> 9                   5      FALSE
#> 10                  6      FALSE
#> 
#> $logical_xx
#> # A tibble: 2 × 6
#>   uuid  question           old_value issue                check_id check_binding
#>   <chr> <chr>              <chr>     <chr>                <chr>    <chr>        
#> 1 5     distance_to_market less_30   distance to market … logical… logical_xx ~…
#> 2 5     access_to_market   no        distance to market … logical… logical_xx ~…
```

``` r
test_data <- data.frame(uuid = c(1:10) %>% as.character(),
                       distance_to_market = rep(c("less_30","more_30"),5),
                       access_to_market = c(rep("yes",4), rep("no",6)),
                       number_children_05 = c(rep(c(0,1),4),5,6),
                       number_children_618 = c(rep(c(0,1),4),5,6))
check_list <- data.frame(name = c("logical_xx", "logical_yy", "logical_zz"),
                         check = c("distance_to_market == \"less_30\" & access_to_market == \"no\"",
                                   "number_children_05 > 3",
                                   "rowSums(across(starts_with(\"number\")), na.rm = T) > 9"),
                         description = c("distance to market less than 30 and no access",
                                         "number of children under 5 seems high",
                                         "number of children very high"),
                         variables_to_clean = c("distance_to_market, access_to_market",
                                                "number_children_05",
                                                ""))
check_for_logical_with_list(test_data,
                            uuid_var = "uuid",
                            list_of_check = check_list,
                            check_id_column = "name",
                            check_to_perform_column = "check",
                            variables_to_clean_column = "variables_to_clean",
                            description_column = "description")
#> Warning in check_for_logical(.dataset = .dataset, uuid_var = uuid_var,
#> variables_to_add = variables_to_add, : variables_to_clean not shared, results
#> may not be accurate
#> $checked_dataset
#>    uuid distance_to_market access_to_market number_children_05
#> 1     1            less_30              yes                  0
#> 2     2            more_30              yes                  1
#> 3     3            less_30              yes                  0
#> 4     4            more_30              yes                  1
#> 5     5            less_30               no                  0
#> 6     6            more_30               no                  1
#> 7     7            less_30               no                  0
#> 8     8            more_30               no                  1
#> 9     9            less_30               no                  5
#> 10   10            more_30               no                  6
#>    number_children_618 logical_xx logical_yy logical_zz
#> 1                    0      FALSE      FALSE      FALSE
#> 2                    1      FALSE      FALSE      FALSE
#> 3                    0      FALSE      FALSE      FALSE
#> 4                    1      FALSE      FALSE      FALSE
#> 5                    0       TRUE      FALSE      FALSE
#> 6                    1      FALSE      FALSE      FALSE
#> 7                    0       TRUE      FALSE      FALSE
#> 8                    1      FALSE      FALSE      FALSE
#> 9                    5       TRUE       TRUE       TRUE
#> 10                   6      FALSE       TRUE       TRUE
#> 
#> $logical_all
#> # A tibble: 10 × 6
#>    uuid  question           old_value               issue check_id check_binding
#>    <chr> <chr>              <chr>                   <chr> <chr>    <chr>        
#>  1 5     distance_to_market less_30                 dist… logical… logical_xx ~…
#>  2 5     access_to_market   no                      dist… logical… logical_xx ~…
#>  3 7     distance_to_market less_30                 dist… logical… logical_xx ~…
#>  4 7     access_to_market   no                      dist… logical… logical_xx ~…
#>  5 9     distance_to_market less_30                 dist… logical… logical_xx ~…
#>  6 9     access_to_market   no                      dist… logical… logical_xx ~…
#>  7 9     number_children_05 5                       numb… logical… logical_yy ~…
#>  8 10    number_children_05 6                       numb… logical… logical_yy ~…
#>  9 9     unable to identify please check this uuid… numb… logical… logical_zz ~…
#> 10 10    unable to identify please check this uuid… numb… logical… logical_zz ~…
```
