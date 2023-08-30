test_that("Works with one test", {
  test_data <- data.frame(
    uuid = c(1:10) %>% as.character(),
    today = rep("2023-01-01", 10),
    location = rep(c("villageA", "villageB"), 5),
    distance_to_market = c(rep("less_30", 5), rep("more_30", 5)),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5))),
    logical_xx = dplyr::tibble(
      uuid = rep("5", 2),
      question = c("distance_to_market", "access_to_market"),
      old_value = c("less_30", "no"),
      issue = rep("distance to market less than 30 and no access", 2),
      check_id = rep("logical_xx", 2)
    ) %>%
      dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))
  )

  expect_equal(
    check_logical(test_data,
      uuid_column = "uuid",
      check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      columns_to_clean = "distance_to_market, access_to_market",
      description = "distance to market less than 30 and no access"
    ),
    expected_results
  )

  # with extra columns
  expected_results2 <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5))),
    logical_xx = dplyr::tibble(
      uuid = rep("5", 2),
      today = rep("2023-01-01", 2),
      location = rep("villageA", 2),
      question = c("distance_to_market", "access_to_market"),
      old_value = c("less_30", "no"),
      issue = rep("distance to market less than 30 and no access", 2),
      check_id = rep("logical_xx", 2)
    ) %>%
      dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))
  )

  expect_equal(
    check_logical(test_data,
      uuid_column = "uuid",
      check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      columns_to_clean = c("distance_to_market", "access_to_market"),
      information_to_add = c("today", "location"),
      description = "distance to market less than 30 and no access"
    ),
    expected_results2
  )
})

test_that("Works with list of tests", {
  # unbinded
  test_data <- data.frame(
    uuid = c(1:10) %>% as.character(),
    today = rep("2023-01-01", 10),
    location = rep(c("villageA", "villageB"), 5),
    distance_to_market = c(rep("less_30", 5), rep("more_30", 5)),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  check_list <- data.frame(
    name = c("logical_xx", "logical_yy"),
    check = c(
      "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      "number_children_05 > 3"
    ),
    description = c(
      "distance to market less than 30 and no access",
      "number of children under 5 seems high"
    ),
    columns_to_clean = c(
      "distance_to_market, access_to_market",
      "number_children_05"
    )
  )


  logical_xx <- dplyr::tibble(
    uuid = rep("5", 2),
    question = c("distance_to_market", "access_to_market"),
    old_value = c("less_30", "no"),
    issue = rep("distance to market less than 30 and no access", 2),
    check_id = rep("logical_xx", 2)
  ) %>%
    dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))


  logical_yy <- dplyr::tibble(
    uuid = c("9", "10"),
    question = rep("number_children_05", 2),
    old_value = c("5", "6"),
    issue = rep("number of children under 5 seems high", 2),
    check_id = rep("logical_yy", 2)
  ) %>%
    dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))

  unbinded_expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(
        logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5)),
        logical_yy = c(rep(FALSE, 8), rep(TRUE, 2))
      ),
    logical_xx = logical_xx,
    logical_yy = logical_yy
  )

  expect_equal(
    check_logical_with_list(test_data,
      uuid_column = "uuid",
      list_of_check = check_list,
      check_id_column = "name",
      check_to_perform_column = "check",
      columns_to_clean_column = "columns_to_clean",
      description_column = "description",
      bind_checks = FALSE
    ),
    unbinded_expected_results
  )

  # binded
  binded_expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(
        logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5)),
        logical_yy = c(rep(FALSE, 8), rep(TRUE, 2))
      ),
    logical_all = rbind(
      logical_xx,
      logical_yy
    )
  )

  expect_equal(
    check_logical_with_list(test_data,
      uuid_column = "uuid",
      list_of_check = check_list,
      check_id_column = "name",
      check_to_perform_column = "check",
      columns_to_clean_column = "columns_to_clean",
      description_column = "description"
    ),
    binded_expected_results
  )

  # with location
  binded_expected_results2 <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(
        logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5)),
        logical_yy = c(rep(FALSE, 8), rep(TRUE, 2))
      ),
    logical_all = rbind(
      logical_xx %>%
        dplyr::mutate(
          location = rep("villageA", 2),
          .after = uuid
        ),
      logical_yy %>%
        dplyr::mutate(
          location = c(
            "villageA",
            "villageB"
          ),
          .after = uuid
        )
    )
  )
  expect_equal(
    check_logical_with_list(test_data,
      uuid_column = "uuid",
      information_to_add = c("location"),
      list_of_check = check_list,
      check_id_column = "name",
      check_to_perform_column = "check",
      columns_to_clean_column = "columns_to_clean",
      description_column = "description"
    ),
    binded_expected_results2
  )
})

test_that("Inputs as list returns correct results", {
  test_data <- data.frame(
    uuid = c(1:10) %>% as.character(),
    today = rep("2023-01-01", 10),
    location = rep(c("villageA", "villageB"), 5),
    distance_to_market = c(rep("less_30", 5), rep("more_30", 5)),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  test_list <- list(
    checked_dataset = test_data,
    other_log = data.frame(xx = "other")
  )

  check_list <- data.frame(
    name = c("logical_xx", "logical_yy"),
    check = c(
      "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      "number_children_05 > 3"
    ),
    description = c(
      "distance to market less than 30 and no access",
      "number of children under 5 seems high"
    ),
    variables_to_clean = c(
      "distance_to_market, access_to_market",
      "number_children_05"
    )
  )

  logical_xx <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5))),
    other_log = data.frame(xx = "other"),
    logical_xx = dplyr::tibble(
      uuid = rep("5", 2),
      question = c("distance_to_market", "access_to_market"),
      old_value = c("less_30", "no"),
      issue = rep("distance to market less than 30 and no access", 2),
      check_id = rep("logical_xx", 2)
    ) %>%
      dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))
  )

  expect_equal(
    check_logical(test_list,
      uuid_column = "uuid",
      check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      columns_to_clean = "distance_to_market, access_to_market",
      description = "distance to market less than 30 and no access"
    ),
    logical_xx
  )

  logical_yy <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(logical_yy = c(rep(FALSE, 8), rep(TRUE, 2))),
    other_log = data.frame(xx = "other"),
    logical_yy = dplyr::tibble(
      uuid = c("9", "10"),
      question = rep("number_children_05", 2),
      old_value = c("5", "6"),
      issue = rep("number of children under 5 seems high", 2),
      check_id = rep("logical_yy", 2)
    ) %>%
      dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))
  )
  unbinded_expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(
        logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5)),
        logical_yy = c(rep(FALSE, 8), rep(TRUE, 2))
      ),
    other_log = data.frame(xx = "other"),
    logical_xx = logical_xx$logical_xx,
    logical_yy = logical_yy$logical_yy
  )

  expect_equal(
    check_logical_with_list(test_list,
      uuid_column = "uuid",
      list_of_check = check_list,
      check_id_column = "name",
      check_to_perform_column = "check",
      columns_to_clean_column = "variables_to_clean",
      description_column = "description",
      bind_checks = F
    ),
    unbinded_expected_results
  )

  binded_expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(
        logical_xx = c(rep(FALSE, 4), TRUE, rep(FALSE, 5)),
        logical_yy = c(rep(FALSE, 8), rep(TRUE, 2))
      ),
    other_log = data.frame(xx = "other"),
    logical_all = rbind(
      logical_xx$logical_xx,
      logical_yy$logical_yy
    )
  )
  expect_equal(
    check_logical_with_list(test_list,
      uuid_column = "uuid",
      list_of_check = check_list,
      check_id_column = "name",
      check_to_perform_column = "check",
      columns_to_clean_column = "variables_to_clean",
      description_column = "description"
    ),
    binded_expected_results
  )
})

# duplicates names
test_that("duplicates names and log names throws error", {
  test_data <- data.frame(
    uuid = c(1:10),
    distance_to_market = rep(c("less_30", "more_30"), 5),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  check_list <- data.frame(
    name = c("logical_xx", "logical_xx"),
    check = c(
      "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      "number_children_05 > 3"
    ),
    description = c(
      "distance to market less than 30 and no access",
      "number of children under 5 seems high"
    ),
    variables_to_clean = c(
      "distance_to_market, access_to_market",
      "number_children_05"
    )
  )

  expect_error(
    check_logical_with_list(test_data,
      uuid_column = "uuid",
      list_of_check = check_list,
      check_id_column = "name",
      check_to_perform_column = "check",
      columns_to_clean_column = "variables_to_clean",
      description_column = "description"
    ),
    "The column name from the checklist contains duplicated."
  )

  check_list2 <- data.frame(
    name = c("logical_xx", "logical_yy"),
    log_name = c("logical_yy", "logical_yy"),
    check = c(
      "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      "number_children_05 > 3"
    ),
    description = c(
      "distance to market less than 30 and no access",
      "number of children under 5 seems high"
    ),
    variables_to_clean = c(
      "distance_to_market, access_to_market",
      "number_children_05"
    )
  )
})

test_that("description cannot be empty", {
  test_data <- data.frame(
    uuid = c(1:10),
    distance_to_market = rep(c("less_30", "more_30"), 5),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  expect_error(
    check_logical(test_data,
      uuid_column = "uuid",
      check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      columns_to_clean = "distance_to_market, access_to_market",
      description = NA
    ),
    "description cannot be empty"
  )
})

test_that("Throws a warning if names are not found", {
  test_data <- data.frame(
    uuid = c(1:10),
    distance_to_market = rep(c("less_30", "more_30"), 5),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6),
    number_children_618 = c(rep(c(0, 1), 4), 5, 6)
  )

  expect_warning(
    check_logical(test_data,
      uuid_column = "uuid",
      check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      description = "distance to market less than 30 and no access"
    ),
    "columns_to_clean not shared, results may not be accurate"
  )

  expect_warning(
    check_logical(test_data,
      uuid_column = "uuid",
      check_to_perform = "rowSums(dplyr::across(starts_with(\"number\")), na.rm = T) > 9",
      description = "distance to market less than 30 and no access"
    ),
    "columns_to_clean not shared, results may not be accurate"
  )

  results <- check_logical(test_data,
    uuid_column = "uuid",
    check_to_perform = "rowSums(dplyr::across(starts_with(\"number\")), na.rm = T) > 9",
    description = "number of children very high"
  ) %>%
    suppressWarnings()
  expected_results <- data.frame(
    uuid = c("9", "10"),
    question = rep("unable to identify", 2),
    old_value = rep("please check this uuid for this check", 2),
    issue = rep("number of children very high", 2),
    check_id = rep("logical_xx", 2),
    check_binding = c("logical_xx ~/~ 9", "logical_xx ~/~ 10")
  )

  expect_equal(expected_results, results$logical_xx)
})

test_that("if check_id names already exists, throws an error", {
  test_data <- data.frame(
    uuid = c(1:10),
    distance_to_market = rep(c("less_30", "more_30"), 5),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6),
    number_children_618 = c(rep(c(0, 1), 4), 5, 6)
  )

  expect_error(
    check_logical(test_data,
      uuid_column = "uuid",
      check_id = "distance_to_market",
      check_to_perform = "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      columns_to_clean = "distance_to_market, access_to_market",
      description = "distance to market less than 30 and no access"
    ),
    "distance_to_market is in the names of the dataset, please change check id name."
  )
})

test_that("when variables_to_clean is empty or null, check_with_list with still work", {
  test_data <- data.frame(
    uuid = c(1:10) %>% as.character(),
    today = rep("2023-01-01", 10),
    LOGICAL = c(T, T, F, T, F, F, F, F, T, T),
    location = rep(c("villageA", "villageB"), 5),
    distance_to_market = c(rep("less_30", 5), rep("more_30", 5)),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  a <- structure(list(
    logic = "distance_to_market==\"less_30\" & access_to_market == \"no\"",
    des = "iuiui", var = "", check = "aa"
  ), row.names = 2L, class = "data.frame")

  expect_warning(
    check_logical_with_list(
      dataset = test_data,
      list_of_check = a,
      information_to_add = NULL,
      uuid_column = "uuid",
      check_id_column = "check",
      check_to_perform_column = "logic",
      columns_to_clean_column = "var",
      description_column = "des"
    ),
    "columns_to_clean not shared, results may not be accurate"
  )

  expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(aa = c(rep(FALSE, 4), TRUE, rep(FALSE, 5))),
    logical_all = tibble::tibble(
      uuid = c("5", "5"),
      question = c("distance_to_market", "access_to_market"),
      old_value = c("less_30", "no"),
      issue = c("iuiui", "iuiui"),
      check_id = "aa",
      check_binding = "aa ~/~ 5"
    )
  )
  results_1 <- check_logical_with_list(
    dataset = test_data,
    list_of_check = a,
    information_to_add = NULL,
    uuid_column = "uuid",
    check_id_column = "check",
    check_to_perform_column = "logic",
    columns_to_clean_column = "var",
    description_column = "des"
  ) %>%
    suppressWarnings()


  results_2 <- check_logical_with_list(
    dataset = test_data,
    list_of_check = a,
    information_to_add = NULL,
    uuid_column = "uuid",
    check_id_column = "check",
    check_to_perform_column = "logic",
    columns_to_clean_column = NULL,
    description_column = "des"
  ) %>%
    suppressWarnings()

  results_3 <- check_logical_with_list(
    dataset = test_data,
    list_of_check = a,
    information_to_add = NULL,
    uuid_column = "uuid",
    check_id_column = "check",
    check_to_perform_column = "logic",
    columns_to_clean_column = NULL,
    description_column = "des"
  ) %>%
    suppressWarnings()

  expect_equal(expected_results, results_1)
  expect_equal(expected_results, results_2)
  expect_equal(expected_results, results_3)
})

test_that("id as number is converted to character", {
  test_data <- data.frame(
    uuid = c(1:10) %>% as.character(),
    today = rep("2023-01-01", 10),
    location = rep(c("villageA", "villageB"), 5),
    distance_to_market = c(rep("less_30", 5), rep("more_30", 5)),
    access_to_market = c(rep("yes", 4), rep("no", 6)),
    number_children_05 = c(rep(c(0, 1), 4), 5, 6)
  )

  check_list <- data.frame(
    name = c(1, 2),
    check = c(
      "distance_to_market == \"less_30\" & access_to_market == \"no\"",
      "number_children_05 > 3"
    ),
    description = c(
      "distance to market less than 30 and no access",
      "number of children under 5 seems high"
    ),
    columns_to_clean = c(
      "distance_to_market, access_to_market",
      "number_children_05"
    )
  )

  expect_no_error(check_logical_with_list(test_data,
    uuid_column = "uuid",
    list_of_check = check_list,
    check_id_column = "name",
    check_to_perform_column = "check",
    columns_to_clean_column = "columns_to_clean",
    description_column = "description",
    bind_checks = FALSE
  ))
})


test_that("Works with one test", {
  test_data <- data.frame(
    uuid = c(1:10) %>% as.character(),
    today = rep("2023-01-01", 10),
    location = rep(c("villageA", "villageB"), 5),
    expenses = c(rep(50, 9), 5000000)
  )

  expected_results <- list(
    checked_dataset = test_data %>%
      dplyr::mutate(logical_xx = c(rep(FALSE, 9), TRUE)),
    logical_xx = dplyr::tibble(
      uuid = "10",
      question = "expenses",
      old_value = "5000000",
      issue = "check expenses above 1000000",
      check_id = "logical_xx"
    ) %>%
      dplyr::mutate(check_binding = paste(check_id, "~/~", uuid))
  )

  expect_equal(
    check_logical(test_data,
      uuid_column = "uuid",
      check_to_perform = "expenses > 1000000",
      columns_to_clean = "expenses",
      description = "check expenses above 1000000"
    ),
    expected_results
  )
})
