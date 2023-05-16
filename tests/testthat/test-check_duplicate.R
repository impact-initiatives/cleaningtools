test_that("3 duplicates, return 3 duplicates", {
  testdata <- data.frame(
    uuid = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7)
  ) %>%
    dplyr::rename(`_uuid` = uuid)
  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("_uuid", 3),
      issue = rep("duplicated uuid", 3)
    )
  )
  expect_equal(check_duplicate(testdata), expected_results)
})

test_that("no duplicates, return no duplicates", {
  testdata <- data.frame(
    uuid = letters[1:7],
    col_a = runif(7),
    col_b = runif(7)
  ) %>%
    dplyr::rename(`_uuid` = uuid)

  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = character(),
      old_value = character(),
      question = character(),
      issue = character()
    )
  )
  expect_equal(check_duplicate(testdata), expected_results)
})

test_that("3 duplicates, return 3 duplicates - names different than _uuid", {
  testdata <- data.frame(
    `X_uuid` = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7)
  )
  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("X_uuid", 3),
      issue = rep("duplicated uuid", 3)
    )
  )
  expect_equal(check_duplicate(testdata, "X_uuid"), expected_results)
})

test_that("no duplicates, return no duplicates - names different than _uuid", {
  testdata <- data.frame(
    `X_uuid` = letters[1:7],
    col_a = runif(7),
    col_b = runif(7)
  )

  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = character(),
      old_value = character(),
      question = character(),
      issue = character()
    )
  )

  expect_equal(check_duplicate(testdata, "X_uuid"), expected_results)
})

test_that("3 duplicates, return 3 duplicates - names different than _uuid but _uuid is included", {
  testdata <- data.frame(
    `id` = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7),
    `_uuid` = c(LETTERS[1:7])
  )

  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("id", 3),
      issue = rep("duplicated uuid", 3)
    )
  )

  expect_equal(check_duplicate(testdata, "id"), expected_results)
})

test_that("no duplicates in the id name, return no duplicates - names different than _uuid but _uuid is included", {
  testdata <- data.frame(
    `id` = LETTERS[1:7],
    col_a = runif(7),
    col_b = runif(7),
    `_uuid` = c(letters[1:4], "a", "b", "c")
  )

  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = character(),
      old_value = character(),
      question = character(),
      issue = character()
    )
  )

  expect_equal(check_duplicate(testdata, "id"), expected_results)
})


test_that("Adds to the list if there is already a check.", {
  test_list <- list(
    checked_dataset = data.frame(
      `uuid` = c(letters[1:4], "a", "b", "c"),
      col_a = runif(7),
      col_b = runif(7)
    ) %>%
      dplyr::rename(`_uuid` = uuid),
    outlier_log = data.frame(
      uuid = NULL,
      old_value = NULL,
      question = NULL,
      issue = NULL
    )
  )

  expected_results <- list(
    checked_dataset = test_list$checked_dataset,
    outlier_log = data.frame(
      uuid = NULL,
      old_value = NULL,
      question = NULL,
      issue = NULL
    ),
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("_uuid", 3),
      issue = rep("duplicated uuid", 3)
    )
  )
  expect_equal(check_duplicate(test_list), expected_results)
})

test_that("Check that the list has an object called checked_dataset", {
  test_list <- list(
    dataset = data.frame(
      `_uuid` = c(letters[1:4], "a", "b", "c"),
      col_a = runif(7),
      col_b = runif(7)
    ),
    outlier_log = data.frame(
      uuid = NULL,
      old_value = NULL,
      question = NULL,
      issue = NULL
    )
  )
  testthat::expect_error(
    check_duplicate(test_list),
    "Cannot identify the dataset in the list"
  )
})

test_that("If column does not exist, return an error", {
  testdata <- data.frame(
    uuid = letters[1:7],
    col_a = runif(7),
    col_b = runif(7)
  ) %>%
    dplyr::rename(`_uuid` = uuid)
  testthat::expect_error(
    check_duplicate(testdata, "X_uuid"),
    "Cannot find X_uuid in the names of the dataset"
  )
})
