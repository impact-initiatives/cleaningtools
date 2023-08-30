test_that("3 duplicates, return 3 duplicates", {
  testdata <- data.frame(
    uuid = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7)
  )
  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("uuid", 3),
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
  expect_equal(check_duplicate(testdata), expected_results)
})

test_that("3 duplicates, return 3 duplicates - names different than uuid", {
  testdata <- data.frame(
    X_uuid = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7)
  )
  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("X_uuid", 3),
      issue = rep("duplicated X_uuid", 3)
    )
  )
  expect_equal(check_duplicate(testdata, uuid_column = "X_uuid"), expected_results)
})

test_that("no duplicates, return no duplicates - names different than uuid", {
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

test_that("3 duplicates, return 3 duplicates - names different than uuid but uuid is included", {
  testdata <- data.frame(
    `id` = c(letters[1:4], "a", "b", "c"),
    col_a = runif(7),
    col_b = runif(7),
    uuid = c(LETTERS[1:7])
  )

  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("a", "b", "c"),
      old_value = c("a", "b", "c"),
      question = rep("id", 3),
      issue = rep("duplicated id", 3)
    )
  )

  expect_equal(check_duplicate(testdata, "id"), expected_results)
})

test_that("no duplicates in the id name, return no duplicates - names different than uuid but uuid is included", {
  testdata <- data.frame(
    `id` = LETTERS[1:7],
    col_a = runif(7),
    col_b = runif(7),
    uuid = c(letters[1:4], "a", "b", "c")
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
    ),
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
      question = rep("uuid", 3),
      issue = rep("duplicated uuid", 3)
    )
  )
  expect_equal(check_duplicate(test_list), expected_results)
})

test_that("Check that the list has an object called checked_dataset", {
  test_list <- list(
    dataset = data.frame(
      uuid = c(letters[1:4], "a", "b", "c"),
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
  )
  testthat::expect_error(
    check_duplicate(testdata, "X_uuid"),
    "Cannot find X_uuid in the names of the dataset"
  )
})

test_that("If 2 values are passed, the duplicated of the combination are passed", {
  testdata <- data.frame(
    uuid = letters[1:7],
    village = paste("village", c(1:3, 1:3, 4)),
    ki_identifier = paste0("xx_", c(1:5, 3, 4))
  )
  expected_output <- list(
    checked_dataset = testdata,
    duplicate_log = dplyr::tibble(
      uuid = c("f", "f"),
      question = c("village", "ki_identifier"),
      old_value = c("village 3", "xx_3"),
      issue = rep("duplicated village ~/~ ki_identifier", 2)
    )
  )
  testthat::expect_equal(
    check_duplicate(testdata, columns_to_check = c("village", "ki_identifier"), uuid = "uuid"),
    expected_output
  )
})

test_that("If col_to_check is null, uuid is checked", {
  testdata <- data.frame(
    uuid = letters[c(1:6, 6)],
    village = paste("village", c(1:3, 1:3, 4)),
    ki_identifier = paste0("xx_", c(1:5, 3, 4))
  )
  expected_output <- list(
    checked_dataset = testdata,
    duplicate_log = data.frame(
      uuid = c("f"),
      old_value = c("f"),
      question = c("uuid"),
      issue = c("duplicated uuid")
    )
  )
  testthat::expect_equal(
    check_duplicate(testdata,
      uuid = "uuid"
    ),
    expected_output
  )
})

test_that("If col_to_check is given, columns_to_check is checked", {
  testdata <- data.frame(
    uuid = letters[c(1:7)],
    village = paste("village", c(1:3, 1:3, 4)),
    ki_identifier = paste0("xx_", c(1:5, 3, 4))
  )
  expected_output <- list(
    checked_dataset = testdata,
    duplicate_log = dplyr::tibble(
      uuid = c("d", "e", "f"),
      question = rep("village", 3),
      old_value = c("village 1", "village 2", "village 3"),
      issue = rep("duplicated village", 3)
    )
  )
  testthat::expect_equal(
    check_duplicate(testdata,
      columns_to_check = "village"
    ),
    expected_output
  )
})

test_that("If 2 values are passed with an error in names, there should be an error", {
  testdata <- data.frame(
    uuid = letters[1:7],
    village = paste("village", c(1:3, 1:3, 4)),
    ki_identifier = paste0("xx_", c(1:5, 3, 4))
  )

  testthat::expect_error(
    check_duplicate(testdata, columns_to_check = c("village", "ki"), uuid = "uuid"),
    "Cannot find village ~/~ ki in the names of the dataset"
  )
})

test_that("If 2 variables are from different types (e.g. int and text), it will work", {
  testdata <- data.frame(
    id = c(letters[1:4], "a", "b", "c"),
    col_a = c(1, 2, 3, 4, 1, 2, 3),
    col_b = runif(7),
    uuid = LETTERS[1:7]
  )

  expected_results <- list(
    checked_dataset = testdata,
    duplicate_log = tibble::tibble(
      uuid = c("E", "E", "F", "F", "G", "G"),
      question = rep(c("id", "col_a"), 3),
      old_value = c("a", "1", "b", "2", "c", "3"),
      issue = rep("duplicated id ~/~ col_a", 6)
    )
  )
  testthat::expect_equal(
    check_duplicate(testdata,
      uuid_column = "uuid",
      columns_to_check = c("id", "col_a")
    ),
    expected_results
  )
})
