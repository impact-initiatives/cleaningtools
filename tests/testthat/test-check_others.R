input_datasets <- list(
  empty_dataset = data.frame(uuid = NA_character_,
                             var2 = NA_character_,
                             var3 = NA_character_),

  dataset_1 = data.frame(
    uuid = c("uuid_1", "uuid_2"),
    var2 = c(NA, NA),
    var3 = c("some_answers", NA)
  ),

  dataset_2 = data.frame(
    X_uuid = c("uuid_1", "uuid_2", "uuid_3"),
    var1 = c(1, 0, 1),
    var2 = c(NA, NA, "other_answer_1"),
    var3 = c("other_answer_2", NA, "")
  ),

  dataset_3 = data.frame(
    X_uuid = c("uuid_1", "uuid_2", "uuid_3"),
    var1 = c(1, 0, 1),
    var2 = c(NA, NA, "other_answer_1"),
    var3 = c("other_answer_2", "other_answer_3", "")
  ),

  list_1 = list(
    checked_dataset = data.frame(
      X_uuid = c("uuid_1", "uuid_2", "uuid_3"),
      var1 = c(1, 0, 1),
      var2 = c(NA, NA, "other_answer_1"),
      var3 = c("other_answer_2", "other_answer_3", "")
    ),

    xx_log = tibble()
  )
)


output_datasets <- list(
  expected_output_1 = tibble(
    uuid = c("uuid_1", "uuid_3"),
    question = c("var3", "var2"),
    old_value = c("other_answer_2", "other_answer_1"),
    issue = c("recode other", "recode other")
  ),

  expected_output_2 = tibble(
    uuid = c("uuid_1", "uuid_3"),
    question = c("var3", "var2"),
    old_value = c("other_answer_2", "other_answer_1"),
    issue = c("recode other", "recode other")
  ),

  expected_output_3 = tibble(
    uuid = character(),
    question = character(),
    old_value = character(),
    issue = character()
  ),

  expected_output_4 = tibble(
    uuid = c("uuid_1", "uuid_2", "uuid_3"),
    question = c("var3", "var3", "var2"),
    old_value = c("other_answer_2", "other_answer_3", "other_answer_1"),
    issue = c("recode other", "recode other", "recode other")
  ),

  expected_output_5 = list(
    checked_dataset = data.frame(
      X_uuid = c("uuid_1", "uuid_2", "uuid_3"),
      var1 = c(1, 0, 1),
      var2 = c(NA, NA, "other_answer_1"),
      var3 = c("other_answer_2", "other_answer_3", "")
    ),
    xx_log = tibble(),

    other_log = tibble(
      uuid = c("uuid_1", "uuid_2", "uuid_3"),
      question = c("var3", "var3", "var2"),
      old_value = c("other_answer_2", "other_answer_3", "other_answer_1"),
      issue = c("recode other", "recode other", "recode other")
    )

  )

)

test_that("check 1 :: uuid", {
  expect_error(
    check_others(
      dataset = input_datasets$empty_dataset,
      uuid = "_uuid_",
      var_list = c("var2", "var3")
    ),
    regex = "uuid column is missing"
  )
})


test_that("check 2 :: variables names", {
  expect_error(
    check_others(
      dataset = input_datasets$empty_dataset,
      uuid = "uuid",
      var_list = c("var2", "var1")
    ),
    regex = "at least one variable is missing from the dataset"
  )

  expect_error(check_others(dataset = input_datasets$empty_dataset,
                            uuid = "uuid"),
               regex = "provide the list of follow up questions")
})


test_that("check 3 :: check output format", {
  expect_true("checked_dataset" %in% names(
    check_others(
      dataset = input_datasets$dataset_1,
      uuid = "uuid",
      var_list = c("var2", "var3")
    )
  ))

  expect_true("other_log" %in% names(
    check_others(
      dataset = input_datasets$dataset_1,
      uuid = "uuid",
      var_list = c("var2", "var3")
    )
  ))


})

test_that("check 4 :: variables appear (or not) in the output", {
  expect_true(
    "var3" %in% check_others(
      dataset = input_datasets$dataset_1,
      uuid = "uuid",
      var_list = c("var2", "var3")
    )$other_log$question
  )

  expect_false(
    "var2" %in% check_others(
      dataset = input_datasets$dataset_1,
      uuid = "uuid",
      var_list = c("var2", "var3")
    )$other_log$question
  )

})



test_that("check 5 :: output check", {
  expect_equal(
    check_others(
      dataset = input_datasets$dataset_2,
      uuid = "X_uuid",
      var_list = c("var2", "var3")
    )$other_log,
    output_datasets$expected_output_1
  )

  expect_equal(
    check_others(
      dataset = input_datasets$empty_dataset,
      uuid = "uuid",
      var_list = c("var2", "var3")
    )$other_log,
    output_datasets$expected_output_3
  )

  expect_equal(
    check_others(
      dataset = input_datasets$dataset_3,
      uuid = "X_uuid",
      var_list = c("var2", "var3")
    )$other_log,
    output_datasets$expected_output_4
  )

  expect_equal(
    check_others(
      dataset = input_datasets$list_1,
      uuid = "X_uuid",
      var_list = c("var2", "var3")
    ),
    output_datasets$expected_output_5
  )


})
