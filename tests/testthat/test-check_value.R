test_that("values are correctly flagged", {
  ###################################################

  df <- data.frame(
    X_uuid = paste0("uuid_", 1:100),
    age = c(sample(18:80, replace = T, size = 96), 99, 99, 98, 88),
    gender = c("99", sample(c("male", "female"), replace = T, size = 95), "98", "98", "88", "888")
  )

  output <- check_value(dataset = df, uuid_column = "X_uuid", element_name = "checked_dataset", values_to_look = c(99, 98, 88, 888))

  expected_output <- data.frame(
    uuid = c(
      "uuid_1", "uuid_97", "uuid_97", "uuid_98", "uuid_98", "uuid_99",
      "uuid_99", "uuid_100", "uuid_100"
    ),
    question = c(
      "gender", "age", "gender", "age", "gender", "age", "gender",
      "age", "gender"
    ),
    old_value = c("99", "99", "98", "99", "98", "98", "88", "88", "888"),
    issue = rep("Possible value to be changed to NA", 9)
  )

  expect_equal(as.data.frame(output$flaged_value), expected_output)

  ################################################## 33
  df2 <- data.frame(
    X_uuid = paste0("uuid_", 1:100),
    age = c(sample(18:80, replace = T, size = 100)),
    gender = c(sample(c("male", "female"), replace = T, size = 100))
  )

  output2 <- check_value(dataset = df2, uuid_column = "X_uuid", element_name = "checked_dataset", values_to_look = c(99, 98, 88, 888))

  expect_equal(nrow(as.data.frame(output2$flaged_value)), 0)
})
