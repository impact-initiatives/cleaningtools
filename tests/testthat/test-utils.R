test_that("detect_variable works", {
  string_to_check <- c(
    "ki_age> 1",
    "noo==\"mar\"",
    "sum(var1, var2, na.rm = T)",
    "oks != \"mar2\"",
    "oknospace!=\"mar2\""
  )
  string_to_find <- c(
    "ki_age",
    "noo",
    "var2,",
    "oks",
    "oknospace"
  )
  expect_equal(detect_variable(string_to_check), string_to_find)
})
