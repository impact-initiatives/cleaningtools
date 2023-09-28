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

test_that("coerce_to_character coerce correctly", {
  expect_equal(coerce_to_character(c("a", NA)), c("a", NA_character_))
  expect_equal(coerce_to_character(c(1, 10000000, NA)), c("1", "10000000", NA_character_))

})

