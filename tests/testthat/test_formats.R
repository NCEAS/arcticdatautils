context("Formats")

test_that("valid formats are valid and invalid ones are not", {
  expect_true(check_format("text/csv"))
  expect_error(check_format("badformat"))
})

test_that("a format can be returned", {
  fmt <- format_eml()
  expect_is(fmt, "character")
  expect_gt(length(fmt), 0)
})
