context("formats")

test_that("valid formats are valid and invalid ones are not", {
  expect_true(check_format("text/csv"))
  expect_error(check_format("badformat"))
})
