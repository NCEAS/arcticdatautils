context("formats")

test_that("a format can be returned", {
  fmt <- format_eml()
  expect_is(fmt, "character")
  expect_gt(length(fmt), 0)
})
