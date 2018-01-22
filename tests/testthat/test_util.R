#' test_util.R

context("util")

test_that("paths can be joined", {
  expect_equal(path_join(""), "")
  expect_equal(path_join(1), "1")
  expect_equal(path_join(NULL), "")
  expect_equal(path_join(c("./", "./test")), "./test")

  # Test that it handles variables and not just literal values
  part_one <- "./"
  part_two <- "./asdf"
  expect_equal(path_join(c(part_one, part_two)), "./asdf")
  expect_equal(path_join(c(part_one, "./", part_two)), "./asdf")

  # Other tests
  expect_equal(path_join("~/src/arcticdata./inst/asdf"), "~/src/arcticdata/inst/asdf")
})
