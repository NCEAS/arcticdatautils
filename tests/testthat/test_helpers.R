#' test_helpers.R

library(testthat)
context("helpers")

test_that("a dummy package can be created", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  library(dataone)

  env <- env_load("development")

  result <- create_dummy_package(env$mn)

  expect_true(object_exists(env$mn, result$metadata))
  expect_true(object_exists(env$mn, result$data))
  expect_true(object_exists(env$mn, result$resource_map))
})
