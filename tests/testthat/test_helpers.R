#' test_helpers.R

library(testthat)
context("helpers")

test_that("a dummy package can be created", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  library(dataone)

  env <- env_load()
  mn <- MNode(env$mn_base_url)

  result <- create_dummy_package(mn)

  expect_true(object_exists(mn, result$metadata))
  expect_true(object_exists(mn, result$data))
  expect_true(object_exists(mn, result$resource_map))
})
