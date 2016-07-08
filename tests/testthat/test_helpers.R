#' test_helpers.R

context("helpers")

mn <- env_load()$mn

test_that("a dummy package can be created", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  library(dataone)

  result <- create_dummy_package(mn)

  expect_true(object_exists(mn, result$metadata))
  expect_true(object_exists(mn, result$data))
  expect_true(object_exists(mn, result$resource_map))
})
