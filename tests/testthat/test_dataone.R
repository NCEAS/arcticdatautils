context("dataone")

node <- env_load()$node

test_that("permissions can be checked", {
  if (!is_token_set(node)) {
    skip("No token set. Skipping test.")
  }

  id <- create_dummy_object(node)
  expect_true(is_authorized(node, id, "write"))
})

test_that("permissions can be checked on multiple objects", {
  if (!is_token_set(node)) {
    skip("No token set. Skipping test.")
  }

  a <- create_dummy_object(node)
  b <- create_dummy_object(node)

  expect_true(all(is_authorized(node, c(a, b), "write")))
})

test_that("checking permissions on a missing object returns an error", {
  if (!is_token_set(node)) {
    skip("No token set. Skipping test.")
  }

  # Create a pid likely to not exist
  id <- paste0(uuid::UUIDgenerate(), uuid::UUIDgenerate())

  expect_error(is_authorized(node, id, "write"))
})
