context("editing")

test_that("we can publish an update", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  env <- env_load()

  # Make a test package
  package <- create_dummy_package(env$mn)
  expect_named(package, c("metadata", "data", "resource_map"))

  # Publish an update on it
  update <- publish_update(env$mn,
                           metadata_old_pid = package$metadata,
                           data_old_pids = package$data,
                           resmap_old_pid = package$resource_map)

  expect_named(update, c("metadata", "resource_map"))
  expect_true(all(object_exists(env$mn, unlist(update))))
})

test_that("we can create a resource map", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  env <- env_load()

  # Make the beginnings of test package
  metadata_pid <- create_dummy_metadata(env$mn)
  data_pid <- create_dummy_object(env$mn)

  response <- create_resource_map(env$mn, metadata_pid, data_pid)

  # Check the object exists
  expect_true(object_exists(env$mn, response))

  # Also check it's in the Solr index
  expect_true(response %in% get_related_pids(env$mn, metadata_pid))

})

test_that("we can update a resource map", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  env <- env_load()

  # Create an initial package
  response <- create_dummy_package(env$mn)

  updated <- update_resource_map(env$mn, old_resource_map_pid = response$resource_map, metadata_pid = response$metadata, data_pids = response$data)

  # Check the object exists
  expect_true(object_exists(env$mn, updated))

  # Also check it's in the Solr index
  expect_true(updated %in% get_related_pids(env$mn, response$metadata))
})
