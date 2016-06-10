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

test_that("an identifier can be manually specified when publishing an update", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  env <- env_load()

  # Make a test package
  package <- create_dummy_package(env$mn)
  expect_named(package, c("metadata", "data", "resource_map"))

  # Publish an update on it
  new_identifier <- uuid::UUIDgenerate()
  update <- publish_update(env$mn,
                           metadata_old_pid = package$metadata,
                           data_old_pids = package$data,
                           resmap_old_pid = package$resource_map,
                           identifier = new_identifier)

  expect_equal(update$metadata, new_identifier)
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

test_that("otherEntity elements are set when publishing an update", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  library(dataone)
  library(EML)
  library(stringr)
  env <- env_load()

  # Create an initial package
  response <- create_dummy_package(env$mn)

  # Create a new dummy object
  data_path <- file.path(system.file("tests", "testfiles", "test-data.csv", package = "arcticdatautils"))
  object <- publish_object(env$mn, data_path, "text/csv")

  # Note I use the wrong data pid argument here. I send the new data pid instead
  # which lets me update the data in a package when doing a publish_update()
  # call
  update <- publish_update(env$mn,
                           metadata_old_pid = response$metadata,
                           resmap_old_pid = response$resource_map,
                           data_old_pids = object)

  tmp <- tempfile()
  writeLines(rawToChar(getObject(env$mn, update$metadata)), con = tmp)
  doc <- read_eml(tmp)

  expect_true(length(doc@dataset@otherEntity) == 1)

  sysmeta <- getSystemMetadata(env$mn, object)
  doc_id <- get_doc_id(sysmeta)
  expect_true(str_detect(doc@dataset@otherEntity[[1]]@physical[[1]]@distribution[[1]]@online@url@.Data, doc_id))

  file.remove(tmp)
})

test_that("an object can be published with a SID", {
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  env <- env_load()
  library(dataone)

  # Create a dummy object
  data_path <- file.path(system.file("tests", "testfiles", "test-data.csv", package = "arcticdatautils"))
  new_sid <- uuid::UUIDgenerate()
  pid <- publish_object(env$mn,
                        filepath = data_path,
                        format_id = "text/csv",
                        sid = new_sid)

  sysmeta <- getSystemMetadata(env$mn, pid)

  expect_true(sysmeta@seriesId == new_sid)
})

test_that("SIDs are maintained when publishing an update to an object with a SID",{
  if (!is_token_set()) {
    skip("No token set. Skipping test.")
  }

  # Setup
  env <- env_load()
  library(dataone)

  # Create a dummy object
  metadata_path <- file.path(system.file("example-eml.xml", package = "arcticdatautils"))
  new_sid <- uuid::UUIDgenerate()
  pid <- publish_object(env$mn,
                        filepath = metadata_path,
                        format_id = "	eml://ecoinformatics.org/eml-2.1.1",
                        sid = new_sid)
  resmap_pid <- create_resource_map(env$mn, metadata_pid = pid)

  response <- publish_update(env$mn, metadata_old_pid = pid, resmap_old_pid = resmap_pid)

  response # Finish this test
})
