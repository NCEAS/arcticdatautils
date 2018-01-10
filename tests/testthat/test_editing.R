context("editing")

mn <- env_load()$mn

test_that("we can publish an update", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  # Make a test package
  package <- create_dummy_package(mn)
  expect_named(package, c("metadata", "resource_map", "data"))

  # Publish an update on it
  update <- publish_update(mn,
                           package$metadata,
                           package$resource_map,
                           package$data,
                           check_first = FALSE)

  expect_named(update, c("metadata", "resource_map", "data"))
  expect_true(all(object_exists(mn, unlist(update))))
})

test_that("an identifier can be manually specified when publishing an update", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  # Make a test package
  package <- create_dummy_package(mn)
  expect_named(package, c("metadata", "resource_map", "data"))

  # Publish an update on it
  new_identifier <- uuid::UUIDgenerate()
  update <- publish_update(mn,
                           package$metadata,
                           package$resource_map,
                           package$data,
                           identifier = new_identifier,
                           check_first = FALSE)

  expect_equal(update$metadata, new_identifier)
})

test_that("we can create a resource map", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  # Make the beginnings of test package
  metadata_pid <- create_dummy_metadata(mn)
  data_pid <- create_dummy_object(mn)

  response <- create_resource_map(mn, metadata_pid, data_pid)

  expect_true(object_exists(mn, response))
  expect_equal(response, get_package(mn, response)$resource_map)

})

test_that("we can update a resource map", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  # Create an initial package
  response <- create_dummy_package(mn)

  updated <- update_resource_map(mn,
                                 resource_map_pid = response$resource_map,
                                 metadata_pid = response$metadata,
                                 data_pids = response$data)

  # Check the object exists
  expect_true(object_exists(mn, updated))

  # Also check it's in the Solr index
  suppressWarnings({
    pkg <- get_package(mn, response$metadata)
  })

  expect_equal(updated, pkg$resource_map)
})

test_that("an object can be published with a SID", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  library(dataone)

  # Create a dummy object
  tmp <- tempfile()
  dummy_df <- data.frame(x = 1:100, y = 1:100)
  write.csv(dummy_df, file = tmp)

  new_sid <- uuid::UUIDgenerate()
  pid <- publish_object(mn,
                        tmp,
                        "text/csv",
                        sid = new_sid)
  file.remove(tmp) # Remove dummy data file

  sysmeta <- getSystemMetadata(mn, pid)

  expect_true(sysmeta@seriesId == new_sid)
})

test_that("SIDs are maintained when publishing an update to an object with a SID",{
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  library(dataone)

  # Create a dummy object
  metadata_path <- file.path(system.file("example-eml.xml", package = "arcticdatautils"))
  new_sid <- uuid::UUIDgenerate()
  pid <- publish_object(mn,
                        metadata_path,
                        "eml://ecoinformatics.org/eml-2.1.1",
                        sid = new_sid)
  resmap_pid <- create_resource_map(mn, metadata_pid = pid)

  response <- publish_update(mn, pid, resmap_pid, check_first = FALSE)

  sysmeta <- getSystemMetadata(mn, response$metadata)
  expect_equal(sysmeta@seriesId, new_sid)
})

test_that("publishing an update produces an error when identifiers are duplicated across args", {
  expect_error(publish_update(mn, "XYZ", "XYZ", "XYZ"), "One or more dupes was found")
})

test_that("we can publish an update to an object", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  old <- create_dummy_object(mn)

  # Create a CSV to replace it
  tmp <- tempfile(fileext = ".csv")
  csv <- data.frame(x = 1:50)
  write.csv(csv, tmp)

  suppressWarnings({
    upd <- update_object(mn, old, tmp)
  })

  file.remove(tmp)
  sm <- dataone::getSystemMetadata(mn, upd)

  expect_equal(sm@fileName, basename(tmp))
  expect_equal(sm@obsoletes, old)
  expect_equal(sm@formatId, "text/csv")
})

test_that("we can publish an update to an object and specify our own format id", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  old <- create_dummy_object(mn)

  # Create a CSV to replace it
  tmp <- tempfile(fileext = ".csv")
  csv <- data.frame(x = 1:50)
  write.csv(csv, tmp)

  upd <- update_object(mn, old, tmp, "text/plain")
  file.remove(tmp)
  sm <- dataone::getSystemMetadata(mn, upd)

  expect_equal(sm@formatId, "text/plain")
})

test_that("rightsholder is properly set back after publishing an update", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg <- create_dummy_package(mn)

  set_result <- set_rights_holder(mn, unlist(pkg), "CN=arctic-data-admins,DC=dataone,DC=org")
  expect_true(all(set_result))

  new_pkg <- publish_update(mn, pkg$metadata, pkg$resource_map, pkg$data, check_first = FALSE)
  rhs <- lapply(unlist(pkg), function(pid) {
    dataone::getSystemMetadata(mn, pid)@rightsHolder
  })

  expect_true(all(unlist(rhs) == "CN=arctic-data-admins,DC=dataone,DC=org"))
})

test_that("publish update returns an error if its arguments are malformed", {
  expect_error(publish_update(mn, metadata_pid = 1))
  expect_error(publish_update(mn, metadata_pid = "a", resource_map_pid = "b", data_pids = list(1, 2, 3)))
})

test_that("update_object updates the packageId for EML object updates", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  eml_pid <- create_dummy_metadata(mn)
  eml_path <- tempfile(fileext = ".xml")
  writeBin(dataone::getObject(mn, eml_pid), eml_path)

  new_pid <- update_object(mn, eml_pid, eml_path, format_id = format_eml())
  updated_eml_path <- tempfile(fileext = ".xml")
  writeBin(dataone::getObject(mn, new_pid), updated_eml_path)

  doc <- xml2::read_xml(updated_eml_path)
  expect_equal(new_pid, xml2::xml_attr(xml2::xml_root(doc), "packageId"))
})

test_that("publish_update removes the deprecated eml@access element", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pids <- create_dummy_package(mn)
  eml_path <- tempfile(fileext = ".xml")
  writeBin(dataone::getObject(mn, pids$metadata), eml_path)

  eml <- EML::read_eml(eml_path)
  # Populate dummy access element
  eml@access@allow <- c(new("allow", .Data = "hello"))
  write_eml(eml, eml_path)

  new_pids <- publish_update(mn, pids$metadata, pids$resource_map, metadata_path = eml_path)
  updated_eml_path <- tempfile(fileext = ".xml")
  writeBin(dataone::getObject(mn, new_pids$metadata), updated_eml_path)

  new_eml <- EML::read_eml(updated_eml_path)
  expect_equal(0, length(new_eml@access@allow))
})
