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
                           package$data)

  expect_named(update, c("metadata", "resource_map"))
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
                           identifier = new_identifier)

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
  expect_equal(response, get_package(mn, metadata_pid)$resource_map)

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
  expect_equal(updated, get_package(mn, response$metadata)$resource_map)
})

test_that("otherEntity elements are set when publishing an update", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  # Setup
  library(dataone)
  library(EML)
  library(stringr)

  # Create an initial package
  response <- create_dummy_package(mn)

  # Create a new dummy object
  tmp <- tempfile()
  dummy_df <- data.frame(x = 1:100, y = 1:100)
  write.csv(dummy_df, file = tmp)
  object <- publish_object(mn, tmp, "text/csv")
  file.remove(tmp) # Remove dummy data file

  # Note I use the wrong data pid argument here. I send the new data pid instead
  # which lets me update the data in a package when doing a publish_update()
  # call
  update <- publish_update(mn,
                           response$metadata,
                           response$resource_map,
                           object)


  tmp <- tempfile()
  writeLines(rawToChar(getObject(mn, update$metadata)), con = tmp)
  doc <- read_eml(tmp)

  expect_true(length(doc@dataset@otherEntity) == 1)

  sysmeta <- getSystemMetadata(mn, object)
  doc_id <- get_doc_id(sysmeta)
  expect_true(str_detect(doc@dataset@otherEntity[[1]]@physical[[1]]@distribution[[1]]@online@url@.Data, doc_id))

  file.remove(tmp)
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

  response <- publish_update(mn, pid, resmap_pid)

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

  upd <- update_object(mn, old, tmp)
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

test_that("replication policies are set to FALSE on new objects", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  path <- tempfile()
  writeLines(LETTERS, path)
  pid <- publish_object(mn, path)

  sysmeta <- dataone::getSystemMetadata(mn, pid)
  expect_false(sysmeta@replicationAllowed)
})

test_that("replication policies are honored when updating objects", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  path <- tempfile()
  writeLines(LETTERS, path)
  pid <- publish_object(mn, path)

  sysmeta <- dataone::getSystemMetadata(mn, pid)
  expect_false(sysmeta@replicationAllowed)

  sysmeta@replicationAllowed <- TRUE
  sysmeta <- dataone::updateSystemMetadata(mn, pid = pid, sysmeta = sysmeta)

  sysmeta <- dataone::getSystemMetadata(mn, pid)
  expect_true(sysmeta@replicationAllowed)

  new_pid <- update_object(mn, pid, path)
  sysmeta <- dataone::getSystemMetadata(mn, pid)
  expect_true(sysmeta@replicationAllowed)
})


test_that("replication policies are honored when updating packages", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg <- create_dummy_package(mn)

  sysmeta <- dataone::getSystemMetadata(mn, pkg$metadata)
  expect_false(sysmeta@replicationAllowed)
  sysmeta@replicationAllowed <- TRUE
  sysmeta <- dataone::updateSystemMetadata(mn, pid = pkg$metadata, sysmeta = sysmeta)

  sysmeta <- dataone::getSystemMetadata(mn, pkg$resource_map)
  expect_false(sysmeta@replicationAllowed)
  sysmeta@replicationAllowed <- TRUE
  sysmeta <- dataone::updateSystemMetadata(mn, pid = pkg$resource_map, sysmeta = sysmeta)

  new_pkg <- publish_update(mn, pkg$metadata, pkg$resource_map, pkg$data)

  sysmeta <- dataone::getSystemMetadata(mn, new_pkg$metadata)
  expect_true(sysmeta@replicationAllowed)

  sysmeta <- dataone::getSystemMetadata(mn, new_pkg$resource_map)
  expect_true(sysmeta@replicationAllowed)
})


test_that("extra statements are maintained between updates", {
  pkg <- create_dummy_package(mn, 3)

  # Add some PROV triples to the Resource Map
  rm <- tempfile()
  writeLines(rawToChar(dataone::getObject(mn, pkg$resource_map)), rm)
  # statements <- data.frame(subject = paste0("https://cn.dataone.org/cn/v2/resolve/", URLencode(pkg$data[1], reserved = TRUE)),
  #                          predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
  #                          object = "http://www.w3.org/ns/prov#Entity")
  #
  # statements <- rbind(statements,
  #                     data.frame(subject = paste0("https://cn.dataone.org/cn/v2/resolve/", URLencode(pkg$data[2], reserved = TRUE)),
  #                                predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
  #                                object = "http://www.w3.org/ns/prov#Entity"))

  statements <- data.frame(subject = paste0("https://cn.dataone.org/cn/v2/resolve/", URLencode(pkg$data[1], reserved = TRUE)),
                           predicate = "http://www.w3.org/ns/prov#wasDerivedFrom",
                           object = paste0("https://cn.dataone.org/cn/v2/resolve/", URLencode(pkg$data[2], reserved = TRUE)))

  new_rm <- update_resource_map(mn, pkg$resource_map, pkg$metadata, pkg$data, other_statements = statements, public = TRUE)

  rm <- tempfile()
  writeLines(rawToChar(dataone::getObject(mn, new_rm)), rm)
  statements <- parse_resource_map(rm)
  expect_true("http://www.w3.org/ns/prov#wasDerivedFrom" %in% statements$predicate)


  new_new_rm <- update_resource_map(mn, new_rm, pkg$metadata, pkg$data, public = TRUE)
  rm <- tempfile()
  writeLines(rawToChar(dataone::getObject(mn, new_new_rm)), rm)
  statements <- parse_resource_map(rm)
  expect_true("http://www.w3.org/ns/prov#wasDerivedFrom" %in% statements$predicate)
})

