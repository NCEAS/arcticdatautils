context("Editing and managing data packages")

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

  doc <- EML::read_eml(eml_path)
  # Populate dummy access element
  doc$access <- list(allow  = "hello")
  write_eml(doc, eml_path)

  new_pids <- publish_update(mn, pids$metadata, pids$resource_map, metadata_path = eml_path)
  updated_eml_path <- tempfile(fileext = ".xml")
  writeBin(dataone::getObject(mn, new_pids$metadata), updated_eml_path)

  new_eml <- EML::read_eml(updated_eml_path)
  expect_equal(0, length(new_eml$access$allow))
})

test_that("publishing an object with a valid format ID succeeds", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  tmp_path <- tempfile()
  writeLines(LETTERS, tmp_path)

  expect_is(publish_object(mn, tmp_path, "text/plain"), "character")
})

test_that("publishing an object with an invalid format ID fails", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  tmp_path <- tempfile()
  writeLines(LETTERS, tmp_path)

  expect_error(publish_object(mn, tmp_path, "asdf/asdf"))
})

test_that("publish_update removes 'resource_map_pid' from 'parent_child_pids' argument", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  parent <- create_dummy_package(mn)
  child <- create_dummy_package(mn)

  # Nest packages
  parent["resource_map"] <- update_resource_map(mn,
                                                parent$resource_map,
                                                parent$metadata,
                                                parent$data,
                                                child$resource_map,
                                                check_first = F)

  # Updating parent incorrectly should still run (with parent resource_map listed in 'parent_parent_pids')
  child <- publish_update(mn,
                           child$metadata,
                           child$resource_map,
                           child$data,
                           parent_resmap_pid = parent$resource_map,
                           parent_metadata_pid = parent$metadata,
                           parent_data_pids = parent$data,
                           parent_child_pids = child$resource_map, check_first = F)
  parent <- get_package(mn, child$parent_resource_map)

  expect_equal(child$resource_map, parent$child_packages)
})

test_that("publish_update errors if the non-current resource map or metadata pid is provided", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg1 <- create_dummy_package(mn)
  pkg2 <- publish_update(mn, pkg1$metadata, pkg1$resource_map, pkg1$data)

  expect_error(publish_update(mn, pkg1$metadata, pkg2$resource_map, pkg2$data))
  expect_error(publish_update(mn, pkg2$metadata, pkg1$resource_map, pkg2$data))
})


test_that("update_physical works", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg <- create_dummy_package_full(mn, title = "Update physical check")

  file.create("dummy_object.csv")

  new_data_pid <- update_object(mn,
                                pid = pkg$data[2],
                                path = "dummy_object.csv",
                                format_id = "text/csv")

  unlink("dummy_object.csv")

  pkg_new <- publish_update(mn,
                            resource_map_pid = pkg$resource_map,
                            metadata_pid = pkg$metadata,
                            data_pids = c(pkg$data[-2], new_data_pid))

  eml_original <- EML::read_eml(rawToChar(dataone::getObject(mn, pkg$metadata)))

  eml_new <- update_physical(eml_original,
                             mn,
                             data_pid = pkg$data[2],
                             new_data_pid = new_data_pid)

  t <- tempfile()
  write_eml(eml_new, t)
  eml_new <- read_eml(t)

  url_original <- eml_get(eml_original, "url") %>% grep("^http", ., value = T) %>% unname()
  url_new <- eml_get(eml_new, "url") %>% grep("^http", ., value = T) %>% unname()

  expect_equal(sum(stringr::str_detect(url_original, pkg$data[1])), 1)
  expect_equal(sum(stringr::str_detect(url_original, pkg$data[2])), 1)
  expect_equal(sum(stringr::str_detect(url_original, pkg$data[3])), 1)
  expect_equal(sum(stringr::str_detect(url_original, pkg$data[4])), 1)

  expect_equal(sum(stringr::str_detect(url_new, new_data_pid)), 1)
  expect_equal(sum(stringr::str_detect(url_new, pkg$data[1])), 1)
  expect_equal(sum(stringr::str_detect(url_new, pkg$data[2])), 0)
  expect_equal(sum(stringr::str_detect(url_new, pkg$data[3])), 1)
  expect_equal(sum(stringr::str_detect(url_new, pkg$data[4])), 1)
})

test_that("update_package_object changes specified data object and rest of package is intact", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg <- create_dummy_package_full(mn, title = "Check update_package_object")

  new_data_path <- "test_file.csv"
  file.create(new_data_path)

  data_pid <- pkg$data[2]

  pkg_new <- update_package_object(mn,
                                   data_pid = data_pid,
                                   new_data_path = new_data_path,
                                   resource_map_pid = pkg$resource_map,
                                   format_id = "text/csv")

  file.remove(new_data_path)

  # test: other objects are retained
  expect_equal(all(pkg$data[-2] %in% pkg_new$data), TRUE)

  # test: metadata changes
  expect_false(pkg$metadata == pkg_new$metadata)

  # test: new data PID is a version of old data PID
  versions <- get_all_versions(mn, data_pid)
  latest_version <- versions[length(versions)]

  new_data_pid <- pkg_new$data[!pkg_new$data %in% pkg$data]

  expect_equal(latest_version, new_data_pid)

  # test: EML is updated
  eml_original <- EML::read_eml(rawToChar(dataone::getObject(mn, pkg$metadata)))

  eml_new <- EML::read_eml(rawToChar(dataone::getObject(mn, pkg_new$metadata)))

  url_original <- eml_get(eml_original, "url") %>% grep("^http", ., value = T) %>% unname()
  url_new <- eml_get(eml_new, "url") %>% grep("^http", ., value = T) %>% unname()

  expect_true(url_original[2] != url_new[2])
  expect_equal(url_original[1], url_new[1])
  expect_equal(url_original[3], url_new[3])
  expect_equal(url_original[4], url_new[4])

  expect_true(stringr::str_detect(url_new[2], new_data_pid))
})

test_that("update_package_object errors if wrong input", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  file_path <- tempfile(fileext = ".csv")

  expect_error(update_package_object(LETTERS,
                                     data_pid = file_path,
                                     new_data_path = "something",
                                     rm_pid = "something"))

  expect_error(update_package_object(mn,
                                     data_pid = c(1, 2),
                                     new_data_path = "something",
                                     rm_pid = "something"))

  expect_error(update_package_object(mn,
                                     data_pid = "something",
                                     new_data_path = "something",
                                     rm_pid = "something"))

  expect_error(update_package_object(mn,
                                     data_pid = "something",
                                     new_data_path = TRUE,
                                     rm_pid = "something"))

  expect_error(update_package_object(mn,
                                     data_pid = file_path,
                                     new_data_path = "something",
                                     rm_pid = 1))
  unlink(file_path)
})

test_that("update_package_object updates EML", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg <- create_dummy_package(mn, size = 4)

  attributes1 <- data.frame(
    attributeName = c("col1", "col2"),
    attributeDefinition = c("Numbers", "Letters in the alphabet"),
    measurementScale = c("ratio", "nominal"),
    domain = c("numericDomain", "textDomain"),
    formatString = c(NA, NA),
    definition = c(NA, "ABCDEFG..."),
    unit = c("dimensionless", NA),
    numberType = c("integer", NA),
    missingValueCode = c(NA, NA),
    missingValueCodeExplanation = c(NA, NA),
    stringsAsFactors = FALSE)

  attributeList1 <- EML::set_attributes(attributes1)
  phys <- pid_to_eml_physical(mn, pkg$data[1])

  dummy_data_table <- list(entityName = "Dummy Data Table",
                          entityDescription = "Dummy Description",
                          physical = phys,
                          attributeList = attributeList1)

  doc <- EML::read_eml(rawToChar(getObject(mn, pkg$metadata)))
  doc$dataset$dataTable <- dummy_data_table

  otherEnts <- list(pid_to_eml_entity(mn, pkg$data[2], entityType = "otherEntity"),
                    pid_to_eml_entity(mn, pkg$data[3], entityType = "otherEntity"))
  doc$dataset$otherEntity <- otherEnts

  eml_path <- tempfile(fileext = ".xml")
  EML::write_eml(doc, eml_path)

  pkg <- publish_update(mn,
                        metadata_pid = pkg$metadata,
                        resource_map_pid = pkg$resource_map,
                        data_pids = pkg$data,
                        metadata_path = eml_path,
                        public = TRUE,
                        use_doi = FALSE)

  dummy_data <- data.frame(col1 = 1:26, col2 = letters)
  new_data_path <- tempfile(fileext = ".csv")
  write.csv(dummy_data, new_data_path, row.names = FALSE)

  data_pid <- pkg$data[1]

  pkg_new <- update_package_object(mn,
                                   data_pid,
                                   new_data_path,
                                   pkg$resource_map,
                                   format_id = "text/csv",
                                   public = TRUE,
                                   use_doi = FALSE)

  doc <- read_eml(getObject(mn, pkg$metadata))
  url_initial <- eml_get(doc, "url") %>% grep("^http", ., value = T) %>% unname()
  expect_equal(sum(stringr::str_count(url_initial, data_pid)), 1)

  eml_new <- EML::read_eml(rawToChar(getObject(mn, pkg_new$metadata)))
  url_final <- eml_get(eml_new, "url") %>% grep("^http", ., value = T) %>% unname()
  expect_equal(sum(stringr::str_count(url_final, data_pid)), 0)

  pid_matches <- lapply(seq_along(pkg_new$data),
                        function(i) {stringr::str_count(url_final, pkg_new$data[i])})

  # confirm that URLs have a matching PID
  # if new PID corresponds to a dataset that had a dataTable/otherEntity
  # and has not been updated, expect_equal will error
  expect_equal(sum(unlist(pid_matches)),
               length(url_final))
})
