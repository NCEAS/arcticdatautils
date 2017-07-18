context("EML")

mn <- env_load()$mn

test_that("an EML otherEntity subtree can be created when the sysmeta has a filename", {
  x <- file.path(system.file("tests", "testfiles", package = "arcticdatautils"), "example-sysmeta.xml")
  doc <- XML::xmlParse(x)
  sysmeta <- new("SystemMetadata")
  sysmeta <- datapack::parseSystemMetadata(sysmeta, XML::xmlRoot(doc))

  other_entity <- sysmeta_to_eml_other_entity(sysmeta)[[1]]

  # Check some rough properties of the subtree
  expect_is(other_entity, "otherEntity")
  expect_equal(other_entity@entityName@.Data, "some_file.bin")
  expect_equal(other_entity@physical[[1]]@dataFormat@externallyDefinedFormat@formatName, "application/octet-stream")
})

test_that("an EML otherEntity subtree can be created when the sysmeta doesn't have a filename ", {
  x <- file.path(system.file("tests", "testfiles", package = "arcticdatautils"), "example-sysmeta-nofilename.xml")
  doc <- XML::xmlParse(x)
  sysmeta <- new("SystemMetadata")
  sysmeta <- datapack::parseSystemMetadata(sysmeta, XML::xmlRoot(doc))

  other_entity <- sysmeta_to_eml_other_entity(sysmeta)[[1]]

  # Check some rough properties of the subtree
  expect_is(other_entity, "otherEntity")
  expect_equal(other_entity@entityName@.Data, "NA")
  expect_equal(other_entity@physical[[1]]@dataFormat@externallyDefinedFormat@formatName, "application/octet-stream")
})

test_that("a methods step can be added to an EML document", {
  library(XML)
  library(EML)

  doc <- new("eml")
  doc <- add_methods_step(doc, "title", "description")

  expect_equal(XML::xmlValue(doc@dataset@methods@methodStep[[1]]@description@section[[1]]@.Data[[1]]), "title")
  expect_equal(XML::xmlValue(doc@dataset@methods@methodStep[[1]]@description@section[[1]]@.Data[[2]]), "description")
})

test_that("multiple method steps can be added to an EML document", {
  library(XML)
  library(EML)

  doc <- new("eml")
  doc <- add_methods_step(doc, "title", "description")
  doc <- add_methods_step(doc, "another", "method")

  expect_length(doc@dataset@methods@methodStep, 2)
})

test_that("methods can be cleared from an EML document", {
  library(EML)

  doc <- new("eml")
  doc <- add_methods_step(doc, "title", "description")

  expect_length(doc@dataset@methods@methodStep, 1)

  doc <- clear_methods(doc)
  expect_length(doc@dataset@methods@methodStep, 0)
})

test_that("a creator can be created", {
  creator <- eml_creator("test", "user")

  expect_is(creator, "creator")
  expect_equal(creator@individualName[[1]]@givenName[[1]]@.Data, "test")
  expect_equal(creator@individualName[[1]]@surName@.Data, "user")
})

test_that("a contact can be created", {
  contact <- eml_contact("test", "user")

  expect_is(contact, "contact")
  expect_equal(contact@individualName[[1]]@givenName[[1]]@.Data, "test")
  expect_equal(contact@individualName[[1]]@surName@.Data, "user")
})


test_that("a project can be created", {
  project <- eml_project("some title", "12345", "a", "user")

  expect_is(project, "project")
  expect_equal(project@title[[1]]@.Data, "some title")
  expect_equal(project@personnel[[1]]@individualName[[1]]@givenName[[1]]@.Data, "a")
  expect_equal(project@personnel[[1]]@individualName[[1]]@surName@.Data, "user")
  expect_equal(xml2::xml_text(project@funding@para[[1]]@.Data[[1]]), "12345")
})


test_that("a project can be created with multiple awards", {
  project <- eml_project("some title", c("12345", "54321"), "a", "user")

  expect_length(project@funding@para, 2)
  expect_equal(xml2::xml_text(project@funding@para[[1]]@.Data[[1]]), "12345")
  expect_equal(xml2::xml_text(project@funding@para[[2]]@.Data[[1]]), "54321")
})

test_that("a project can be created with multiple organizations", {
  project <- eml_project("some title", "12345", "a", "user", organizations = c("org1", "org2"))

  expect_length(project@personnel[[1]]@organizationName, 2)
  expect_equal(project@personnel[[1]]@organizationName[[1]]@.Data, "org1")
  expect_equal(project@personnel[[1]]@organizationName[[2]]@.Data, "org2")
})

test_that("an other entity can be added from a pid", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  data_path <- tempfile()
  writeLines(LETTERS, data_path)
  pid <- publish_object(mn, data_path, "text/plain")

  eml_path <- file.path(system.file("inst", package = "arcticdatautils"), "example-eml.xml")

  doc <- EML::read_eml(eml_path)
  doc@dataset@otherEntity <- new("ListOfotherEntity", list())

  set_other_entities(mn, eml_path, pid)

  doc <- EML::read_eml(eml_path)
  testthat::expect_length(doc@dataset@otherEntity, 1)

  unlink(data_path)
})
