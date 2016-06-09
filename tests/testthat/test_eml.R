context("EML")

test_that("an EML otherEntity subtree can be created when the sysmeta has a filename", {
  skip("This test needs to be skipped until ecogrid URLs support DataONE objects.")

  x <- file.path(system.file("tests", "testfiles", package = "arcticdatautils"), "example-sysmeta.xml")
  doc <- XML::xmlParse(x)
  sysmeta <- new("SystemMetadata")
  sysmeta <- datapack::parseSystemMetadata(sysmeta, XML::xmlRoot(doc))

  other_entity <- sysmeta_to_entity(sysmeta)

  # Check some rough properties of the subtree
  expect_is(other_entity, "otherEntity")
  expect_equal(other_entity@entityName@.Data, "some_file.bin")
  expect_equal(other_entity@physical[[1]]@dataFormat@externallyDefinedFormat@formatName, "application/octet-stream")
})

test_that("an EML otherEntity subtree can be created when the sysmeta doesn't have a filename ", {
  skip("This test needs to be skipped until ecogrid URLs support DataONE objects.")

  x <- file.path(system.file("tests", "testfiles", package = "arcticdatautils"), "example-sysmeta-nofilename.xml")
  doc <- XML::xmlParse(x)
  sysmeta <- new("SystemMetadata")
  sysmeta <- datapack::parseSystemMetadata(sysmeta, XML::xmlRoot(doc))

  other_entity <- sysmeta_to_entity(sysmeta)

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
  creator <- eml_creator("test", "user", "test@email.com")

  expect_is(creator, "creator")
  expect_equal(creator@individualName[[1]]@givenName[[1]]@.Data, "test")
  expect_equal(creator@individualName[[1]]@surName@.Data, "user")
  expect_equal(creator@electronicMailAddress[[1]]@.Data, "test@email.com")
})

test_that("a contact can be created", {
  contact <- eml_contact("test", "user", "test@email.com")

  expect_is(contact, "contact")
  expect_equal(contact@individualName[[1]]@givenName[[1]]@.Data, "test")
  expect_equal(contact@individualName[[1]]@surName@.Data, "user")
  expect_equal(contact@electronicMailAddress[[1]]@.Data, "test@email.com")
})
