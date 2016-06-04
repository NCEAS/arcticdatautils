context("EML")

test_that("an EML otherEntity subtree can be created when the sysmeta has a filename", {
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
  doc@dataset@methods@methodStep[[1]]@description@section[[1]]@.Data[[1]]

  expect_true(XML::xmlValue(doc@dataset@methods@methodStep[[1]]@description@section[[1]]@.Data[[1]]) == "title")
  expect_true(XML::xmlValue(doc@dataset@methods@methodStep[[1]]@description@section[[1]]@.Data[[2]]) == "description")
})
