#' test_util.R

context("util")

test_that("paths can be joined", {
  expect_equal(path_join(""), "")
  expect_equal(path_join(1), "1")
  expect_equal(path_join(NULL), "")
  expect_equal(path_join(c("./", "./test")), "./test")

  # Test that it handles variables and not just literal values
  part_one <- "./"
  part_two <- "./asdf"
  expect_equal(path_join(c(part_one, part_two)), "./asdf")
  expect_equal(path_join(c(part_one, "./", part_two)), "./asdf")

  # Other tests
  expect_equal(path_join("~/src/arcticdata./inst/asdf"), "~/src/arcticdata/inst/asdf")
})

test_that("a string can be added to a file", {
  # Prepare a temp file with an example EML doc
  eml_file <- file.path(system.file("tests", "testfiles", package = "arcticdatautils"), "example-eml.xml")
  tmp <- tempfile()
  file.copy(eml_file, tmp)

  # Get original title
  doc_pre <- XML::xmlParseDoc(tmp)
  title_pre <- XML::xmlValue(XML::getNodeSet(doc_pre, "//dataset/title")[[1]])

  # Add the string
  add_string_to_title(tmp, " a test")

  # Get the updated title
  doc_post <- XML::xmlParseDoc(tmp)
  title_post <- XML::xmlValue(XML::getNodeSet(doc_post, "//dataset/title")[[1]])

  expect_equal(paste0(title_pre, " a test"), title_post)

  # Clean up
  file.remove(tmp)
})

test_that("a package id can be changed", {
  library(EML)

  eml_file <- file.path(system.file("tests", "testfiles", package = "arcticdatautils"), "example-eml.xml")
  tmp <- tempfile()
  file.copy(eml_file, tmp)

  replace_package_id(tmp, "new_package_id")

  doc <- read_eml(tmp)
  expect_equal(as.character(doc@packageId), "new_package_id")

  # Clean up
  file.remove(tmp)
})
