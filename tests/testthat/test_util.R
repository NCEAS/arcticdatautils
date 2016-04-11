#' test_util.R

test_that("identifiers can be extracted", {
  x <- file.path(system.file("tests", "data", package = "arcticdata"), "example-field-projects-file.xml")
  identifier <- extract_local_identifier("field-projects", x)
  expect_equal(identifier, "215.001")

  x <- file.path(system.file("tests", "data", package = "arcticdata"), "example-gateway-file.xml")
  identifier <- extract_local_identifier("gateway", x)
  expect_equal(identifier, "urn:x-wmo:md:org.aoncadis.www::d9330d2b-4174-11e3-8af4-00c0f03d5b7c")
})

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
  eml_file <- file.path(system.file("tests", "data", package = "arcticdata"), "example-eml.xml")
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
