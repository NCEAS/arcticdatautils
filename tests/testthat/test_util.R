#' test_util.R

test_that("identifiers can be extracted", {
  x <- file.path(system.file("tests", "data", package = "arcticdata"), "example-field-projects-file.xml")
  identifier <- extract_local_identifier("field-projects", x)
  expect_equal(identifier, "215.001")

  x <- file.path(system.file("tests", "data", package = "arcticdata"), "example-gateway-file.xml")
  identifier <- extract_local_identifier("gateway", x)
  expect_equal(identifier, "urn:x-wmo:md:org.aoncadis.www::d9330d2b-4174-11e3-8af4-00c0f03d5b7c")
})
