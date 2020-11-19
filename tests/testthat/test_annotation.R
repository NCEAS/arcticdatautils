test_that("annotations are added"){

  eml_path <- file.path(system.file(package = "arcticdatautils"), "example-eml.xml")

  doc <- EML::read_eml(eml_path)

  dummy_factors <- c("factor 1", "factor 2")
  dummy_attributes <- create_dummy_attributes_dataframe(10, dummy_factors)
  dummy_enumeratedDomain <- create_dummy_enumeratedDomain_dataframe(dummy_factors)

  dummy_attributeList <- EML::set_attributes(dummy_attributes, factors = dummy_enumeratedDomain)
  dummy_entityName <- "Test_Name"
  dummy_entityDescription <- "Test_Description"

  expect_true(OE$entityName == dummy_entityName)
  expect_true(OE$entityDescription == dummy_entityDescription)

  doc$dataset$dataTable <- DT
  expect_true(EML::eml_validate(doc))
}
