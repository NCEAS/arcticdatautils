context("EML")

mn <- env_load()$mn

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

test_that("a personnel can be created", {
  personnel <- eml_personnel(given_names = "test", sur_name = "user", role = "principalInvestigator")

  expect_is(personnel, "personnel")
  expect_equal(personnel@individualName[[1]]@givenName[[1]]@.Data, "test")
  expect_equal(personnel@individualName[[1]]@surName@.Data, "user")
  expect_equal(personnel@role[[1]]@.Data, "principalInvestigator")
})

test_that("a project can be created", {
  test_personnel_1 <- eml_personnel(given_names = "A", sur_name = "User", organization = "NCEAS", role = "originator")

  project <- eml_project("some title",
                         list(test_personnel_1),
                         "This is a test abstract",
                         "I won an award, yay")

  expect_is(project, "project")
  expect_equal(project@title[[1]]@.Data, "some title")
  expect_equal(project@personnel[[1]]@individualName[[1]]@givenName[[1]]@.Data, "A")
  expect_equal(project@personnel[[1]]@individualName[[1]]@surName@.Data, "User")
  expect_equal(project@personnel[[1]]@organizationName[[1]]@.Data, "NCEAS")
  expect_equal(project@personnel[[1]]@role[[1]]@.Data, "originator")
  expect_equal(xml2::xml_text(project@funding@para[[1]]@.Data[[1]]), "I won an award, yay")
})

test_that("a project can be created with multiple personnel, an abstract can be created with multiple paragraphs, awards with multiple awards", {
  test_personnel_1 <- eml_personnel(given_names = "A", sur_name = "User", organization = "NCEAS", role = "originator")
  test_personnel_2 <- eml_personnel(given_names = "Testy", sur_name = "Mactesterson", organization = "A Test Org", role = c("user", "author"))

  project <- eml_project("some title",
                         list(test_personnel_1, test_personnel_2),
                         c("This is a test abstract", "This is the second paragraph"),
                         c("I won an award, yay", "I won a second award, wow"))

  expect_is(project, "project")
  expect_equal(project@title[[1]]@.Data, "some title")
  expect_equal(project@personnel[[2]]@individualName[[1]]@givenName[[1]]@.Data, "Testy")
  expect_equal(project@personnel[[2]]@individualName[[1]]@surName@.Data, "Mactesterson")
  expect_equal(project@personnel[[2]]@organizationName[[1]]@.Data, "A Test Org")
  expect_equal(project@personnel[[2]]@role[[2]]@.Data, "author")
  expect_equal(xml2::xml_text(project@abstract@para[[2]]@.Data[[1]]), "This is the second paragraph")
  expect_equal(xml2::xml_text(project@funding@para[[2]]@.Data[[1]]), "I won a second award, wow")
})

test_that("a dataTable and otherEntity can be added from a pid", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  data_path <- tempfile()
  writeLines(LETTERS, data_path)
  pid <- publish_object(mn, data_path, "text/csv")

  eml_path <- file.path(system.file(package = "arcticdatautils"), "example-eml.xml")

  doc <- EML::read_eml(eml_path)

  dummy_factors <- c("factor 1", "factor 2")
  dummy_attributes <- create_dummy_attributes_dataframe(10, dummy_factors)
  dummy_enumeratedDomain <- create_dummy_enumeratedDomain_dataframe(dummy_factors)

  dummy_attributeList <- EML::set_attributes(dummy_attributes, factors = dummy_enumeratedDomain)
  dummy_entityName <- "Test_Name"
  dummy_entityDescription <- "Test_Description"

  # Create an otherEntity
  OE <- pid_to_eml_entity(mn, pid,
                    entityName = dummy_entityName,
                    entityDescription = dummy_entityDescription,
                    attributeList = dummy_attributeList)
  expect_s4_class(OE, "otherEntity")
  expect_true(slot(OE, "entityName") == dummy_entityName)
  expect_true(slot(OE, "entityDescription") == dummy_entityDescription)

  # Create a dataTable
  DT <- pid_to_eml_entity(mn, pid,
                          entityType = "dataTable",
                          entityName = dummy_entityName,
                          entityDescription = dummy_entityDescription,
                          attributeList = dummy_attributeList)
  expect_s4_class(DT, "dataTable")
  expect_true(slot(DT, "entityName") == dummy_entityName)
  expect_true(slot(DT, "entityDescription") == dummy_entityDescription)

  doc@dataset@otherEntity[[1]] <- OE
  expect_true(EML::eml_validate(doc))

  doc@dataset@dataTable[[1]] <- DT
  expect_true(EML::eml_validate(doc))

  unlink(data_path)
})

test_that("eml_otherEntity_to_dataTable fails gracefully", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  eml <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))

  # incorrect inputs
  expect_error(eml_otherEntity_to_dataTable("dummy input"))
  expect_error(eml_otherEntity_to_dataTable(eml, "1"))

  # subscripts out of bounds
  expect_error(eml_otherEntity_to_dataTable(eml, eml@dataset@otherEntity[[2]]))
  expect_error(eml_otherEntity_to_dataTable(eml, 2))

  # Duplicate entityNames found
  eml@dataset@otherEntity[[2]] <- eml@dataset@otherEntity[[1]]
  expect_error(eml_otherEntity_to_dataTable(eml, eml@dataset@otherEntity[[1]]))

})

test_that("eml_otherEntity_to_dataTable fails gracefully", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  eml <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))
  otherEntity <- eml@dataset@otherEntity[[1]]

  eml <- eml_otherEntity_to_dataTable(eml, eml@dataset@otherEntity[[1]])

  # test that otherEntity was removed
  expect_length(eml@dataset@otherEntity, 0)

  # test that dataTable was added
  expect_equal(otherEntity@entityName, eml@dataset@dataTable[[1]]@entityName)
  expect_equivalent(otherEntity@physical, eml@dataset@dataTable[[1]]@physical)
})

test_that("which_in_eml returns correct locations", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  attributes <-
    data.frame(
      attributeName = c(
        "length_1",
        "time_2",
        "length_3"),
      attributeDefinition = c(
        "def 1",
        "def 2",
        "def 3"),
      formatString = c(
        NA,
        NA,
        NA),
      measurementScale = c(
        "ratio",
        "ratio",
        "ratio"),
      domain = c(
        "numericDomain",
        "numericDomain",
        "numericDomain"),
      definition = c(
        NA,
        NA,
        NA),
      unit = c(
        "meter",
        "second",
        "meter"),
      numberType = c(
        "real",
        "real",
        "real"),
      stringsAsFactors = FALSE
    )

  attributeList <- EML::set_attributes(attributes)

  dataTable_1 <- new("dataTable",
                     entityName = "2016_data.csv",
                     entityDescription = "2016 data",
                     attributeList = attributeList)

  dataTable_2 <- dataTable_1

  dataTable_3 <- new("dataTable",
                     entityName = "2015_data.csv",
                     entityDescription = "2016 data",
                     attributeList = attributeList)

  creator_1 <- new("creator",
                   individualName = new("individualName",
                                        surName = "LAST",
                                        givenName = "FIRST"))
  creator_2 <- new("creator",
                   individualName = new("individualName",
                                        surName = "LAST",
                                        givenName = "FIRST_2"))
  creator_3 <- creator_2

  title <- "Title"

  dataset <- new("dataset",
                 title = title,
                 creator = c(creator_1, creator_2, creator_3),
                 dataTable = c(dataTable_1, dataTable_2, dataTable_3))

  eml <- new("eml",
             dataset = dataset)

  expect_equal(c(2,3), which_in_eml(eml@dataset@creator, "givenName", "FIRST_2"))
  expect_error(which_in_eml(eml@dataset@dataTable, "attributeName", "length_3"))
  expect_equal(c(1,3), which_in_eml(eml@dataset@dataTable[[1]]@attributeList@attribute, "attributeName", function(x) {grepl("^length", x)}))
})

test_that('eml_set_reference sets a reference', {
  eml_path <- file.path(system.file(package = "arcticdatautils"), "example-eml.xml")
  doc <- EML::read_eml(eml_path)

  expect_error(eml_set_reference(doc@dataset@creator[[1]], doc@dataset@contact[[1]]))

  # Add id to use references
  doc@dataset@creator[[1]]@id <- new('xml_attribute', 'creator_id')
  doc@dataset@contact[[1]] <- eml_set_reference(doc@dataset@creator[[1]], doc@dataset@contact[[1]])

  expect_equal(doc@dataset@creator[[1]]@id[1], doc@dataset@contact[[1]]@references[1])
  expect_true(EML::eml_validate(doc))
})

test_that('eml_set_shared_attributes creates shared attribute references', {
  eml_path <- file.path(system.file(package = 'arcticdatautils'), 'example-eml.xml')
  doc <- EML::read_eml(eml_path)

  attributes <- data.frame(attributeName = 'length_1', attributeDefinition = 'def1',
                           formatString = NA, measurementScale = 'ratio', domain = 'numericDomain',
                           definition = NA, unit = 'meter', numberType = 'real',
                           stringsAsFactors = FALSE)
  attributeList <- EML::set_attributes(attributes)

  dataTable_1 <- new('dataTable',
                     entityName = '2016_data.csv',
                     entityDescription = '2016 data',
                     attributeList = attributeList)
  dataTable_2 <- dataTable_1
  doc@dataset@dataTable <- c(dataTable_1, dataTable_2)

  doc <- eml_set_shared_attributes(doc)

  expect_equal(doc@dataset@dataTable[[1]]@id[1], doc@dataset@dataTable[[2]]@references[1])
  expect_true(EML::eml_validate(doc))
})

test_that('eml_party creates multiple givenName, organizationName, and positionName fields', {
  creator <- eml_party('creator', c('John', 'and Jack'), 'Smith', c('NCEAS', 'UCSB'),
                       c('Programmers', 'brothers'))

  expect_is(creator, "creator")
  expect_equal(unlist(EML::eml_get(creator, 'givenName')), c('John', 'and Jack'))
  expect_equal(unlist(EML::eml_get(creator, 'organizationName')), c('NCEAS', 'UCSB'))
  expect_equal(unlist(EML::eml_get(creator, 'positionName')), c('Programmers', 'brothers'))
})
