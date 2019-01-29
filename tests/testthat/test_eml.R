context("EML")

mn <- env_load()$mn

test_that("a creator can be created", {
  creator <- eml_creator("test", "user")

  expect_equal(creator$individualName$givenName, "test")
  expect_equal(creator$individualName$surName, "user")
})

test_that("a contact can be created", {
  contact <- eml_contact("test", "user")

  expect_is(contact, "contact")
  expect_equal(contact$individualName$givenName, "test")
  expect_equal(contact$individualName$surName, "user")
})

test_that("a personnel can be created", {
  personnel <- eml_personnel(given_names = "test", sur_name = "user", role = "principalInvestigator", userId = "https://orcid.org/WWWW-XXXX-YYYY-ZZZZ")

  expect_is(personnel, "personnel")
  expect_equal(personnel$individualName$givenName, "test")
  expect_equal(personnel$individualName$surName, "user")
  expect_equal(personnel$role, "principalInvestigator")
})

test_that("a project can be created", {
  test_personnel_1 <- eml_personnel(given_names = "A", sur_name = "User", organization = "NCEAS", role = "originator")

  project <- eml_project("some title",
                         list(test_personnel_1),
                         "This is a test abstract",
                         "I won an award, yay")

  expect_equal(project$title[[1]], "some title")
  expect_equal(project$personnel[[1]]$individualName[[1]]$givenName[[1]], "A")
  expect_equal(project$personnel[[1]]$individualName[[1]]$surName, "User")
  expect_equal(project$personnel[[1]]$organizationName[[1]], "NCEAS")
  expect_equal(project$personnel[[1]]$role[[1]], "originator")
  expect_equal(project$funding$para[[1]], "I won an award, yay")
})

test_that("a project can be created with multiple personnel, an abstract can be created with multiple paragraphs, awards with multiple awards", {
  test_personnel_1 <- eml_personnel(given_names = "A", sur_name = "User", organization = "NCEAS", role = "originator")
  test_personnel_2 <- eml_personnel(given_names = "Testy", sur_name = "Mactesterson", organization = "A Test Org", role = list("user", "author"))

  project <- eml_project("some title",
                         list(test_personnel_1, test_personnel_2),
                         list("This is a test abstract", "This is the second paragraph"),
                         list("I won an award, yay", "I won a second award, wow"))

  expect_equal(project$title[[1]], "some title")
  expect_equal(project$personnel[[2]]$individualName[[1]]$givenName[[1]], "Testy")
  expect_equal(project$personnel[[2]]$individualName[[1]]$surName, "Mactesterson")
  expect_equal(project$personnel[[2]]$organizationName[[1]], "A Test Org")
  expect_equal(project$personnel[[2]]$role[[2]], "author")
  expect_equal(project$abstract$para[[2]], "This is the second paragraph")
  expect_equal(project$funding$para[[2]], "I won a second award, wow")
})

test_that("a dataTable and otherEntity can be added from a pid", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  data_path <- tempfile()
  writeLines(LETTERS, data_path)
  pid1 <- publish_object(mn, data_path, "text/csv")
  pid2 <- publish_object(mn, data_path, "text/csv")

  eml_path <- file.path(system.file(package = "arcticdatautils"), "example-eml.xml")

  doc <- EML::read_eml(eml_path)

  dummy_factors <- c("factor 1", "factor 2")
  dummy_attributes <- create_dummy_attributes_dataframe(10, dummy_factors)
  dummy_enumeratedDomain <- create_dummy_enumeratedDomain_dataframe(dummy_factors)

  dummy_attributeList <- EML::set_attributes(dummy_attributes, factors = dummy_enumeratedDomain)
  dummy_entityName <- "Test_Name"
  dummy_entityDescription <- "Test_Description"

  # Create an otherEntity
  OE <- pid_to_eml_entity(mn, pid1,
                    entityName = dummy_entityName,
                    entityDescription = dummy_entityDescription,
                    attributeList = dummy_attributeList)

  expect_true(OE$entityName == dummy_entityName)
  expect_true(OE$entityDescription == dummy_entityDescription)

  # Create a dataTable
  DT <- pid_to_eml_entity(mn, pid2,
                          entityType = "dataTable",
                          entityName = dummy_entityName,
                          entityDescription = dummy_entityDescription,
                          attributeList = dummy_attributeList)

  expect_true(DT$entityName == dummy_entityName)
  expect_true(DT$entityDescription == dummy_entityDescription)

  doc$dataset$otherEntity <- OE
  expect_true(EML::eml_validate(doc))

  doc$dataset$dataTable <- DT
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
  expect_error(eml_otherEntity_to_dataTable(eml, eml$dataset$otherEntity[[2]]))
  expect_error(eml_otherEntity_to_dataTable(eml, 2))

  # Duplicate entityNames found
  eml$dataset$otherEntity[[2]] <- eml$dataset$otherEntity[[1]]
  expect_error(eml_otherEntity_to_dataTable(eml, eml$dataset$otherEntity[[1]]))

})

test_that("eml_otherEntity_to_dataTable fails gracefully", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  eml <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))
  otherEntity <- eml$dataset$otherEntity[[1]]

  eml <- eml_otherEntity_to_dataTable(eml, eml$dataset$otherEntity[[1]])

  # test that otherEntity was removed
  expect_length(eml$dataset$otherEntity, 0)

  # test that dataTable was added
  expect_equal(otherEntity$entityName, eml$dataset$dataTable[[1]]$entityName)
  expect_equivalent(otherEntity$physical, eml$dataset$dataTable[[1]]$physical)
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

  dataTable_1 <- eml$dataTable(
                     entityName = "2016_data.csv",
                     entityDescription = "2016 data",
                     attributeList = attributeList)

  dataTable_2 <- dataTable_1

  dataTable_3 <- eml$dataTable(
                     entityName = "2015_data.csv",
                     entityDescription = "2016 data",
                     attributeList = attributeList)

  creator_1 <- eml$creator(
                   individualName = eml$individualName(
                                        surName = "LAST",
                                        givenName = "FIRST"))
  creator_2 <- eml$creator(
    individualName = eml$individualName(
                                        surName = "LAST",
                                        givenName = "FIRST_2"))
  creator_3 <- creator_2

  title <- "Title"

  dataset <- eml$dataset(
                 title = title,
                 creator = list(creator_1, creator_2, creator_3),
                 dataTable = list(dataTable_1, dataTable_2, dataTable_3))

  doc <- list(dataset = dataset)

  expect_equal(c(2,3), which_in_eml(doc$dataset$creator, "givenName", "FIRST_2"))
  expect_error(which_in_eml(doc$dataset$dataTable, "attributeName", "length_3"))
  expect_equal(c(1,3), which_in_eml(doc$dataset$dataTable[[1]]$attribute, "attributeName", function(x) {grepl("^length", x)}))
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
