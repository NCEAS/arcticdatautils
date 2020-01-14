context("EML")

mn <- env_load()$mn

test_that("a creator can be created", {
  creator <- eml_creator("tester", "user")

  expect_equal(creator$individualName$givenName, "tester")
  expect_equal(creator$individualName$surName, "user")
})

test_that("a contact can be created", {
  contact <- eml_contact("test", "user")

  expect_equal(contact$individualName$givenName, "test")
  expect_equal(contact$individualName$surName, "user")
})

test_that("a personnel can be created", {
  personnel <- eml_personnel(given_names = "test", sur_name = "user", role = "principalInvestigator", userId = "https://orcid.org/WWWW-XXXX-YYYY-ZZZZ")

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

  expect_equal(project$title, "some title")
  expect_equal(project$personnel[[1]]$individualName$givenName, "A")
  expect_equal(project$personnel[[1]]$individualName$surName, "User")
  expect_equal(project$personnel[[1]]$organizationName, "NCEAS")
  expect_equal(project$personnel[[1]]$role, "originator")
  expect_equal(project$funding$para, "I won an award, yay")
})

test_that("a project can be created with multiple personnel, an abstract can be created with multiple paragraphs, awards with multiple awards", {
  test_personnel_1 <- eml_personnel(given_names = "A", sur_name = "User", organization = "NCEAS", role = "originator")
  test_personnel_2 <- eml_personnel(given_names = "Testy", sur_name = "Mactesterson", organization = "A Test Org", role = list("user", "author"))

  project <- eml_project("some title",
                         list(test_personnel_1, test_personnel_2),
                         list("This is a test abstract", "This is the second paragraph"),
                         list("I won an award, yay", "I won a second award, wow"))

  expect_equal(project$title, "some title")
  expect_equal(project$personnel[[2]]$individualName$givenName, "Testy")
  expect_equal(project$personnel[[2]]$individualName$surName, "Mactesterson")
  expect_equal(project$personnel[[2]]$organizationName, "A Test Org")
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
                          entity_type = "dataTable",
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

  doc <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))

  # incorrect inputs
  expect_error(eml_otherEntity_to_dataTable("dummy input"))
  expect_error(eml_otherEntity_to_dataTable(doc, "1"))

  # subscripts out of bounds
  expect_error(eml_otherEntity_to_dataTable(doc, 2))

  # Duplicate entityNames found
  doc$dataset$otherEntity <- list(doc$dataset$otherEntity, doc$dataset$otherEntity)
  expect_error(eml_otherEntity_to_dataTable(doc, 1))

})

test_that("eml_otherEntity_to_dataTable works", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  doc <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))
  doc$dataset$otherEntity$attributeList <- EML::set_attributes(create_dummy_attributes_dataframe(1))
  otherEntity <- doc$dataset$otherEntity

  doc <- eml_otherEntity_to_dataTable(doc, 1)

  # test that otherEntity was removed
  expect_length(doc$dataset$otherEntity, 0)

  # test that dataTable was added
  expect_equal(otherEntity$entityName, doc$dataset$dataTable$entityName)
  expect_equivalent(otherEntity$physical, doc$dataset$dataTable$physical)
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

  dataTable_1 <- list(
                     entityName = "2016_data.csv",
                     entityDescription = "2016 data",
                     attributeList = attributeList)

  dataTable_2 <- dataTable_1

  dataTable_3 <- list(
                     entityName = "2015_data.csv",
                     entityDescription = "2016 data",
                     attributeList = attributeList)

  creator_1 <- list(
                   individualName = list(individualName = list(
                                        surName = "LAST",
                                        givenName = "FIRST")))
  creator_2 <- list(
                  individualName = list(individualName = list(
                                        surName = "LAST",
                                        givenName = "FIRST_2")))
  creator_3 <- creator_2

  title <- "Title"

  dataset <- list(dataset = list(
                 title = title,
                 creator = list(creator_1, creator_2, creator_3),
                 dataTable = list(dataTable_1, dataTable_2, dataTable_3)))

  doc <- dataset

  expect_equal(c(2,3), which_in_eml(doc$dataset$creator, "givenName", "FIRST_2"))
  expect_error(which_in_eml(doc$dataset$dataTable, "attributeName", "length_3")) # not sure why this should fail?
  expect_equal(c(1,3), which_in_eml(doc$dataset$dataTable[[1]]$attribute, "attributeName", function(x) {grepl("^length", x)}))
})

test_that('eml_set_reference sets a reference', {
  eml_path <- file.path(system.file(package = "arcticdatautils"), "example-eml.xml")
  doc <- EML::read_eml(eml_path)

  expect_error(eml_set_reference(doc$dataset$creator, doc$dataset$contact))

  # Add id to use references
  doc$dataset$creator$id <- 'creator_id'
  doc$dataset$contact <- eml_set_reference(doc$dataset$creator, doc$dataset$contact)

  expect_equal(doc$dataset$creator$id, doc$dataset$contact$references)
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

  dataTable_1 <- list(entityName = '2016_data.csv',
                     entityDescription = '2016 data',
                     attributeList = attributeList)
  dataTable_2 <- dataTable_1
  doc$dataset$dataTable <- list(dataTable_1, dataTable_2)

  doc <- eml_set_shared_attributes(doc)

  expect_equal(doc$dataset$dataTable[[1]]$attributeList$id, doc$dataset$dataTable[[2]]$attributeList$references)
  expect_true(EML::eml_validate(doc))
})

test_that('eml_party creates multiple givenName, organizationName, and positionName fields', {
  creator <- eml_party('creator', c('John', 'and Jack'), 'Smith', c('NCEAS', 'UCSB'),
                       c('Programmers', 'brothers'))

  expect_equal(EML::eml_get(creator, 'givenName'), EML::as_emld(list('John', 'and Jack')))
  expect_equal(EML::eml_get(creator, 'organizationName'), EML::as_emld(list('NCEAS', 'UCSB')))
  expect_equal(EML::eml_get(creator, 'positionName'), EML::as_emld(list('Programmers', 'brothers')))
})

test_that('reorder_pids orders pids correctly', {
  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))
  oe1 <- list(entityName = "object one", entityType = "other")
  oe2 <- list(entityName = "object two", entityType = "other")
  doc <- list(packageId = "an id", system = "a system",
    dataset = list(
    title = "A Mimimal Valid EML Dataset",
    creator = me,
    contact = me,
    otherEntity = list(oe1, oe2)))

  pid_list <- list("object two" = "some identifier2", "object one" = "some identifier1")

  ordered_pids <- reorder_pids(pid_list, doc)
  entity_names <- eml_get_simple(doc, "entityName")
  expect_equal(names(ordered_pids), entity_names)
})

test_that('reorder_pids fails gracefully', {
  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))
  oe1 <- list(entityName = "object one", entityType = "other")
  doc <- list(packageId = "an id", system = "a system",
              dataset = list(
                title = "A Mimimal Valid EML Dataset",
                creator = me,
                contact = me,
                otherEntity = list(oe1)))

  pid_list <- list("object two" = "some identifier2", "object one" = "some identifier1")

  expect_error(reorder_pids(pid_list, doc))
})

test_that('eml_nsf_to_project generates a valid project section', {

  # for a single award, EML 2.1.1
  awards <- "1203146"
  proj <- eml_nsf_to_project(awards)

  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))

  doc <- list(packageId = "id", system = "system",
              dataset = list(title = "A Mimimal Valid EML Dataset",
                             creator = me,
                             contact = me))

  doc$dataset$project <- proj

  expect_true(eml_validate(doc))

  # for multiple awards, EML 2.1.1
  awards <- c("1203146", "1203473", "1603116")

  proj <- eml_nsf_to_project(awards)

  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))

  doc <- list(packageId = "id", system = "system",
              dataset = list(title = "A Mimimal Valid EML Dataset",
                             creator = me,
                             contact = me))

  doc$dataset$project <- proj

  expect_true(eml_validate(doc))

  # for multiple awards, EML 2.2.0
  awards <- c("1203146", "1203473", "1603116")

  emld::eml_version("eml-2.2.0")
  proj <- eml_nsf_to_project(awards, eml_version = "2.2")

  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))

  doc <- list(packageId = "id", system = "system",
              dataset = list(title = "A Mimimal Valid EML Dataset",
                             creator = me,
                             contact = me))

  doc$dataset$project <- proj

  expect_true(eml_validate(doc))

})

test_that('eml_nsf_to_project handles bad funding numbers gracefully', {

  awards <- c("abcdef", "1203473", "12345")

  expect_warning(proj <- eml_nsf_to_project(awards), "this award will not be included in the project section")

  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))

  doc <- list(packageId = "id", system = "system",
              dataset = list(title = "A Mimimal Valid EML Dataset",
                             creator = me,
                             contact = me))

  doc$dataset$project <- proj

  expect_true(eml_validate(doc))
})

test_that('eml_nsf_to_project fails gracefully', {

  awards <- c("abcdef", "12345")
  expect_error(suppressWarnings(proj <- eml_nsf_to_project(awards)), "No valid award numbers were found")

})


