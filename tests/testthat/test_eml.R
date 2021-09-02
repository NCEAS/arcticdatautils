context("EML")

mn <- env_load()$mn

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

test_that("eml_otherEntity_to_dataTable works when every object is boxed", {
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
  expect_equal(otherEntity$entityName, doc$dataset$dataTable[[1]]$entityName)
  expect_equivalent(otherEntity$physical, doc$dataset$dataTable[[1]]$physical)
})

test_that("eml_oe_to_dt works in all cases", {


  me <- list(individualName = list(givenName = "Kristen", surName = "Peach"))
  attributes <- data.frame(attributeName = 'length_1',
                           attributeDefinition = 'def1',
                           measurementScale = 'ratio',
                           domain = 'numericDomain',
                           unit = 'meter',
                           numberType = 'real',
                           stringsAsFactors = FALSE)

  att_list <- EML::set_attributes(attributes)

  oe1 <- list(entityName = 'other entity', attributeList = att_list, entityType = "other")
  oe2 <- list(entityName = 'other entity2', attributeList = att_list, entityType = "other")
  dT1 = list(entityName = "data table", attributeList = att_list)
  dT2 = list(entityName = "data table2", attributeList = att_list)

  #Testing if it works when there are 0 dataTables and 1 otherEntity

  doc_zero_dts <- list(packageId = 'id', system = 'system',
              dataset = list(title = 'A Mimimal Valid EML Dataset',
                             creator = me,
                             contact = me,
                             otherEntity = oe1))

  write_eml(doc_zero_dts, "~/test.xml")
  doc_zero_dts <- read_eml("~/test.xml")
  doc_zero_dts <- eml_otherEntity_to_dataTable(doc_zero_dts, 1)

  expect_true(eml_validate(doc_zero_dts))

  #Testing if it works when there are 2 dataTables and 1 otherEntity

  doc_two_dts <- list(packageId = 'id', system = 'system',
                       dataset = list(title = 'A Mimimal Valid EML Dataset',
                                      creator = me,
                                      contact = me,
                                      dataTable = list(dT1, dT2),
                                      otherEntity = oe1))

  write_eml(doc_two_dts, "~/test.xml")
  doc_two_dts <- read_eml("~/test.xml")
  doc_two_dts <- eml_otherEntity_to_dataTable(doc_two_dts, 1)

  expect_true(eml_validate(doc_two_dts))

  #Testing if it works when there are 2 dataTables and 1 boxed otherEntity

  doc_two_dts_boxed_oe <- list(packageId = 'id', system = 'system',
                      dataset = list(title = 'A Mimimal Valid EML Dataset',
                                     creator = me,
                                     contact = me,
                                     dataTable = list(dT1, dT2),
                                     otherEntity = list(oe1)))

  write_eml(doc_two_dts_boxed_oe, "~/test.xml")
  doc_two_dts_boxed_oe <- read_eml("~/test.xml")
  doc_two_dts_boxed_oe <- eml_otherEntity_to_dataTable(doc_two_dts_boxed_oe, 1)

  expect_true(eml_validate(doc_two_dts_boxed_oe))

  #Testing if it works when there are 2 otherEntities and 0 dataTables

  doc_two_oes <- list(packageId = 'id', system = 'system',
                      dataset = list(title = 'A Mimimal Valid EML Dataset',
                                     creator = me,
                                     contact = me,
                                     otherEntity = list(oe1, oe2)))

  write_eml(doc_two_oes, "~/test.xml")
  doc_two_oes <- read_eml("~/test.xml")
  doc_two_oes <- eml_otherEntity_to_dataTable(doc_two_oes, 1)

  expect_true(eml_validate(doc_two_oes))

  #Testing if it works when there are two otherEntities and 1 dataTable

  doc_two_oes_one_dt <- list(packageId = 'id', system = 'system',
                      dataset = list(title = 'A Mimimal Valid EML Dataset',
                                     creator = me,
                                     contact = me,
                                     dataTable = dT1,
                                     otherEntity = list(oe1, oe2)))

  write_eml(doc_two_oes_one_dt, "~/test.xml")
  doc_two_oes_one_dt <- read_eml("~/test.xml")
  doc_two_oes_one_dt <- eml_otherEntity_to_dataTable(doc_two_oes_one_dt, 1)

  expect_true(eml_validate(doc_two_oes_one_dt))

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

test_that('eml_nsf_to_project parses two-word last names correctly', {
  proj <- eml_nsf_to_project("1822406", eml_version = "2.2")

  expect_equal(proj$personnel[[2]]$individualName$givenName, "Maria")
  expect_equal(proj$personnel[[2]]$individualName$surName, "Val Martin")
})

test_that('Data object physical created for an EML', {

  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  cn_staging <- CNode('STAGING')
  mn_test <- getMNode(cn_staging, 'urn:node:mnTestARCTIC')

  pkg <- arcticdatautils::get_package(mn_test, 'resource_map_urn:uuid:8e0cf450-44d2-4c9d-aa48-09cb32538d2b')

  doc <- EML::read_eml(getObject(mn_test, pkg$metadata))

  csv1_physical <- arcticdatautils::pid_to_eml_physical(mn_test, pkg$data[4], num_header_lines = sample(1:100,1))

  attributes1 <- arcticdatautils::create_dummy_attributes_dataframe(2)
  attributeList1 <- set_attributes(attributes1)

  dataTable <- list(entityName = "dummy1.csv",
                    entityDescription = "test csv",
                    physical = csv1_physical,
                    attributeList = attributeList1)

  doc$dataset$dataTable[[1]] <- dataTable

  expect_true(EML::eml_validate(doc))

})

test_that('Valid publisher information can be added', {

  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))


  doc <- list(packageId = "id", system = "system",
              dataset = list(title = "A Mimimal Valid EML Dataset",
                             creator = me,
                             contact = me))

  doc <- eml_add_publisher(doc)

  expect_true(EML::eml_validate(doc))
  expect_equal(doc$dataset$publisher$organizationName, "NSF Arctic Data Center")
})


test_that('Identifier systems can be added', {

  me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))
  doc <- list(packageId = "id", system = "system",
              dataset = list(title = "A Mimimal Valid EML Dataset",
                             creator = me,
                             contact = me,
                             otherEntity = list(list(entityName = "name",
                                                     entityType = "other",
                                                     id = "urn:uuid:asldkfjh"),
                                                list(entityName = "name2",
                                                     entityType = "other",
                                                     id = "arctic-data.2.2"))))

  doc <- eml_add_entity_system(doc)

  expect_true(EML::eml_validate(doc))
  expect_equal(doc$dataset$otherEntity[[1]]$system, "https://tools.ietf.org/html/rfc4122")
  expect_equal(doc$dataset$otherEntity[[2]]$system, "https://search.dataone.org")
})
