test_that("creating a ECSO annotation works", {

  expected <- list(
    propertyURI = list(label = "contains measurements of type",
                       propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType"),
    valueURI = list(label = "Water Salinity",
                    valueURI = "http://purl.dataone.org/odo/ECSO_00001164")
  )

  ecso <- eml_ecso_annotation("Water Salinity")

  expect_equal(ecso$propertyURI$label, expected$propertyURI$label)
})


test_that("fails if the annotation does not exist in ECSO", {

  expect_error(eml_ecso_annotation("Annotation does not exist"))

})

test_that("Creating an ARCRC key variable annotation works", {

  expected <- list(
    propertyURI = list(label = "isAbout",
                       propertyURI = "http://purl.obolibrary.org/obo/IAO_0000136"),
    valueURI = list(label = "age of sea ice",
                    valueURI = "http://purl.dataone.org/odo/ARCRC_00000057")
  )

  arcrc_annotation <- eml_arcrc_key_variable_annotation("age of sea ice")

  expect_equal(arcrc_annotation$propertyURI$label, expected$propertyURI$label)
  expect_equal(arcrc_annotation$valueURI$label, expected$valueURI$label)
})

test_that("fails if the annotation does not exist in ARCRC", {

  expect_error(eml_arcrc_key_variable_annotation("Annotation does not exist"))
})

test_that("Creating an ARCRC essay annotation works", {

  expected <- list(
    propertyURI = list(label = "influenced",
                       propertyURI = "http://www.w3.org/ns/prov#influenced"),
    valueURI = list(label = "Sea Ice Indicator",
                    valueURI = "http://purl.dataone.org/odo/ARCRC_00000007")
  )

  arcrc_annotation <- eml_arcrc_essay_annotation("Sea Ice Indicator")

  expect_equal(arcrc_annotation$propertyURI$label, expected$propertyURI$label)
  expect_equal(arcrc_annotation$valueURI$label, expected$valueURI$label)
})

test_that("fails if the annotation does not exist in ARCRC", {

  expect_error(eml_arcrc_essay_annotation("Annotation does not exist"))
})
