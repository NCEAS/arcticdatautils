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

test_that("produces a warning if there are more than one annotation found", {

  expect_warning(eml_ecso_annotation("thickness"))
  expect_warning(eml_ecso_annotation("Depth"))

})

