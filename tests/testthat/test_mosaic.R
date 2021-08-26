test_that("mosaic dataset level annotations work", {
  annotation <- mosaic_annotate_dataset("PS122/2")

  example_annotation <- list(
    #Basis
    list(
      propertyURI = list(label = "hasBasis",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000034"),
      valueURI = list(label = "Polarstern",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00000030")
    ),
    #Project
    list(
      propertyURI = list(label = "hasProjectLabel",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000025"),
      valueURI = list(label = "MOSAiC20192020",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00000023")
    ),
    #Campaign
    list(
      propertyURI = list(label = "isPartOfCampaign",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000032"),
      valueURI = list(label = "PS122/2",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00000018")
    )
  )

  expect_equal(
    eml_get_simple(example_annotation, "label"),
    eml_get_simple(annotation, "label")
  )

  expect_equal(
    eml_get_simple(example_annotation, "propertyURI"),
    eml_get_simple(annotation, "propertyURI")
  )
})

test_that("multiple campaigns work", {
  annotations <- mosaic_annotate_dataset(c("PS122/2", "PS122/1"))

  expect_equal(length(annotations), 4)
})

test_that("warns users if it isn't a PS attribute", {
  expect_warning(mosaic_annotate_dataset("AF-MOSAiC-1"))
})

test_that("mosaic attribute level annotations work", {
  annotation <- mosaic_annotate_attribute("PS122/2_14-270")

  example_annotation <- list(
    #event
    list(
      propertyURI = list(label = "wasGeneratedBy",
                         propertyURI = "http://www.w3.org/ns/prov#wasGeneratedBy"),
      valueURI = list(label = "PS122/2_14-270",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00004550")
    ),
    #Method/Device
    list(
      propertyURI = list(label = "deployedSystem",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00002201"),
      valueURI = list(label = "Ultra-Wideband Software-defined Microwave Radiometer (0.5-2GHZ)",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00001163")
    )
  )

  expect_equal(
    eml_get_simple(example_annotation, "propertyURI"),
    eml_get_simple(annotation, "propertyURI")
  )

})

test_that("the MOSAiC portal filter works", {

  basis <-
    "<choice><label>Akademik Fedorov</label><value>https://purl.dataone.org/odo/MOSAIC_00000038</value></choice><choice><label>Akademik Tryoshnikov</label><value>https://purl.dataone.org/odo/MOSAIC_00011203</value></choice><choice><label>Polar 5</label><value>https://purl.dataone.org/odo/MOSAIC_00011224</value></choice><choice><label>Polar 6</label><value>https://purl.dataone.org/odo/MOSAIC_00011223</value></choice><choice><label>Polarstern</label><value>https://purl.dataone.org/odo/MOSAIC_00000030</value></choice>"

  expect_equal(mosaic_portal_filter("Basis"), basis)

})
