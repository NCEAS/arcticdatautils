test_that("multiplication works", {

  annotation <- mosaic_annotate_dataset("PS122/2")

  example_annotation <- list(
    #Basis
    list(propertyURI = list(label = "hasBasis",
                            propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000034"),
         valueURI = list(label = "Polarstern",
                         valueURI = "https://purl.dataone.org/odo/MOSAIC_00000030")),
    #Project
    list(propertyURI = list(label = "hasProjectLabel",
                            propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000025"),
         valueURI = list(label = "MOSAiC20192020",
                         valueURI = "https://purl.dataone.org/odo/MOSAIC_00000023")),
    #Campaign
    list(propertyURI = list(label = "isPartOfCampaign",
                            propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000032"),
         valueURI = list(label = "PS122/2",
                         valueURI = "https://purl.dataone.org/odo/MOSAIC_00000018"))
  )

  expect_equal(eml_get_simple(example_annotation, "label"),
               eml_get_simple(annotation, "label"))

  expect_equal(eml_get_simple(example_annotation, "propertyURI"),
               eml_get_simple(annotation, "propertyURI"))
})
