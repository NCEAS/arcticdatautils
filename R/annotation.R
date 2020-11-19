annotate_att <- function(d, index = NULL, attribute, identifier, label, valueURI){

  if (is.null(index)) {
    att <- arcticdatautils::eml_get_simple(d$dataset$dataTable,
                                           "attributeName")
    j <- which(att == attribute)

    d$dataset$dataTable$attributeList$attribute[[j]]$id <- identifier
    d$dataset$dataTable$attributeList$attribute[[j]]$annotation$propertyURI <- list(label = "contains measurements of type",
                                                                                    propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType")
    d$dataset$dataTable$attributeList$attribute[[j]]$annotation$valueURI <- list(label = label,
                                                                                 valueURI = valueURI)
  }
  else {
    att <- arcticdatautils::eml_get_simple(d$dataset$dataTable[[index]],
                                           "attributeName")
    j <- which(att == attribute)

    d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$id <- identifier
    d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$annotation$propertyURI <- list(label = "contains measurements of type",
                                                                                             propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType")
    d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$annotation$valueURI <- list(label = label,
                                                                                          valueURI = valueURI)
  }
  return(d)
}
