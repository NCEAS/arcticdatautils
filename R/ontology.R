
#' Get an owl file from github
#'
#' @param ontology_name the name of the onotology to read; one of mosaic or ecso
#'
#' @return list
#' @export
#'
#' @examples
#' read_ontology("mosaic")
#' read_ontology("ecso")
read_ontology <- function(ontology_name) {
  # get the owl file from github
  if(ontology_name == "mosaic"){
    ann_url <-
      "https://raw.githubusercontent.com/DataONEorg/sem-prov-ontologies/main/MOSAiC/MOSAiC.owl"
    ont <- rdflib::rdf_parse(pins::pin(ann_url),
                                format = "rdfxml")
  } else if(ontology_name == "ecso"){
    ann_url <-
      "https://raw.githubusercontent.com/DataONEorg/sem-prov-ontologies/ECSO8-add_non-carbon_measurements/observation/ECSO8.owl"
    ont <- rdflib::rdf_parse(pins::pin(ann_url),
                                format = "rdfxml")
  } else if (ontology_name == "ADCAD"){
    ann_url <- "https://data.bioontology.org/ontologies/ADCAD/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb&download_format=rdf"
    ont <- rdflib::rdf_parse(ann_url,
                             format = "rdfxml")
  } else if (ontology_name == "ARCRC") {
    ann_url <- "https://data.bioontology.org/ontologies/ARCRC/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb&download_format=rdf"
    ont <- rdflib::rdf_parse(ann_url,
                             format = "rdfxml")
  }

}

#' Gets all the concepts
#'
#' Takes an ontology and returns a dataframe with all the URIs and labels.
#' This is mainly used for MOSAiC because the ontology is modeled differently
#'
#' @param ontology (list) the list form of a OWL file
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' mosaic <- read_ontology("mosaic")
#' get_ontology_concepts(mosaic)
get_ontology_concepts <- function(ontology){
  #find the class IRI
  query_class <-
    'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  select distinct ?label ?Concept
  where {
  [] a ?Concept .
  ?Concept rdfs:label ?label .}'

  suppressMessages(rdflib::rdf_query(ontology, query_class))
}


#' Given a an annotation from the ECSO ontology, produce the corresponding annotation
#'
#' Reduces the amount of copy pasting needed
#'
#' @param valueLabel (character) the label for the annotation found in
#' [ECSO](https://bioportal.bioontology.org/ontologies/ECSO/?p=classes&conceptid=root)
#'
#' @return list - a formatted EML annotation
#' @export
#'
#' @examples eml_ecso_annotation("latitude coordinate")
eml_ecso_annotation <- function(valueLabel){

  ecso <- read_ontology("ecso")

  query <-
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?iri ?label
   WHERE {
     ?iri rdf:type <http://www.w3.org/2002/07/owl#Class> .
     ?iri rdfs:label ?label .
   }"

  df <- suppressMessages(rdflib::rdf_query(ecso, query))

  stopifnot(valueLabel %in% df$label)

  annotations <- dplyr::filter(df, label == valueLabel)

  list(
    propertyURI = list(label = "contains measurements of type",
                       propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType"),
    valueURI = list(label = annotations$label,
                    valueURI = annotations$iri)
  )
}

#' Given a term from the ADC Academic Disciplines (ADCAD) ontology, produce the corresponding annotation
#'
#' Reduces the amount of copy pasting needed
#'
#' @param valueLabel (character) One of the disciplines found in
#' [ADCAD](https://bioportal.bioontology.org/ontologies/OBOE/?p=classes&conceptid=root)
#'
#' @return list - a formatted EML annotation
#' @export
#'
#' @examples eml_ecso_annotation("latitude coordinate")
#' @importFrom rlang .data
eml_adcad_annotation <- function(valueLabel){

  adcad <- read_ontology("ADCAD")

  query <-
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?iri ?label
   WHERE {
     ?iri rdf:type <http://www.w3.org/2002/07/owl#Class> .
     ?iri rdfs:label ?label .
   }"

  df <- suppressMessages(rdflib::rdf_query(adcad, query))

  stopifnot(valueLabel %in% df$label)

  annotations <- dplyr::filter(df, .data$label == valueLabel)

  list(
    propertyURI = list(label = "theme",
                       propertyURI = "http://www.w3.org/ns/dcat#theme"),
    valueURI = list(label = annotations$label,
                    valueURI = annotations$iri)
  )
}

#' Given a key variable from the Arctic Report Card (ARCRC) ontology, produce the corresponding annotation
#'
#' Reduces the amount of copy pasting needed
#'
#' @param valueLabel (character) One of the key variables found in
#' [ARCRC](https://bioportal.bioontology.org/ontologies/ARCRC/?p=classes&conceptid=http%3A%2F%2Fpurl.dataone.org%2Fodo%2FARCRC_00000040)
#'
#' @return list - a formatted EML annotation
#' @export
#'
#' @examples eml_arcrc_key_variable_annotation("age of sea ice")
eml_arcrc_key_variable_annotation <- function(valueLabel) {

  arcrc <- read_ontology("ARCRC")

  query <-
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?iri ?label
   WHERE {
     ?iri rdf:type <http://www.w3.org/2002/07/owl#Class> .
     ?iri rdfs:label ?label .
   }"

  df <- suppressMessages(rdflib::rdf_query(arcrc, query))

  stopifnot(valueLabel %in% df$label)

  annotations <- dplyr::filter(df, label == valueLabel)

  list(
    propertyURI = list(label = "isAbout",
                       propertyURI = "http://purl.obolibrary.org/obo/IAO_0000136"),
    valueURI = list(label = annotations$label,
                    valueURI = annotations$iri)
  )
}

#' Given an essay topic from the Arctic Report Card (ARCRC) ontology, produce the corresponding annotation
#'
#' Reduces the amount of copy pasting needed
#'
#' @param valueLabel (character) One of the essay topics found in
#' [ARCRC](https://bioportal.bioontology.org/ontologies/ARCRC/?p=classes&conceptid=http%3A%2F%2Fpurl.dataone.org%2Fodo%2FARCRC_00000510)
#'
#' @return list - a formatted EML annotation
#' @export
#'
#' @examples eml_arcrc_essay_annotation("Sea Ice Indicator")
eml_arcrc_essay_annotation <- function(valueLabel) {

  arcrc <- read_ontology("ARCRC")

  query <-
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?iri ?label
   WHERE {
     ?iri rdf:type <http://www.w3.org/2002/07/owl#Class> .
     ?iri rdfs:label ?label .
   }"

  df <- suppressMessages(rdflib::rdf_query(arcrc, query))

  stopifnot(valueLabel %in% df$label)

  annotations <- dplyr::filter(df, label == valueLabel)

  list(
    propertyURI = list(label = "influenced",
                       propertyURI = "http://www.w3.org/ns/prov#influenced"),
    valueURI = list(label = annotations$label,
                    valueURI = annotations$iri)
  )
}
