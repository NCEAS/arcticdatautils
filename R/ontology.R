
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

  annotations <- dplyr::filter(df, label == valueLabel)

  list(
    propertyURI = list(label = "theme",
                       propertyURI = "http://www.w3.org/ns/dcat#theme"),
    valueURI = list(label = annotations$label,
                    valueURI = annotations$iri)
  )
}

