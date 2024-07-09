

#' Add a MOSAiC (https://mosaic-expedition.org/) attribute annotation (the returned object does not include the id slot)
#'
#' @param eventLabel (character) the event ID provided by the researcher
#'
#' @return (list) the attribute level annotation
#' @export
#'
#' @examples mosaic_annotate_attribute("PS122/2_14-270")
#' @importFrom rlang .data
mosaic_annotate_attribute <- function(eventLabel) {
  # get the owl file from github
  mosaic <- read_ontology("mosaic")

  # search for the event labels and corresponding devices (labels and URIs)
  query <-
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
   PREFIX ssn: <http://www.w3.org/ns/ssn/>

   SELECT ?event_iri ?label ?device_iri ?dlabel
   WHERE {
     ?event_iri rdf:type <https://schema.org/Event> .
     ?event_iri rdfs:label ?label .
     ?event_iri ssn:deployedSystem ?device_iri .
     ?device_iri rdfs:label ?dlabel .
   }"

  events <- suppressMessages(rdflib::rdf_query(mosaic, query))

  stopifnot(eventLabel %in% events$label)

  event_device_iri <- events %>%
    dplyr::filter(.data$label == eventLabel)

  #construct the annotation
  event_annotation <- list(
    # event
    list(
      propertyURI = list(label = "wasGeneratedBy",
                         propertyURI = "http://www.w3.org/ns/prov#wasGeneratedBy"),
      valueURI = list(label = event_device_iri$label[1],
                      valueURI = event_device_iri$event_iri[1])
    ),
    # Method/Device
    list(
      propertyURI = list(label = "deployedSystem",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00002201"),
      valueURI = list(label = event_device_iri$dlabel[1],
                      valueURI = event_device_iri$device_iri[1])
    )
  )

  return(event_annotation)
}

#' Annotating the MOSAiC dataset level annotations
#'
#' The basis might differ depending on the campaign if it does not follow the pattern PS122/#. This function assumes the use of the Polarstern as the basis.
#' Please verify this field before adding the annotation.
#'
#' @param campaign (character vector) the campaign number (can be derrived from the eventID), PS122/#
#'
#' @return (list) the dataset level annotation
#' @export
#'
#' @examples
#' #with one campaign
#' mosaic_annotate_dataset("PS122/2")
#'
#' #multiple campaigns
#' mosaic_annotate_dataset(c("PS122/2", "PS122/1"))
#' @importFrom rlang .data
mosaic_annotate_dataset <- function(campaign) {
  check_ps <-
    purrr::map(campaign, ~ stringr::str_detect(.x, "PS", negate = T))

  if (all(unlist(check_ps))) {
    warning("Event id does not start with PS. Check if the basis is correct")
  }

  mosaic <- read_ontology("mosaic")

  #get the possible campaigns
  query <-
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?campaign_iri ?label
   WHERE {
     ?campaign_iri rdf:type <https://purl.dataone.org/odo/MOSAIC_00000001> .
     ?campaign_iri rdfs:label ?label .
   }"

  df_campaign <- suppressMessages(rdflib::rdf_query(mosaic, query))

  stopifnot(campaign %in% df_campaign$label)

  campaign_iri <- dplyr::filter(df_campaign, .data$label %in% campaign)

  construct_campaign <- function(label, uri) {
    # Campaign
    list(
      propertyURI = list(label = "isPartOfCampaign",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000032"),
      valueURI = list(label = label,
                      valueURI = uri)
    )
  }

  campaigns <-
    purrr::map2(campaign_iri$label,
                campaign_iri$campaign_iri,
                construct_campaign)

  #construct annotation
  standard_annotations <- list(
    # Basis
    list(
      propertyURI = list(label = "hasBasis",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000034"),
      valueURI = list(label = "Polarstern",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00000030")
    ),
    # Project
    list(
      propertyURI = list(label = "hasProjectLabel",
                         propertyURI = "https://purl.dataone.org/odo/MOSAIC_00000025"),
      valueURI = list(label = "MOSAiC20192020",
                      valueURI = "https://purl.dataone.org/odo/MOSAIC_00000023")
    )
  )

  append(standard_annotations, campaigns)
}

#just the concepts
query_class <-
  'select distinct ?Concept where {[] a ?Concept} LIMIT 100'

#' Creates the choice label pairs to be pasted into a portal document
#'
#' The function only selects the annotations that are used for method/devices (there are 500 + options).
#' copy and paste the output into a portal document's choice filters
#'
#' @param class (character) a class in the MOSAiC ontology to get the filters from
#'
#' @return character
#' @export
#'
#' @examples
#' mosaic_portal_filter("Method/Device")
#'
#' mosaic_portal_filter("Basis")
#'
#' mosaic_portal_filter("Campaign")
#' @importFrom rlang .data
mosaic_portal_filter <- function(class) {

  #find the class IRI
  mosaic <- read_ontology("mosaic")

  concepts <- get_ontology_concepts(mosaic)

  df_uri <- dplyr::filter(concepts, .data$label == class)

  #build the SPARQL query
  query <-
    paste0(
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?iri ?label
   WHERE {
     ?iri rdf:type <",
     df_uri$Concept[1],
      "> .
     ?iri rdfs:label ?label .
   }"
    )

  df <- suppressMessages(rdflib::rdf_query(mosaic, query)) %>%
    dplyr::arrange(.data$label)

  #for method/devices, filter the list based on existing annotations
  if (df_uri$Concept[1] == "https://purl.dataone.org/odo/MOSAIC_00000036") {

    cn <- dataone::CNode('PROD')
    adc <- dataone::getMNode(cn, 'urn:node:ARCTIC')

    #get all the MOSAiC datasets
    result <-
      dataone::query(
        adc,
        list(
          q = "sem_annotation:*MOSAIC* AND (*:* NOT obsoletedBy:*)",
          fl = "identifier,rightsHolder,formatId, fileName, dateUploaded, sem_annotation",
          sort = 'dateUploaded+desc',
          start = "0",
          rows = "1500"
        ),
        as = "data.frame"
      )

    #select only the relevant annotations
    relevant <- unique(unlist(result$sem_annotation))

    df <- df %>%
      dplyr::filter(.data$iri %in% relevant)

  }

  formatted_choice <- purrr::map2(
    df$label,
    df$iri,
    ~ paste0(
      "<choice><label>",
      .x,
      "</label><value>",
      .y,
      "</value></choice>"
    )
  )

  paste0(formatted_choice, collapse = "")

}
