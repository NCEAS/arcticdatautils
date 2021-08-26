

#' Add a MOSAiC (https://mosaic-expedition.org/) attribute annotation (the returned object does not include the id slot)
#'
#' @param eventLabel (character) the event ID provided by the researcher
#'
#' @return (list) the attribute level annotation
#' @export
#'
#' @examples mosaic_annotate_attribute("PS122/2_14-270")
mosaic_annotate_attribute <- function(eventLabel) {
  # get the owl file from github
  mosaic <- read_mosaic()

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
    dplyr::filter(label == eventLabel)

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
mosaic_annotate_dataset <- function(campaign) {
  check_ps <-
    purrr::map(campaign, ~ stringr::str_detect(.x, "PS", negate = T))

  if (all(unlist(check_ps))) {
    warning("Event id does not start with PS. Check if the basis is correct")
  }

  mosaic <- read_mosaic()

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

  campaign_iri <- dplyr::filter(df_campaign, label %in% campaign)

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

#' Creates the choice label pairs to be pasted into a portal document
#'
#' @param class
#'
#' @return
#' @export
#'
#' @examples
#' method <- mosaic_portal_filter("https://purl.dataone.org/odo/MOSAIC_00000036")
#' basis <- mosaic_portal_filter("https://purl.dataone.org/odo/MOSAIC_00000028")
#' campaign <- mosaic_portal_filter("https://purl.dataone.org/odo/MOSAIC_00000001")
mosaic_portal_filter <- function(class) {
  #get the possible annotations under this class

  #build the SPARQL query
  query <-
    paste0(
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?iri ?label
   WHERE {
     ?iri rdf:type <",
      URI,
      "> .
     ?iri rdfs:label ?label .
   }"
    )

  mosaic <- read_mosaic()

  df <- suppressMessages(rdflib::rdf_query(mosaic, query)) %>%
    arrange(label)

  #for method/devices find what the existing datasets are annotated with
  if (class == "https://purl.dataone.org/odo/MOSAIC_00000036") {
    cn <- CNode('PROD')
    adc <- getMNode(cn, 'urn:node:ARCTIC')

    #get all the MOSAiC datasets
    result <-
      query(
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

    #all the relevant annotations
    relevant <- unique(unlist(result$sem_annotation))

    df <- df %>%
      filter(iri %in% relevant)

  }

  campaigns <- purrr::map2(
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

  paste0(campaigns, collapse = "")

}

#helper
#' Get the MOSAiC owl file from github
#'
#' @return
#' @export
#'
#' @examples
read_mosaic <- function() {
  # get the owl file from github
  mosaic_url <-
    "https://raw.githubusercontent.com/DataONEorg/sem-prov-ontologies/main/MOSAiC/MOSAiC.owl"
  mosaic <- rdflib::rdf_parse(pins::pin(mosaic_url),
                              format = "rdfxml")
}
