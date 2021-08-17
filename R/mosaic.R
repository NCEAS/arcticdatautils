# add attribute annotations
#' Add an attribute annotation (does not include the id slot)
#'
#' @param eventLabel (character) the event ID provided by the researcher
#'
#' @return (list) the annotation
#' @export
#'
#' @examples mosaic_annotate_attribute("PS122/2_14-270")
mosaic_annotate_attribute <- function(eventLabel) {
  # get the owl file from github
  mosaic <- rdflib::rdf_parse("https://raw.githubusercontent.com/DataONEorg/sem-prov-ontologies/main/MOSAiC/MOSAiC.owl",
    format = "rdfxml"
  )
  # search device
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

  event_device_iri <- events %>%
    dplyr::filter(label == eventLabel)

  event_annotation <- list(
    # event
    list(
      propertyURI = list(
        label = "wasGeneratedBy",
        propertyURI = "http://www.w3.org/ns/prov#wasGeneratedBy"
      ),
      valueURI = list(
        label = event_device_iri$label[1],
        valueURI = event_device_iri$event_iri[1]
      )
    ),
    # Method/Device
    list(
      propertyURI = list(
        label = "deployedSystem",
        propertyURI = "https://purl.dataone.org/odo/MOSAIC_00002201"
      ),
      valueURI = list(
        label = event_device_iri$dlabel[1],
        valueURI = event_device_iri$device_iri[1]
      )
    )
  )

  return(event_annotation)
}

#' Annotating the dataset level annotations
#'
#' the basis might differ depending on the campaign this function assumes the use of the Polarstern as the basis- please check carefully
#'
#' @param campaign (character) the campaign number (can be derrived from the eventID)
#'
#' @return
#' @export
#'
#' @examples mosaic_annotate_dataset("PS122/2")
mosaic_annotate_dataset <- function(campaign){

  #get the owl file from github
  mosaic <- rdflib::rdf_parse("https://raw.githubusercontent.com/DataONEorg/sem-prov-ontologies/main/MOSAiC/MOSAiC.owl",
                      format = "rdfxml")

  #possible campaigns
  query <-
    'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

   SELECT ?campaign_iri ?label
   WHERE {
     ?campaign_iri rdf:type <https://purl.dataone.org/odo/MOSAIC_00000001> .
     ?campaign_iri rdfs:label ?label .
   }'

  df_campaign <- rdflib::rdf_query(mosaic, query)

  campaign_iri <- dplyr::filter(df_campaign, label == campaign)


  list(
    #Basis
    list(propertyURI = list(label = "hasBasis",
                            propertyURI = "http://purl.dataone.org/odo/MOSAiC_00000034"),
         valueURI = list(label = "Polarstern",
                         valueURI = "http://purl.dataone.org/odo/MOSAiC_00000030")),
    #Project
    list(propertyURI = list(label = "hasProjectLabel",
                            propertyURI = "http://purl.dataone.org/odo/MOSAiC_00000025"),
         valueURI = list(label = "MOSAiC20192020",
                         valueURI = "http://purl.dataone.org/odo/MOSAiC_00000023")),
    #Campaign
    list(propertyURI = list(label = "isPartOfCampaign",
                            propertyURI = "http://purl.dataone.org/odo/MOSAiC_00000032"),
         valueURI = list(label = campaign_iri$label[1],
                         valueURI = campaign_iri$campaign_iri[1]))
  )
}
