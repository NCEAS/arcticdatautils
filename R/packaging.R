#' package.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Code related to packaging datasets.

library(whisker)
library(redland)

#' Create a Resource Map XML string suitable for use in an MN.Create() call
#'
#' Relevant documentation:
#' https://jenkins-ucsb-1.dataone.org/job/API%20Documentation%20-%20trunk/ws/api-documentation/build/html//design/DataPackage.html?highlight=resourcemap#generating-resource-maps
#'
#' @return The text of the file (character)
#' @export
#'
#' @examples
create_resource_map <- function() {
  # Create objects related to an RDF Model
  world <- new("World")
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world=world, storage, options="")

  # Set up the namespaces we need
  rdf <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdfs1 <- "http://www.w3.org/2001/01/rdf-schema#"
  dc <- "http://purl.org/dc/elements/1.1/"
  dcterms <- "http://purl.org/dc/terms/"
  cito <- "http://purl.org/spar/cito/"
  ore <- "http://www.openarchives.org/ore/terms/"

  # Temporary: Prepare the (invalid) URIs we'll need
  resource_map_identifier <- "resmapid"
  science_metadata_identifier <- "scimetaid"
  dataobject_identifiers <- c("dataobj1id", "dataobj2id", "dataobj3id", "dataobj4id")
  aggregation <- paste0(resource_map_identifier, "#aggregation")

  # Add statemnts

  # scimeta isDocumentedBy resourceMap
  addStatement(model, new("Statement", world = world,
                          subject=science_metadata_identifier,
                          predicate=paste0(cito, "isDocumentedBy"),
                          object=resource_map_identifier))

  # aggregation type Aggregation
  addStatement(model, new("Statement", world = world,
                          subject=aggregation,
                          predicate=paste0(rdf, "type"),
                          object=paste0(ore, "Aggregation")))

  # aggregation aggregates scimeta
  addStatement(model, new("Statement", world = world,
                          subject=aggregation,
                          predicate=paste0(rdf, "aggregates"),
                          object=science_metadata_identifier))

  # aggregation aggregates dataobject
  for (identifier in dataobject_identifiers) {
    addStatement(model, new("Statement", world = world,
                            subject=aggregation,
                            predicate=paste0(rdf, "aggregates"),
                            object=identifier))
  }

  # Aggregation label "Aggregation"
  addStatement(model, new("Statement", world = world,
                          subject=paste0(ore, "Aggregation"),
                          predicate=paste0(rdfs1, "label"),
                          object="Aggregation"))

  # Aggregation isDefinedBy ORE
  addStatement(model,  new("Statement", world = world,
                           subject=paste0(ore, "Aggregation"),
                           predicate=paste0(rdfs1, "isDefinedBy"),
                           object=ore))

  # ResourceMap label
  addStatement(model, new("Statement", world = world,
                          subject=paste0(ore, "ResourceMap"),
                          predicate=paste0(rdfs1, "label"),
                          object="ResourceMap"))

  # ResourceMap isDefinedBy ORE
  addStatement(model, new("Statement", world = world,
                          subject=paste0(ore, "ResourceMap"),
                          predicate=paste0(rdfs1, "isDefinedBy"),
                          object=ore))

  # dataobject isDocumentedBy + identifier
  for (identifier in dataobject_identifiers) {
    addStatement(model, new("Statement", world = world,
                            subject=identifier,
                            predicate=paste0(cito, "isDocumentedBy"),
                            object=science_metadata_identifier))

    addStatement(model, new("Statement", world = world,
                            subject=identifier,
                            predicate=paste0(dcterms, "identifier"),
                            object=identifier))
  }

  # scimeta identifier
  addStatement(model, new("Statement", world = world,
                          subject=science_metadata_identifier,
                          predicate=paste0(dcterms, "identifier"),
                          object=science_metadata_identifier))

  # scimeta documents dataobject
  for (identifier in dataobject_identifiers) {
    addStatement(model, new("Statement", world = world,
                            subject=science_metadata_identifier,
                            predicate=paste0(cito, "documents"),
                            object=identifier))
  }

  # resource_map type ResourceMap
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(rdf, "type"),
                          object=paste0(ore, "ResourceMap")))

  # resource_map identifier
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(dcterms, "identifier"),
                          object=resource_map_identifier))

  # resource_map format
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(dc, "format"),
                          object="application/rdf+xml"))

  # resource_map describes aggregation
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(ore, "describes"),
                          object=aggregation))

  # resource_map creator
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(dcterms, "creator"),
                          object="The `arcticdata` R package."))

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000%z")

  # resource_map created
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(dcterms, "created"),
                          object=now))

  # resource_map modified
  addStatement(model, new("Statement", world = world,
                          subject=resource_map_identifier,
                          predicate=paste0(dcterms, "modified"),
                          object=now))

  serializer <- new("Serializer", world, mimeType="application/rdf+xml")

  # Set up serializer namespaces
  setNameSpace(serializer, world, namespace=rdf, prefix="rdf")
  setNameSpace(serializer, world, namespace=rdfs1, prefix="rdfs1")
  setNameSpace(serializer, world, namespace=dc, prefix="dc")
  setNameSpace(serializer, world, namespace=dcterms, prefix="dcterms")
  setNameSpace(serializer, world, namespace=cito, prefix="cito")
  setNameSpace(serializer, world, namespace=ore, prefix="ore")

  serialized_resource_map <- serializeToCharacter(serializer, world, model)

  freeSerializer(serializer)
  freeModel(model)
  freeWorld(world)

  serialized_resource_map
}


#' Generate a System Metadata XML string suitable for use in an MN.Create() call
#'
#' Relevant documentation:
#' https://jenkins-ucsb-1.dataone.org/job/API%20Documentation%20-%20trunk/ws/api-documentation/build/html//design/SystemMetadata.html#id3
#'
#'
#' @param identifier
#' @param size
#' @param checksum
#' @param submitter
#' @param rightsHolder
#' @param checksumAlgorithm
#' @param formatID
#' @param replication
#'
#' @return The text of the file (character)
#' @export
#'
#' @examples
#' generate_system_metadata(identifier="IDENT",
#'                        size="1234",
#'                        checksum="ae626a6d626a6d626a6d6",
#'                        submitter="some_submitter",
#'                        rightsHolder = "some_submitter")

generate_system_metadata <- function(identifier,
                                     size,
                                     checksum,
                                     submitter,
                                     rightsHolder,
                                     checksumAlgorithm="SHA-256",
                                     formatID="application/octet-stream",
                                     replication="true") {
  filepath <- system.file("data/sysmeta_template.xml", package="arcticdata")
  stopifnot(file.exists(filepath))

  template <- readChar(filepath, file.info(filepath)$size)
  stopifnot(nchar(template) > 0)

  values <- list(identifier = identifier,
                 formatID = formatID,
                 size = size,
                 checksumAlgorithm = checksumAlgorithm,
                 checksum = checksum,
                 submitter = submitter,
                 rightsHolder = rightsHolder,
                 replication = replication)

  text <- whisker.render(template, values)
  cat(text)

  text
}
