# Helper functions for creating EML metadata

#' Create EML entity with physical section from any DataONE PID
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pid (character) The PID of the object to create the sub-tree for.
#' @param entity_type (character) What kind of object to create from the input. One of "dataTable",
#'   "spatialRaster", "spatialVector", "storedProcedure", "view", or "otherEntity".
#' @param ... (optional) Additional arguments to be passed to \code{eml$entityType())}.
#'
#' @return (list) The entity object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EML otherEntity
#' pid_to_eml_entity(mn,
#'                   pid,
#'                   entity_type = "otherEntity",
#'                   entityName = "Entity Name",
#'                   entityDescription = "Description about entity")
#' }
pid_to_eml_entity <- function(mn,
                              pid,
                              entity_type = "otherEntity",
                              ...) {

  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0,
            length(pid) == 1)

  stopifnot(entity_type %in% c("dataTable",
                              "spatialRaster",
                              "spatialVector",
                              "storedProcedure",
                              "view",
                              "otherEntity"))

  systmeta <- getSystemMetadata(mn, pid)

  entity <- list(physical = pid_to_eml_physical(mn, pid), ...)

  # Set entity slots
  if (length(entity$id) == 0) {
    entity$id <- systmeta@identifier
  }

  if (length(entity$scope) == 0) {
    entity$scope <- "document"
  }

  if (length(entity$system) == 0) {
    entity$system <- get_system_uri(entity$id)
  }

  if (length(entity$entityName) == 0) {

    if (!is.na(systmeta@fileName)) {
      entity$entityName <- systmeta@fileName
    }
  }

  if (entity_type == "otherEntity" && length(entity$entity_type) == 0) {
    entity$entityType <- "Other"
  }
  else if (entity_type != "otherEntity"){
    entity$entityType <- NULL
  }

  return(entity)
}

#' Create an EML physical object from system metadata
#'
#' This function creates an EML physical object based on what's in the
#' System Metadata of an object. Note that it sets an Online Distribution URL
#' of the DataONE v2 resolve service for the PID.
#'
#' @param sysmeta (SystemMetadata) One or more System Metadata objects.
#'
#' @return (list) A list of physical objects.
#'
#'
#' @examples
#' \dontrun{
#' # Generate EML physical object from a system metadata object
#' sm <- getSystemMetadata(mn, pid)
#' sysmeta_to_eml_physical(sm)
#' }
sysmeta_to_eml_physical <- function(sysmeta) {
    stopifnot(is(sysmeta, "SystemMetadata"))

    if (is.na(sysmeta@fileName)) {
      ob_name <- "NA"
    } else {
      ob_name <- sysmeta@fileName
    }


    phys <- set_physical(objectName = ob_name,
                         size = format(sysmeta@size, scientific = FALSE),
                         sizeUnit = "bytes",
                         authentication = sysmeta@checksum,
                         authMethod = sysmeta@checksumAlgorithm,
                         url = URLencode(paste0("https://cn.dataone.org/cn/v2/resolve/", sysmeta@identifier)))

    phys$dataFormat <- list(externallyDefinedFormat = list(formatName = sysmeta@formatId))

    phys
}


#' Get the Metacat docid for the given identifier
#'
#' Get the Metacat docid for the given identifier.
#'
#' @param sysmeta (SystemMetadata) The sysmeta of the object you want to find.
#'
#' @return (character) The docid.
#'
#' @noRd
get_doc_id <- function(sysmeta) {
  stopifnot(is(sysmeta, "SystemMetadata"))

  message("Looking up docid for ", sysmeta@identifier, ".")

  # Hack: Determine whether we should check production or dev Metacat
  if (sysmeta@originMemberNode == "urn:node:ARCTIC") {
    metacat_base_url <- env_load("production", skip_mn = TRUE)$metacat_base_url
  } else {
    metacat_base_url <- env_load("test", skip_mn = TRUE)$metacat_base_url
  }
  # EndHack

  # Get the docID from metacat
  request_url <- paste0(metacat_base_url, "?action=getdocid&pid=", URLencode(sysmeta@identifier, reserved = TRUE))
  response <- httr::GET(request_url)

  if (response$status_code != 200) {
    stop(paste0("Failed to get the doc ID for ", sysmeta@identifier, " from Metacat. (HTTP Status was ", response$status_code, ")."))
  }

  # Parse the response
  content <- httr::content(response)
  stopifnot(inherits(content, "xml_document"))

  # Extract the doc ID
  doc_id_nodes <- xml2::xml_find_all(content, "//docid")
  stopifnot(length(doc_id_nodes) == 1)

  doc_id <- xml2::xml_text(doc_id_nodes[[1]])
  stopifnot(is.character(doc_id),
            nchar(doc_id) > 0)

  doc_id
}

#' Convert otherEntities to dataTables
#'
#' Convert an EML 'otherEntity' object to a 'dataTable' object. This will convert an
#' otherEntity object as currently constructed - it does not add a physical or add attributes.
#' However, if these are already in their respective slots, they will be retained.
#'
#' @param doc (list) An EML document.
#' @param index (integer) The indicies of the otherEntities to be transformed.
#' @param validate_eml (logical) Optional. Whether or not to validate the EML after
#'   completion. Setting this to `FALSE` reduces execution time by ~50 percent.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @importFrom magrittr '%>%'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' doc <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))
#'
#' doc <- eml_otherEntity_to_dataTable(doc, 1)
#' }
eml_otherEntity_to_dataTable <- function(doc, index, validate_eml = TRUE) {
  stopifnot(methods::is(doc, "emld"))
  stopifnot(is.logical(eml_validate(doc)))
  stopifnot(is.numeric(index))
  stopifnot(length(eml_get_simple(doc$dataset$otherEntity, "entityName")) >= index)
  if (any(duplicated(eml_get_simple(doc$dataset, "entityName"))) == T){
    stop(call. = FALSE,
         "entityNames must be unique")
  }

  ## set OE entityTypes to NULL and select the ones we want to use
  if (length(eml_get_simple(doc$dataset$otherEntity, "entityName")) == 1) {
    ## prepare OE to copy
    otherEntity <- doc$dataset$otherEntity
    ## Handle case where otherEntity is in a list of length 1 (boxed)
    if (is.null(names(otherEntity))) {
      otherEntity <- otherEntity[[1]]
    } else if (!is.null(names(otherEntity))) {  ## Handle case where otherEntity is unboxed
      otherEntity <- list(otherEntity)
    }
    otherEntity[[1]]$entityType <- NULL
    ## delete otherEntity from list
    doc$dataset$otherEntity <- NULL
  }  else {
    otherEntity <- doc$dataset$otherEntity[index]

    for (i in 1:length(index)){
      otherEntity[[i]]$entityType <- NULL
    }
    ## delete otherEntity from list
    doc$dataset$otherEntity <- doc$dataset$otherEntity[-index]
  }


  dts <- doc$dataset$dataTable

  ## handle various datatable length cases
  if (length(dts) == 0){
    doc$dataset$dataTable <- otherEntity
  } else{
    if (length(eml_get_simple(dts, "entityName")) == 1){
      dts <- list(dts)
      doc$dataset$dataTable <- c(dts, otherEntity)
    }

    else {
      doc$dataset$dataTable <- c(dts, otherEntity)
    }
  }

  ## return eml
  if (validate_eml == TRUE) {
    valid_eml <- eml_validate(doc)
    if (!valid_eml) {
      stop(attributes(valid_eml))
    }
  }

  return(doc)
}


#' Get a simple list output from EML::eml_get()
#'
#' This function is a convenience wrapper around EML::eml_get() which
#' returns the output as a simple list as opposed to an object of type
#' `emld` by removing the attributes and context from the object. If an
#' element containing children is returned all of it's children will be
#' flattened into a named character vector. This function is best used
#' to extract values from elements that have no children.
#'
#' @param doc (list) An EML object or child/descendant object
#' @param element (character) Name of the element to be extracted. If
#' multiple occurrences are found, will extract all.
#'
#' @return out (vector) A list of values contained in element given
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#'
#' doc <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#'
#' datatable_names <- eml_get_simple(doc$dataset$dataTable, element = "entityName")
#'}
#'
eml_get_simple <- function(doc, element){
  out <- eml_get(doc, element, from = "list")
  out$`@context` <- NULL
  attributes(out) <- NULL
  out <- unlist(out)
  return(out)
}

#' Reorder a named list of objects according to the order in the metadata
#'
#' This function takes a named list of data objects, such as what is
#' returned from `get_package`, and reorders them according to the order
#' they are given in the EML document.
#'
#' @param pid_list (list) A named list of data pids
#' @param doc (list) an `emld` document
#'
#' @return ordered_pids (list) A list of reordered pids
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#
#' ids <- get_package(adc, 'resource_map_doi:10.18739/A2S17SS1M', file_names = TRUE)
#' doc <- EML::read_eml(dataone::getObject(adc, ids$metadata))
#'
#' # return all entity types
#' ordered_pids <- reorder_pids(ids$data, doc)
#'}
#'
reorder_pids <- function(pid_list, doc){
  stopifnot(!is.null(names(pid_list)))

  entity_names <- eml_get_simple(doc, "entityName")

  if (is.null(entity_names)){
    stop("No entity names were found.")
  }

  if (length(entity_names) != length(pid_list)){
    stop("Number of entities in EML and resource map do not match")
  }

  ordered_pids <- pid_list[order(match(names(pid_list), entity_names))]
  return(ordered_pids)
}

#' Create an EML project section from a list of NSF award numbers
#'
#' This function takes a list of NSF award numbers and uses it to
#' query the NSF API to get the award title, PIs, and coPIs. The
#' return value is an EML project section. The function supports 1
#' or more award numbers
#'
#' @param awards (list) A list of NSF award numbers as characters
#' @param eml_version (char) EML version to use (2.1.1 or 2.2.0)
#' @return project (emld) An EML project section
#'
#' @export
#'
#' @examples
#' awards <- c("1203146", "1203473", "1603116")
#'
#' proj <- eml_nsf_to_project(awards, eml_version = "2.1.1")
#'
#' me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))
#'
#' doc <- list(packageId = "id", system = "system",
#'            dataset = list(title = "A Mimimal Valid EML Dataset",
#'                           creator = me,
#'                           contact = me))
#'
#' doc$dataset$project <- proj
#'
#' EML::eml_validate(doc)
#'
eml_nsf_to_project <- function(awards, eml_version = "2.2"){

  stopifnot(is.character(awards))
  stopifnot(eml_version %in% c("2.1", "2.1.1", "2.2", "2.2.0"))

  award_nums <- awards

  result <- lapply(award_nums, function(x){
    url <- paste0("https://api.nsf.gov/services/v1/awards.json?id=", x ,"&printFields=coPDPI,pdPIName,title")

    t <- tryCatch(jsonlite::fromJSON(url),
                  error = function(j) {
                    j$message <- paste0("The NSF API is most likely down. Check back later. ", j$message)
                  })

    if ("serviceNotification" %in% names(t$response)) {
      warning(paste(t$response$serviceNotification$notificationMessage, "\n",
                    t$response$serviceNotification$notificationType, "for award", x ,
                    "\n this award will not be included in the project section."), call. = FALSE)
      t <- NULL
    }
    else if (length(t$response$award) == 0){
      warning(paste("Empty result for award", x, "\n this award will not be included in the project section."), call. = FALSE)
      t <- NULL
    }
    else t
  })

  i <- lapply(result, function(x) {!is.null(x)})
  result <- result[unlist(i)]
  award_nums <- award_nums[unlist(i)]

  if (length(award_nums) == 0){
    stop(call. = F,
         "No valid award numbers were found.")
  }

  co_pis <- lapply(result, function(x){
    extract_name(x$response$award$coPDPI)
  })

  co_pis <- unlist(co_pis, recursive = F)
  co_pis <- do.call("rbind", co_pis)
  if (!is.null(co_pis)){
    co_pis$role <- "coPrincipalInvestigator"
  }

  pis <- lapply(result, function(x){
    extract_name(x$response$award$pdPIName)
  })

  pis <- unlist(pis, recursive = F)
  pis <- do.call("rbind", pis) %>%
    dplyr::mutate(role = "principalInvestigator")

  people <- dplyr::bind_rows(co_pis, pis) %>%
    dplyr::distinct()

  p_list <- list()
  for (i in 1:nrow(people)){
    p_list[[i]] <- EML::eml$personnel(individualName = list(givenName = people$firstName[i],
                                                            surName = people$lastName[i]),
                                      role = people$role[i])
  }

  titles <- lapply(result, function(x){
    unlist(x$response$award$title)
  })

  if (eml_version %in% c("2.1", "2.1.1")){
    award_nums <- paste("NSF", award_nums)
    proj <- EML::eml$project(title = titles, personnel = p_list, funding = award_nums)

  }
  else if (eml_version %in% c("2.2", "2.2.0")){
    awards <- list()

    for (i in 1:length(award_nums)){
      awards[[i]] <- list(title = titles[i],
                          funderName = "National Science Foundation",
                          funderIdentifier = "https://doi.org/10.13039/100000001",
                          awardNumber = award_nums[i],
                          awardUrl = paste0("https://www.nsf.gov/awardsearch/showAward?AWD_ID=", award_nums[i]))
    }

    proj <- list(title = titles, personnel = p_list, award = awards)
    return(proj)
  }
}

# Extract first and last name from NSF API results
#
# The NSF API jams the first name, last name, and middle initial if it exists into a single string.
# This simple helper uses some regex to split the names up.
extract_name <- function(x){
  lapply(x, function(x) {
    data.frame(
      firstName = trimws(stringr::str_extract(x, "[A-Za-z]{2,}\\s[A-Z]?")),
      lastName = trimws(gsub("^([A-Za-z]{2,})\\s[A-Z]?", "", x)),
      stringsAsFactors = F)})
}


#' Get raster info from a file on disk
#'
#' This function populates a spatialRaster element with the
#' required elements by reading a local raster file in. The
#' `coord_name` argument can be found by examining the data.frame
#' that `get_coord_list()` returns against the proj4string of the
#' raster file.
#'
#' @param path (char) Path to a raster file
#' @param coord_name (char) horizCoordSysDef name
#' @param attributeList (list) Entity attributeList for raster
#'
#'
#' @export
eml_get_raster_metadata <- function(path, coord_name = NULL, attributeList){

  raster_obj <- raster::raster(path)
  message(paste("Reading raster object with proj4string of ", raster::crs(raster_obj)@projargs))

  if (is.null(coord_name)){
    coord_name <- raster::crs(raster_obj)@projargs
  }


  if (identical(raster::origin(raster_obj), c(0,0))){
    raster_orig <- "Upper Left"
  } else if(!identical(raster::origin(raster_obj), c(0,0))){
    message("Raster origin not at 0,0")
    raster_orig <- "unknown"
  }

  raster_info <- list(entityName = basename(path),
                      attributeList = attributeList,
                      spatialReference = list(horizCoordSysName = coord_name),
                      horizontalAccuracy = list(accuracyReport = "unknown"),
                      verticalAccuracy = list(accuracyReport = "unknown"),
                      cellSizeXDirection = raster::res(raster_obj)[1],
                      cellSizeYDirection = raster::res(raster_obj)[2],
                      numberOfBands = raster::nbands(raster_obj),
                      rasterOrigin = raster_orig,
                      rows = dim(raster_obj)[1],
                      columns = dim(raster_obj)[2],
                      verticals = dim(raster_obj)[3],
                      cellGeometry = "pixel")
  return(raster_info)
}


#' Create EML physical objects for the given set of PIDs
#'
#' This function creates a data object's physical.
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pid (character) The PID of the object to create the physical for.
#' @param num_header_lines (double) The number of headers in a csv/Excel file. Default is equal to 1.
#'
#' @return (list) A physical object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EML physical sections for an object in a data package
#' phys <- pid_to_eml_physical(mn, pid, num_header_lines)
#' }
pid_to_eml_physical <- function(mn, pid, num_header_lines = 1) {
  stopifnot(is(mn, "MNode"))

  if (!is_token_set(mn)) {
    stop('No token set')
  }
  stopifnot(is.character(pid),
            all(nchar(pid)) > 0,
            length(pid) == 1)
  names(pid) <- ''  # Named inputs produce a named output list - which is invalid in EML

  sysmeta <- dataone::getSystemMetadata(mn, pid)

  if (is.na(sysmeta@fileName)) {
    ob_name <- "NA"
  } else {
    ob_name <- sysmeta@fileName
  }

  if(grepl(".csv", ob_name, fixed = TRUE) == TRUE){
    physical <- EML::set_physical(objectName = ob_name,
                                  size = format(sysmeta@size, scientific = FALSE),
                                  sizeUnit = 'bytes',
                                  authentication = sysmeta@checksum,
                                  authMethod = sysmeta@checksumAlgorithm,
                                  numHeaderLines = num_header_lines,
                                  fieldDelimiter = ',',
                                  attributeOrientation = 'column',
                                  url = URLencode(paste0("https://cn.dataone.org/cn/v2/resolve/", sysmeta@identifier)))

  }else{
    physical <- sysmeta_to_eml_physical(sysmeta)
  }
  return(physical)

}

#' Add publisher information to EML document
#'
#' This function adds Arctic Data Center publisher information to an EML document
#'
#' @param doc (emld) An EML document
#'
#' @return (emld) An EML document
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Add publisher information to an existing document
#' doc <- eml_add_publisher(doc)
#' }
eml_add_publisher <- function(doc){
  stopifnot(methods::is(doc, 'list'))

  doc$dataset$publisher <- list(organizationName = "NSF Arctic Data Center",
                                         onlineUrl = "http://arcticdata.io",
                                         userId = list(directory = "https://www.wikidata.org/", userId = "Q77285095"),
                                electronicMailAddress = "support@arcticdata.io")

  return(doc)


}

#' Add system information to entities
#'
#' This function adds system information to entities in a document
#'
#' @param doc (emld) An EML document
#'
#' @return (emld) An EML document
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Add publisher information to an existing document
#' doc <- eml_add_entity_system(doc)
#' }
eml_add_entity_system <- function(doc){
  stopifnot(methods::is(doc, 'list'))

  # other entity
  if (length(doc$dataset$otherEntity) > 1){
    if (!is.null(names(doc$dataset$otherEntity))) {
      doc$dataset$otherEntity <- list(doc$dataset$otherEntity)
    }
    for (i in 1:length(doc$dataset$otherEntity)){
      doc$dataset$otherEntity[[i]]$system <- get_system_uri(doc$dataset$otherEntity[[i]]$id)
    }
  }


  # data table
  if (length(doc$dataset$dataTable) > 1){
    if (!is.null(names(doc$dataset$dataTable))) {
      doc$dataset$dataTable <- list(doc$dataset$dataTable)
    }
    for (i in 1:length(doc$dataset$dataTable)){
      doc$dataset$dataTable[[i]]$system <- get_system_uri(doc$dataset$dataTable[[i]]$id)
    }
  }

  # vector
  if (length(doc$dataset$spatialVector) > 1){
    if (!is.null(names(doc$dataset$spatialVector))) {
      doc$dataset$spatialVector <- list(doc$dataset$spatialVector)
    }
    for (i in 1:length(doc$dataset$spatialVector)){
      doc$dataset$spatialVector[[i]]$system <- get_system_uri(doc$dataset$spatialVector[[i]]$id)
    }
  }

  # raster
  if (length(doc$dataset$spatialRaster) > 1){
    if (!is.null(names(doc$dataset$spatialRaster))) {
      doc$dataset$spatialRaster <- list(doc$dataset$spatialRaster)
    }
    for (i in 1:length(doc$dataset$spatialRaster)){
      doc$dataset$spatialRaster[[i]]$system <- get_system_uri(doc$dataset$spatialRaster[[i]]$id)
    }
  }

  return(doc)


}

#' Categorize a dataset with an annotation
#'
#' Creates an annotation from the ADC Academic Disciplines ontology
#' [here](https://bioportal.bioontology.org/ontologies/ADCAD/?p=classes&conceptid=root)
#' and inserts the annotation into the EML document `doc` while retaining any existing
#' annotations such as the sensitivity annotations. For a list of available disciplines,
#' see link above.
#'
#'
#'
#' @param doc (emld) An EML document
#' @param discipline (character) One or more disciplines in title case from the ADCAD ontology.
#'
#' @return doc (emld) An EML document with annotation added
#' @export
#' @examples
#' library(EML)
#' # read in any EML document
#' doc <- read_eml(system.file("extdata/strix-pacific-northwest.xml", package="dataone"))
#' # add the dataset categories
#' doc <- eml_categorize_dataset(doc, c("Soil Science", "Ecology"))
#'
eml_categorize_dataset <- function(doc, discipline){

  stopifnot("emld" %in% class(doc))

  if (is.null(doc$dataset$id)){
    doc$dataset$id <- gsub(":", "-", doc$packageId)
  }

  existing_anns <- doc$dataset$annotation

  new_ann <- purrr::map(discipline, eml_adcad_annotation)

  doc$dataset$annotation <- c(list(existing_anns), new_ann)

  return(doc)
}


#' Add distribution information to EML
#'
#' Adds a landing page URL to the dataset, and corrects the metadata identifier
#' by replacing the existing identifier with that which is passed. Note that this
#' function constructs landing page URLs for the Arctic Data Center only and will not work
#' correctly on other repositories.
#'
#'
#'
#' @param doc (emld) An EML document
#' @param identifier (character) A pre-issued, unassigned identifier (as from `dataone::generateIdentifier()`)
#'
#' @return doc (emld) An EML document with distribution added
#' @export
#' @examples
#' \dontrun{
#' library(EML)
#' d1c <- dataone::D1Client("STAGING", "mnTestARCTIC")
#' # read in any EML document
#' doc <- read_eml(system.file("extdata/strix-pacific-northwest.xml", package="dataone"))
#' # generate a doi
#' id <- generateIdentifier(d1c@mn, "doi")
#' doc <- eml_add_distribution(doc, id)
#' }
#'
eml_add_distribution <- function(doc, identifier){

  stopifnot("emld" %in% class(doc))

  doc$packageId <- identifier

  use_doi <- grepl("doi", identifier)

  # Add landing page
  if (use_doi == T){
    doc$dataset$distribution$offline <- NULL
    doc$dataset$distribution$online$url <- list(url = paste0("http://doi.org/", identifier),
                                                `function` = "information")
  }
  else if (use_doi == F){
    doc$dataset$distribution$offline <- NULL
    doc$dataset$distribution$online$url <- list(url = paste0("http://arcticdata.io/catalog/view/", identifier),
                                                `function` = "information")
  }

  return(doc)
}
