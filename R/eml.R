# Helper functions for creating EML metadata

#' Create EML entity with physical section from a DataONE PID
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
    # entity$id <- list(xml_attribute = systmeta@identifier)
    entity$id <- systmeta@identifier
  }

  if (length(entity$scope) == 0) {
    #entity$scope <- list(xml_attribute = "document")
    entity$scope <- "document"
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
#' This function creates a pre-canned EML physical object from what's in the
#' System Metadata of an object. Note that it sets an Online Distribution URL
#' of the DataONE v2 resolve service for the PID.
#'
#' @param sysmeta (SystemMetadata) One or more System Metadata objects.
#'
#' @return (list) A list of physical objects.
#'
#' @export
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
                         url = paste0("https://cn.dataone.org/cn/v2/resolve/", sysmeta@identifier))

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


#' Create an EML party
#'
#' You will usually want to use the high-level functions such as
#' [eml_creator()] and [eml_contact()] but using this is fine.
#'
#' The `userId` argument assumes an ORCID so be sure to adjust for that.
#'
#' @param type (character) The type of party (e.g. 'contact').
#' @param given_names (character) The party's given name(s).
#' @param sur_name (character) The party's surname.
#' @param organization (character) The party's organization name.
#' @param position (character) The party's position.
#' @param email (character) The party's email address(es).
#' @param phone (character) The party's phone number(s).
#' @param address (character) The party's address(es) as a valid EML address
#' @param userId (character) The party's ORCID, in format https://orcid.org/WWWW-XXXX-YYYY-ZZZZ.
#' @param role (character) The party's role.
#'
#' @return (party) An instance of the party specified by the `type` argument.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_party("creator", "Test", "User")
#' eml_party("creator", "Bryce", "Mecum", userId = "https://orcid.org/0000-0002-0381-3766")
#' eml_party("creator", given_names = list("Dominic", "'Dom'"),
#'                      sur_name = "Mullen", list("NCEAS", "UCSB"),
#'                      position = list("Data Scientist", "Programmer"),
#'                          address = eml$address(deliveryPoint = "735 State St",
#'                          city = "Santa Barbara",
#'                          administrativeArea = "CA",
#'                          postalCode = "85719"))
#'}
eml_party <- function(type="associatedParty",
                      given_names = NULL,
                      sur_name = NULL,
                      organization = NULL,
                      position = NULL,
                      email = NULL,
                      phone = NULL,
                      address = NULL,
                      userId = NULL,
                      role = NULL) {
  if (all(sapply(c(sur_name, organization, position), is.null))) {
    stop(call. = FALSE,
         "You must specify at least one of sur_name, organization, or position to make a valid creator")
  }
  if (!is.null(address) &
      !"deliveryPoint" %in% names(address) &
      !"administrativeArea" %in% names(address) &
      !"postalCode" %in% names(address) &
      !"city" %in% names(address)) {
    stop(call. = FALSE,
         "An address was given but no deliveryPoint, administrativeArea, city, or postalCode child elements were specified.")
  }

  party <- list()

  # Individual Name
  if (!is.null(sur_name)) {
    party$individualName <- list(givenName = given_names, surName = sur_name)
  }

  # Organization Name
  if (!is.null(organization)) {
    party$organizationName <- organization
  }

  # Position
  if (!is.null(position)) {
    party$positionName <- position
  }

  # Email
  if (!is.null(email)) {
    party$electronicMailAddress <- email
  }

  # Address
  if (!is.null(address)) {
    party$address <- address
  }

  # Phone
  if (!is.null(phone)) {
    party$phone <- phone
  }

  # userId
  if (!is.null(userId)) {
    # Warn if the userId doesn't look like an ORCID
    if (!grepl("^https:\\/\\/orcid\\.org", userId)) {
      warning(paste0("The provided `userId` of '", userId, "' does not look like an ORCID and the `userId` argument assumes the given `userId` is an ORCID. ORCIDs should be passed in like https://orcid.org/WWWW-XXXX-YYYY-ZZZZ."))
    }

    party$userId$userId <- userId
    party$userId$directory = "https://orcid.org"
  }

  # Role
  if (!is.null(role)) {
    # Only allow roles to be set if type is associatedParty or personnel
    if (type != "associatedParty" && type != "personnel") {
      stop(call. = FALSE,
           paste0("Setting a role is only valid on an associatedParty or personnel, not a ", type, "."))
    }

    party$role <- role
  }



  party
}


#' Create an EML creator
#'
#' See [eml_party()] for details.
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (creator) The new creator.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_creator("test", "user", email = "test@@user.com")
#' eml_creator("creator", "Bryce", "Mecum", userId = "https://orcid.org/0000-0002-0381-3766")
#' eml_creator("creator", c("Dominic", "'Dom'"), "Mullen", c("NCEAS", "UCSB"),
#'             c("Data Scientist", "Programmer"))
#'}
eml_creator <- function(...) {
  eml_party("creator", ...)
}


#' Create an EML contact
#'
#' See [eml_party()] for details.
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (contact) The new contact.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_contact("test", "user", email = "test@@user.com")
#' eml_creator("creator", "Bryce", "Mecum", userId = "https://orcid.org/0000-0002-0381-3766")
#' eml_creator("creator", c("Dominic", "'Dom'"), "Mullen", c("NCEAS", "UCSB"),
#'             c("Data Scientist", "Programmer"))
#'}
eml_contact <- function(...) {
  eml_party("contact", ...)
}


#' Create an EML metadataProvider
#'
#' See [eml_party()] for details.
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (metadataProvider) The new metadataProvider.
#'
#' @export
#'
#' @examples
#' eml_metadata_provider("test", "user", email = "test@@user.com")
eml_metadata_provider <- function(...) {
  eml_party("metadataProvider", ...)
}


#' Create an EML associatedParty
#'
#' See [eml_party()] for details.
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (associatedParty) The new associatedParty.
#'
#' @export
#'
#' @examples
#' eml_associated_party("test", "user", email = "test@@user.com", role = "Principal Investigator")
eml_associated_party <- function(...) {
  eml_party("associatedParty", ...)
}


#' Create an EML personnel
#'
#' See [eml_party()] for details.
#'
#' @param ... Arguments passed on to [eml_party()].
#' @param role (character) Personnel role, e.g. "principalInvestigator".
#'
#' @return (personnel) The new personnel.
#'
#' @export
#'
#' @examples
#' eml_personnel("test", "user", email = "test@@user.com", role = "principalInvestigator")
eml_personnel <- function(role = NULL, ...) {
  if (is.null(role)) {
    stop(call. = FALSE,
         "You must specify a role for a personnel.")
  }

  eml_party("personnel", role = role, ...)
}

#' Create an EML project section
#'
#' Create an EML project section.
#'
#' Note - studyAreaDescription, designDescription, and relatedProject are not
#' fully fleshed out. Need to pass these objects in directly if you want to use
#' them.
#'
#' @param title (character) Title of the project (Required). May have multiple titles
#' constructed using `list`.
#' @param personnelList (list of personnel) Personnel involved with the project.
#' @param abstract (character) Project abstract. Can pass as a list
#'   for separate paragraphs.
#' @param funding (character) Funding sources for the project such as grant and
#'   contract numbers. Can pass as a list for separate paragraphs.
#' @param studyAreaDescription (studyAreaDescription)
#' @param designDescription (designDescription)
#' @param relatedProject (project)
#'
#' @return (project) The new project section.
#'
#' @export
#'
#' @examples
#' proj <- eml_project(list("Some title", "A second title if needed"),
#'            list(eml_personnel("Bryce", "Mecum", role = "principalInvestigator")),
#'            list("Abstract paragraph 1", "Abstract paragraph 2"),
#'            "Funding Agency: Award Number 12345")
eml_project <- function(title,
                        personnelList,
                        abstract = NULL,
                        funding = NULL,
                        studyAreaDescription = NULL,
                        designDescription = NULL,
                        relatedProject = NULL) {


  if (is.null(eml_get_simple(personnelList, "role"))) {
    stop(call. = FALSE,
         "Each person in the personnelList must have a role.")
  }

  project <- list()

  # Title
  project$title <- title

  project$personnel <- personnelList

  # Abstract
  if (!is.null(abstract)) {
    project$abstract <- list(para = abstract)
  }

  # Funding
  if (!is.null(funding)) {
    project$funding <- list(para = funding)
  }

  # Study area description
  if (!is.null(studyAreaDescription)) {
    project$studyAreaDescription <- studyAreaDescription
  }

  # Design description
  if (!is.null(designDescription)) {
    project$designDescription <- designDescription
  }

  # Related Project
  if (!is.null(relatedProject)) {
    project$relatedProject <- relatedProject
  }

  project
}


#' Create an EML geographicCoverage section
#'
#' A simple way to create an EML geographicCoverage section.
#'
#' For a bounding box, all coordinates should be unique.
#' For a single point, the North and South bounding coordinates should be the same and
#' the East and West bounding coordinates should be the same.
#'
#' Note that EML::set_coverage() provides the same (and more) functionality
#'
#' @param description (character) A textual description.
#' @param north (numeric) North bounding coordinate.
#' @param east (numeric) East bounding coordinate.
#' @param south (numeric) South bounding coordinate.
#' @param west (numeric) West bounding coordinate.
#'
#' @return (geographicCoverage) The new geographicCoverage section.
#'
eml_geographic_coverage <- function(description, north, east, south, west) {
  cov <- list()

  cov$geographicDescription <- description

  cov$boundingCoordinates$northBoundingCoordinate <- as.character(north)
  cov$boundingCoordinates$eastBoundingCoordinate <- as.character(east)
  cov$boundingCoordinates$southBoundingCoordinate <- as.character(south)
  cov$boundingCoordinates$westBoundingCoordinate <- as.character(west)

  cov
}


#' Create an EML address element
#'
#' A simple way to create an EML address element.
#'
#' Note that EML::eml$address() provides the same functionality
#'
#' @param delivery_points (character) One or more delivery points.
#' @param city (character) City.
#' @param administrative_area (character) Administrative area.
#' @param postal_code (character) Postal code.
#'
#' @return (address) An EML address object.
#'
eml_address <- function(delivery_points, city, administrative_area, postal_code) {
  stopifnot(is.character(delivery_points),
            is.character(city),
            is.character(administrative_area),
            (is.character(postal_code) || is.numeric(postal_code)))

  address <- list()

  address$deliveryPoint <- delivery_points
  address$city <- city
  address$administrativeArea <- administrative_area
  address$postalCode <- as.character(postal_code)


  address
}


#' Set the abstract for an EML document
#'
#' Set the abstract for an EML document.
#'
#' @param doc (eml) An EML document.
#' @param text (character) The abstract text. If \code{text} is length one, an
#'   abstract without \code{<para>} or \code{<section>} elements will be created.
#'   If \code{text} is greater than one in length, \code{para} elementes will be
#'   used for each element.
#'
#' @return (eml) The modified EML document.
#'
set_abstract <- function(doc, text) {
  # need to rewrite this test
  # stopifnot(is(doc, "eml"))

  if (length(text) == 1) {
    doc$dataset$abstract <- list(abstract = text)
  } else if (length(text) > 1) {
    doc$dataset$abstract <- list(abstract = text)
  }

  doc
}


#' Create an EML abstract
#'
#' Create an EML abstract.
#'
#' Note that eml$abstract() provides the same functionality.
#'
#' @param text (character) Paragraphs of text with one paragraph per element in the
#'   character vector, constructed using `list`
#'
#' @return (abstract) An EML abstract.
#'
#'
#' @examples
#' \dontrun{
#' # Set an abstract with a single paragraph
#' eml_abstract("Test abstract...")
#'
#' # Or one with multiple paragraphs
#' eml_abstract(list("First para...", "second para..."))
#' }
eml_abstract <- function(text) {
  stopifnot(is.character(text),
            length(text) > 0,
            all(nchar(text)) > 0)

    abstract <- list(abstract = list(para = text))

  abstract
}


#' Validate an EML attributeList attribute-by-attribute
#'
#' The attributes passed into this function are validated one-by-one and the
#' progress of going through each attribute is printed to the screen along
#' with any and all validation issues. This is done by, for each attribute in the list,
#' creating a minimum valid EML document and adding a new otherEntity with a new
#' attributeList containing the single attribute to be validated.
#'
#' @param attributes (attributeList) An attributeList.
#'
#' @return (logical) Named vector indicating which attributes are valid.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' atts_df <- read.csv('attributes_table.csv', stringsAsFactors = F)
#' enum_domain <- read.csv('enumerated_domain.csv') # optional
#' attributes <- EML::set_attributes(atts_df, factor = enum_domain)
#' eml_validate_attributes(attributes)
#' }
eml_validate_attributes <- function(attributes) {


  # Define an interal applyable function to validate each attribute
  eml_validate_attribute <- function(attribute) {
    stopifnot(!is.null(names(attribute)))

    doc$dataset$otherEntity$attributeList$attribute[[1]] <- attribute

    # Validate!
    eml_validate(doc)
  }


  # Create a minimum valid EML doc we'll re-use each time we validate a single
  # attribute


  # Create a dummy otherEntity with our attributeList

  doc <- list(packageId = "test",
              system = "test",
              dataset = list(
                title = "test",
                creator = list(individualName = list(givenName = "test", surName = "test")),
                contact = list(individualName = list(givenName = "test", surName = "test")),
                otherEntity = list(entityName = "name", entityType = "otherEntity")))

  results <- sapply(attributes$attribute, function(attribute) {
    cat(paste0("Validating single attribute '", attribute$attributeName, "': "))

    result <- NULL
    result <- tryCatch({
      eml_validate_attribute(attribute)
    },
    message = function(m) { m }
    )

    if (is(result, "simpleMessage")) {
      cat("FALSE\n")
      message(trimws(result$message))
      return(FALSE)
    } else {
      cat(result, "\n")
      return(result)
    }
  })

  names(results) <- sapply(attributes$attribute, function(x) x$attributeName)

  results
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
    }
    otherEntity$entityType <- NULL
    ## delete otherEntity from list
    doc$dataset$otherEntity <- NULL
  } else {
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


#' Search through EMLs
#'
#' This function returns indices within an EML list that contain an instance where
#' `test == TRUE`. See examples for more information.
#'
#' @param doc (list) An EML object.
#' @param element (character) Element to evaluate.
#' @param test (function/character) A function to evaluate (see examples). If test is a character,
#'   will evaluate if \code{element == test} (see example 1).
#'
#' @import EML
#'
#' @export
#'
#' @author Mitchell Maier mitchell.maier@@gmail.com
#'
#' @examples
#' \dontrun{
#' # Question: Which creators have a surName "Smith"?
#' n <- which_in_eml(eml$dataset$creator, "surName", "Smith")
#' # Answer: eml$dataset$creator[n]
#'
#' # Question: Which dataTables have an entityName that begins with "2016"
#' n <- which_in_eml(eml$dataset$dataTable, "entityName", function(x) {grepl("^2016", x)})
#' # Answer: eml$dataset$dataTable[n]
#'
#' # Question: Which attributes in dataTable[[1]] have a numberType "natural"?
#' n <- which_in_eml(eml$dataset$dataTable[[1]]$attributeList$attribute, "numberType", "natural")
#' # Answer: eml$dataset$dataTable[[1]]$attributeList$attribute[n]
#'
#' #' # Question: Which dataTables have at least one attribute with a numberType "natural"?
#' n <- which_in_eml(eml$dataset$dataTable, "numberType", function(x) {"natural" %in% x})
#' # Answer: eml$dataset$dataTable[n]
#' }
which_in_eml <- function(doc, element, test) {

  stopifnot(methods::is(doc, "list"))
  stopifnot(is.character(element))

  if (is.character(test)) {
    value = test
    test = function(x) {x == value}

  } else {
    stopifnot(is.function(test))
  }

  elements_test <- eml_get(doc, element)

  if (is.null(elements_test)) {
    location <- NULL

  } else {
    result <- test(elements_test)

    if (length(isTRUE(result)) > 1) {
      stop("Test must only return one value.")

    } else if (length(isTRUE(result)) == 1){
      location <- which(result == TRUE)

    } else {
      location <- NULL
    }
  }
  names(location) <- NULL
  return(location)
}


#' Set a reference to an EML object
#'
#' This function creates a new object with the same class as \code{element_to_replace}
#' using a reference to \code{element_to_reference}.
#'
#' @param element_to_reference (list) An EML element to reference.
#' @param element_to_replace (list) An EML element to replace with a reference.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' doc <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#'
#' # Set the first contact as a reference to the first creator
#' doc$dataset$contact[[1]] <- eml_set_reference(doc$dataset$creator[[1]],
#' doc$dataset$contact[[1]])
#'
#' # This is also useful when we want to set references to a subset of 'dataTable'
#'   or 'otherEntity' objects
#' # Add a few more objects first to illustrate the use:
#' doc$dataset$dataTable[[3]] <- doc$dataset$dataTable[[1]]
#' doc$dataset$dataTable[[4]] <- doc$dataset$dataTable[[1]]
#' # Add references to the second and third elements only (not the 4th):
#' for (i in 2:3) {
#'     doc$dataset$dataTable[[i]]$attributeList <- eml_set_reference(
#'                                                       doc$dataset$dataTable[[1]]$attributeList,
#'                                                       doc$dataset$dataTable[[i]]$attributeList)
#' }
#' # If we print the entire 'dataTable' list we see elements 2 and 3 have
#' references while 4 does not.
#'
#' doc$dataset$dataTable
#' }
eml_set_reference <- function(element_to_reference, element_to_replace) {
  if (length(element_to_reference$id) == 0) {
    stop('No id detected at element_to_reference$id. Please add an id in order to use references.')
  }
  id <- element_to_reference$id[1]
  element_to_replace <- list(references = id)
  return(element_to_replace)
}


#' Set shared attribute references
#'
#' This function sets shared attributes using the attributes of the first \code{type}
#' selected and creates references for all remaining objects of equivalent \code{type}.
#'
#' @param doc (emld) An EML object.
#' @param attributeList (attributeList) Optional. An EML attributeList object. If not provided
#'   then it will default to the attributeList of the first \code{type} element.
#' @param type (character) Optional. Specifies whether to replace 'dataTable' or 'otherEntity'
#'   attributeList objects with references. Defaults to 'dataTable'.
#'
#' @return (doc) The modified EML document.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' doc <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#' atts <- EML::set_attributes(
#'                      EML::get_attributes(eml$dataset$dataTable[[1]]$attributeList)$attributes)
#'
#' eml <- eml_set_shared_attributes(eml, atts, type = 'dataTable')
#' }
eml_set_shared_attributes <- function(doc, attributeList = NULL, type = 'dataTable') {
  stopifnot(methods::is(doc, 'emld'))
  stopifnot(type %in% c('dataTable', 'otherEntity'))

  x <- doc$dataset[[type]]
  n <- length(x)
  if (n <= 1) {
    stop('1 or fewer entities') # add message
  }

  # If a new attributeList is provided set it
  if (!is.null(attributeList)) {
    x[[1]]$attributeList <- attributeList
  }
  x[[1]]$attributeList$id <- stringi::stri_rand_strings(1, length = 10) # generate random identifier
  # Apply references to all other elements
  for (i in 2:n) {
    x[[i]]$attributeList <- eml_set_reference(x[[1]]$attributeList, x[[i]]$attributeList)
  }

  doc$dataset[[type]] <- x
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
eml_nsf_to_project <- function(awards, eml_version = "2.1"){

  stopifnot(is.character(awards))
  stopifnot(eml_version %in% c("2.1", "2.1.1", "2.2", "2.2.0"))

  award_nums <- awards

  result <- lapply(award_nums, function(x){
    url <- paste0("https://api.nsf.gov/services/v1/awards.json?id=", x ,"&printFields=coPDPI,pdPIName,title")

    t <- jsonlite::fromJSON(url)

    if ("serviceNotification" %in% names(t$response)) {
      warning(paste(t$response$serviceNotification$notificationType, "for award", x , "\n this award will not be included in the project section."), call. = FALSE)
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
    p_list[[i]] <- eml_personnel(given_names = people$firstName[i],
                                 sur_name = people$lastName[i],
                                 role = people$role[i])
  }

  titles <- lapply(result, function(x){
    unlist(x$response$award$title)
  })

  if (eml_version %in% c("2.1", "2.1.1")){
    award_nums <- paste("NSF", award_nums)
    proj <- eml_project(title = titles, personnelList = p_list, funding = award_nums)
  }
  else if (eml_version %in% c("2.2", "2.2.0")){
    awards <- list()

    for (i in 1:length(award_nums)){
      awards[[i]] <- list(title = titles[i],
                          funderName = "National Science Foundation",
                          funderIdentifier = "https://doi.org/10.13039/00000001",
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
      lastName = trimws(gsub("[A-Za-z]{2,}\\s[A-Z]?", "", x)),
      stringsAsFactors = F)})
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

  if(grepl(".csv", ob_name, fixed = TRUE) == TRUE || grepl(".xlsx", ob_name, fixed = TRUE) == TRUE){
    physical <- EML::set_physical(objectName = ob_name,
                                  size = format(sysmeta@size, scientific = FALSE),
                                  sizeUnit = 'bytes',
                                  authentication = sysmeta@checksum,
                                  authMethod = sysmeta@checksumAlgorithm,
                                  numHeaderLines = num_header_lines,
                                  fieldDelimiter = ',',
                                  attributeOrientation = 'column',
                                  url = paste0("https://cn.dataone.org/cn/v2/resolve/", sysmeta@identifier))
  }else{
    physical <- sysmeta_to_eml_physical(sysmeta)
  }
  return(physical)
}
