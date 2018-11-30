# Helper functions for creating EML metadata




#' Create EML entity from a DataONE PID
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pid (character) The PID of the object to create the sub-tree for.
#' @param entityType (character) What kind of objects to create from the input. One of "dataTable",
#'   "spatialRaster", "spatialVector", "storedProcedure", "view", or "otherEntity".
#' @param ... (optional) Additional arguments to be passed to \code{new(entityType, ...)}.
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
#'                   entityType = "otherEntity",
#'                   entityName = "Entity Name",
#'                   entityDescription = "Description about entity")
#' }
pid_to_eml_entity <- function(mn,
                              pid,
                              entityType = "otherEntity",
                              ...) {

  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  stopifnot(entityType %in% c("dataTable",
                              "spatialRaster",
                              "spatialVector",
                              "storedProcedure",
                              "view",
                              "otherEntity"))

  systmeta <- getSystemMetadata(mn, pid)

  # Create entity
  entity <- new(entityType,
                physical = pid_to_eml_physical(mn, pid),
                ...)

  # Set entity slots
  if (length(slot(entity, "id")) == 0) {
    entity@id <- new("xml_attribute", systmeta@identifier)
  }

  if (length(slot(entity, "scope")) == 0) {
    entity@scope <- new("xml_attribute", "document")
  }

  if (length(slot(entity, "entityName")) == 0) {

    if (!is.na(systmeta@fileName)) {
      entity@entityName <- new("entityName", systmeta@fileName)
    }
  }

  if (entityType == "otherEntity" && length(slot(entity, "entityType")) == 0) {
    entity@entityType <- "Other"
  }

  return(entity)
}


#' Create EML physical objects for the given set of PIDs
#'
#' This is a wrapper around [sysmeta_to_eml_physical()] which handles the task of
#' creating the EML physical.
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pids (character) The PID of the object to create the sub-tree for.
#'
#' @return (list) A list of otherEntity object(s).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EML physical objects for all the data in a package
#' pkg <- get_package(mn, pid)
#' pid_to_eml_physical(mn, pkg$data)
#' }
pid_to_eml_physical <- function(mn, pids) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pids),
            all(nchar(pids)) > 0)
  names(pids) <- ''  # Named inputs produce a named output list - which is invalid in EML

  sysmeta <- lapply(pids, function(pid) { getSystemMetadata(mn, pid) })
  sysmeta_to_eml_physical(sysmeta)
}


#' Create an EML physical object from system metadata
#'
#' This function creates a pre-canned EML physical object from what's in the
#' System Metadata of an object. Note that it sets an Online Distribution URL
#' of the DataONE v2 resolve service for the PID.
#'
#' @param sysmeta (SystemMetadata) One or more System Metadata objects.
#'
#' @return (list) A list of physical objects for each sysmeta.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EML physical objects for all the data in a package
#' pkg <- get_package(mn, pid)
#' sm <- lapply(pkg$data, function(pid) {
#'   getSystemMetadata(mn, pid)
#' })
#' sysmeta_to_eml_physical(sm)
#' }
sysmeta_to_eml_physical <- function(sysmeta) {
  work <- function(x) {
    phys <- new("physical")
    phys@scope <- new("xml_attribute", "document")

    if (is.na(x@fileName)) {
      phys@objectName <- new("objectName", "NA")
    } else {
      phys@objectName <- new("objectName", x@fileName)
    }

    phys@size <- new("size", format(x@size, scientific = FALSE))
    phys@size@unit <- new("xml_attribute", "bytes")

    phys@authentication <- new("ListOfauthentication", list(new("authentication", x@checksum)))
    phys@authentication[[1]]@method <- new("xml_attribute", x@checksumAlgorithm)

    phys@dataFormat <- new("dataFormat")
    phys@dataFormat@externallyDefinedFormat <- new("externallyDefinedFormat")
    phys@dataFormat@externallyDefinedFormat@formatName <- x@formatId

    phys@distribution <- new("ListOfdistribution", list(new("distribution")))
    phys@distribution[[1]]@scope  <- new("xml_attribute", "document")
    phys@distribution[[1]]@online <- new("online")
    phys@distribution[[1]]@online@url <- new("url", paste0("https://cn.dataone.org/cn/v2/resolve/", x@identifier))

    slot(phys@distribution[[1]]@online@url, "function") <- new("xml_attribute", "download")

    phys
  }

  if (!is(sysmeta, "list")) sysmeta <- list(sysmeta)

  lapply(sysmeta, work)
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


#' Add a methods step
#'
#' Add a methods step to an EML document.
#'
#' @param doc (eml) The EML document to add the method step to.
#' @param title (character) The title of the method step.
#' @param description (character) The description of the method.
#'
#' @return (eml) The modified EML document.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml <- read_eml("~/Documents/metadata.xml")
#' eml <- add_methods_step(eml, "Field Sampling", "Samples were
#' collected using a niskin water sampler.")
#' }
add_methods_step <- function(doc, title, description) {
  stopifnot(is(doc, "eml"))
  stopifnot(is(doc@dataset, "dataset"))
  stopifnot(is.character(title),
            nchar(title) > 0)
  stopifnot(is.character(description),
            nchar(description) > 0)

  new_step <- new("methodStep",
                  description  = new("description",
                                     section = new("section", list(newXMLNode("title", title),
                                                                   newXMLNode("para", description)))))

  doc@dataset@methods@methodStep[[length(doc@dataset@methods@methodStep) + 1]] <- new_step

  doc
}


#' Clear all methods
#'
#' Clear all methods from an EML document.
#'
#' @param doc (eml) The document to clear methods from.
#'
#' @return (eml) The modified EML document.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml <- read_eml("~/Documents/metadata.xml")
#' eml <- clear_methods(eml)
#' }
clear_methods <- function(doc) {
  stopifnot(is(doc, "eml"))

  # Clear the methods out
  doc@dataset@methods <- new("MethodsType")

  doc
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
#' @param address (character) The party's address(es).
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
#' eml_party("creator", c("Dominic", "'Dom'"), "Mullen", c("NCEAS", "UCSB"),
#'           c("Data Scientist", "Programmer"))
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

  party <- new(type)

  # Individual Name
  if (!is.null(sur_name)) {
    party@individualName <- c(eml_individual_name(given_names, sur_name))
  }

  # Organization Name
  if (!is.null(organization)) {
    party@organizationName <- new('ListOforganizationName', lapply(organization, function(x) {new('organizationName', .Data = x)}))
  }

  # Position
  if (!is.null(position)) {
    party@positionName <- new('ListOfpositionName', lapply(position, function(x) {new('positionName', .Data = x)}))
  }

  # Email
  if (!is.null(email)) {
    party@electronicMailAddress <- new("ListOfelectronicMailAddress", lapply(email, function(x) { new("electronicMailAddress", .Data = x )}))
  }

  # Address
  if (!is.null(address)) {
    # Upgade to a ListOfaddress if needed
    if (is(address, "address")) {
      address <- c(address)
    }

    party@address <- address
  }

  # Phone
  if (!is.null(phone)) {
    # Upgrade to phone is needed
    if (is.character(phone)) {
      phone <- new("ListOfphone", lapply(phone, function(x) as(x, "phone")))
    }

    # Upgade to a ListOfphone if needed
    if (is(phone, "phone")) {
      phone <- c(phone)
    }

    party@phone <- phone
  }

  # userId
  if (!is.null(userId)) {
    # Warn if the userId doesn't look like an ORCID
    if (!grepl("^https:\\/\\/orcid\\.org", userId)) {
      warning(paste0("The provided `userId` of '", userId, "' does not look like an ORCID and the `userId` argument assumes the given `userId` is an ORCID. ORCIDs should be passed in like https://orcid.org/WWWW-XXXX-YYYY-ZZZZ."))
    }

    party@userId <- c(new("userId", .Data = userId, directory = "https://orcid.org"))
  }

  # Role
  if (!is.null(role)) {
    # Only allow roles to be set if type is associatedParty or personnel
    if (type != "associatedParty" && type != "personnel") {
      stop(call. = FALSE,
           paste0("Setting a role is only valid on an associatedParty or personnel, not a ", type, "."))
    }

    # If type is personnel, role needs to be ListOfrole, otherwise just role
    if (type == "personnel") {
      party@role <- as(lapply(role, as, Class = "role"), "ListOfrole")
    } else {
      party@role <- as(role, "role")
    }
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


#' Create an EML individualName section
#'
#' Create an EML individualName section.
#'
#' @param given_names (character) One or more given names.
#' @param sur_name (character) A sur (last) name.
#'
#' @return (individualName) The new individualName section.
#'
#' @export
#'
#' @examples
#' eml_individual_name("some", "user")
eml_individual_name <- function(given_names=NULL, sur_name) {
  stopifnot(is.character(sur_name) && nchar(sur_name) > 0)

  # Create <individualName>
  indiv_name <- new("individualName")

  if (!is.null(given_names)) {
    stopifnot(all(sapply(given_names, is.character)))
    stopifnot(all(lengths(given_names) > 0))

    givens <- lapply(given_names, function(given_name) {
      x <- new("givenName")
      x@.Data <- given_name
      x
    })

    indiv_name@givenName <- new("ListOfgivenName", givens)
  }

  indiv_name@surName <- new("surName", .Data = sur_name)

  indiv_name
}


#' Create an EML project section
#'
#' Create an EML project section.
#'
#' Note - studyAreaDescription, designDescription, and relatedProject are not
#' fully fleshed out. Need to pass these objects in directly if you want to use
#' them.
#'
#' @param title (character) Title of the project (Required). May have multiple titles.
#' @param personnelList (list of personnel) Personnel involved with the project.
#' @param abstract (character) Project abstract. Can pass as a character vector
#'   for separate paragraphs.
#' @param funding (character) Funding sources for the project such as grant and
#'   contract numbers. Can pass as a character vector for separate paragraphs.
#' @param studyAreaDescription (studyAreaDescription)
#' @param designDescription (designDescription)
#' @param relatedProject (project)
#'
#' @return (project) The new project section.
#'
#' @export
#'
#' @examples
#' proj <- eml_project(c("Some title", "A second title if needed"),
#'            c(eml_personnel("Bryce", "Mecum", role = "principalInvestigator")),
#'            c("Abstract paragraph 1", "Abstract paragraph 2"),
#'            "Funding Agency: Award Number 12345")
eml_project <- function(title,
                        personnelList,
                        abstract = NULL,
                        funding = NULL,
                        studyAreaDescription = NULL,
                        designDescription = NULL,
                        relatedProject = NULL) {

  stopifnot(is.character(title),
            length(title) > 0,
            all(nchar(title)) > 0)
  stopifnot(length(personnelList) > 0)

  # Project
  project <- new("project")

  # Title
  titles <- lapply(title, function(x) { as(x, "title") })
  project@title <- as(titles, "ListOftitle")

  # Personnel
  if (!all(sapply(personnelList, function(x) { is(x, "personnel") }))) {
    stop(call. = FALSE,
         "All personnel in the list must be of type 'personnel'")
  }

  project@personnel <- as(personnelList, "ListOfpersonnel")

  # Abstract
  if (!is.null(abstract)) {
    abstract_paras <- lapply(abstract, function(x) {
      as(list(xml2::xml_new_root("para", as.character(x))), "para")
    })
    project@abstract@para <- as(abstract_paras, "ListOfpara")
  }

  # Funding
  if (!is.null(funding)) {
    funding_paras <- lapply(funding, function(x) {
      as(list(xml2::xml_new_root("para", as.character(x))), "para")
    })
    project@funding@para <- as(funding_paras, "ListOfpara")
  }

  # Study area description
  if (!is.null(studyAreaDescription)) {
    project@studyAreaDescription <- studyAreaDescription
  }

  # Design description
  if (!is.null(designDescription)) {
    project@designDescription <- designDescription
  }

  # Related Project
  if (!is.null(relatedProject)) {
    project@relatedProject <- relatedProject
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
#' @param description (character) A textual description.
#' @param north (numeric) North bounding coordinate.
#' @param east (numeric) East bounding coordinate.
#' @param south (numeric) South bounding coordinate.
#' @param west (numeric) West bounding coordinate.
#'
#' @return (geographicCoverage) The new geographicCoverage section.
#'
#' @export
eml_geographic_coverage <- function(description, north, east, south, west) {
  cov <- new("geographicCoverage")

  cov@geographicDescription <- description

  cov@boundingCoordinates@northBoundingCoordinate <- new("northBoundingCoordinate", as.character(north))
  cov@boundingCoordinates@eastBoundingCoordinate <- new("eastBoundingCoordinate", as.character(east))
  cov@boundingCoordinates@southBoundingCoordinate <- new("southBoundingCoordinate", as.character(south))
  cov@boundingCoordinates@westBoundingCoordinate <- new("westBoundingCoordinate", as.character(west))

  cov
}


#' Create an EML address element
#'
#' A simple way to create an EML address element.
#'
#' @param delivery_points (character) One or more delivery points.
#' @param city (character) City.
#' @param administrative_area (character) Administrative area.
#' @param postal_code (character) Postal code.
#'
#' @return (address) An EML address object.
#'
#' @export
#'
#' @examples
#' NCEASadd <- eml_address("735 State St #300", "Santa Barbara", "CA", "93101")
eml_address <- function(delivery_points, city, administrative_area, postal_code) {
  stopifnot(is.character(delivery_points),
            is.character(city),
            is.character(administrative_area),
            (is.character(postal_code) || is.numeric(postal_code)))

  address <- new("address")

  # Delivery point(s)
  dps <- lapply(delivery_points, function(dp) {
    x <- new("deliveryPoint")
    x@.Data <- dp
    x
  })

  # City
  ct <- new("city")
  ct@.Data <- city

  # Administrative area
  aa <- new("administrativeArea")
  aa@.Data <- administrative_area

  # Postal Code
  pc <- new("postalCode")
  pc@.Data <- as.character(postal_code)

  # Put them all together
  address@deliveryPoint <- new("ListOfdeliveryPoint", dps)
  address@city <- ct
  address@administrativeArea <- aa
  address@postalCode <- pc

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
#' @export
#'
#' @examples
#' # Create a new EML document
#' library(EML)
#' doc <- new("eml")
#'
#' # Set an abstract with a single paragraph
#' set_abstract(doc, c("Test abstract..."))
#'
#' # Or one with multiple paragraphs
#' set_abstract(doc, c("First para...", "second para..."))
set_abstract <- function(doc, text) {
  stopifnot(is(doc, "eml"))

  if (length(text) == 1) {
    doc@dataset@abstract <- eml_abstract(text)
  } else if (length(text) > 1) {
    doc@dataset@abstract <- eml_abstract(text)
  }

  doc
}


#' Create an EML abstract
#'
#' Create an EML abstract.
#'
#' @param text (character) Paragraphs of text with one paragraph per element in the
#'   character vector.
#'
#' @return (abstract) An EML abstract.
#'
#' @export
#'
#' @examples
#' # Set an abstract with a single paragraph
#' eml_abstract("Test abstract...")
#'
#' # Or one with multiple paragraphs
#' eml_abstract(c("First para...", "second para..."))
eml_abstract <- function(text) {
  stopifnot(is.character(text),
            length(text) > 0,
            all(nchar(text)) > 0)

  if (length(text) == 1) {
    abstract <- new("abstract", .Data = new("TextType", .Data = "hi"))
  } else if (length(text) > 1) {
    abstract <- new("abstract", para = new("ListOfpara", lapply(text, function(x) new("para", x))))
  }

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
  stopifnot(is(attributes, "attributeList"))

  # Define an interal applyable function to validate each attribute
  eml_validate_attribute <- function(attribute) {
    stopifnot(is(attribute, "attribute"))

    doc@dataset@otherEntity[[1]]@attributeList@attribute[[1]] <- attribute

    # Validate!
    eml_validate(doc)
  }


  # Create a minimum valid EML doc we'll re-use each time we validate a single
  # attribute
  doc <- new("eml", packageId = "test", system = " test")
  doc@dataset@title <- c(new("title", .Data = "test"))
  doc@dataset@creator <- new("ListOfcreator", list(eml_creator("Test", "test")))
  doc@dataset@contact <- new("ListOfcontact", list(eml_contact("Test", "test")))

  # Create a dummy otherEntity with our attributeList
  entity <- new("otherEntity",
                entityName = "name",
                entityType = "type")
  entity@attributeList <- new("attributeList")
  doc@dataset@otherEntity <- new("ListOfotherEntity", list(entity))

  results <- sapply(attributes@attribute, function(attribute) {
    cat(paste0("Validating single attribute '", attribute@attributeName@.Data, "': "))

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

  names(results) <- sapply(attributes@attribute, function(x) x@attributeName)

  results
}


#' Add new entity elements to an EML document from a table
#'
#' Add new entity elements to an EML document from a table.
#'
#' @param doc (eml) An EML document.
#' @param entities (data.frame) A data.frame with columns type, path, pid, and
#'   format_id.
#' @param resolve_base (character) Optional. Specify a DataONE CN resolve base
#'  URI which will be used for serializing download URLs into the EML. Most users
#'  should not override the default value.
#'
#' @return (eml) The modified EML document.
#'
#' @export
#'
#' @examples
#' # Create entities from files on disk
#' \dontrun{
#'   types <- c("dataTable")
#'   paths <- list.files(., full.names = TRUE) # Get full paths to some files
#'   pids <- vapply(paths, function(x) {
#'     paste0("urn:uuid:", uuid::UUIDgenerate())
#'   }, "") # Generate some UUID PIDs
#' Try to guess format IDs, you should check this afterwards
#'   format_ids <- guess_format_id(paths)
#'
#'   entity_df <- data.frame(type = types,
#'                           path = paths,
#'                           pid = pids,
#'                           format_id = format_ids,
#'                           stringsAsFactors = FALSE)
#'
#'   doc <- new("eml")
#'   doc <- eml_add_entities(doc, entity_df)
#'}
#'
#' # Read in a CSV containing the info about files on disk
#' \dontrun{
#'   entity_df <- read.csv("./my_entities.csv", stringsAsFactors = FALSE)
#'   doc <- new("eml")
#'   doc <- eml_add_entities(doc, entity_df)
#' }
eml_add_entities <- function(doc,
                             entities,
                             resolve_base="https://cn.dataone.org/cn/v2/resolve/") {
  stopifnot(is(doc, "eml"))

  if (!is(entities, "data.frame")) {
    stop("The argument 'entities' must be a 'data.frame'.")
  }

  if (!identical(sort(names(entities)), c("format_id", "path", "pid", "type"))) {
    stop("The columns in the data.frame you passed in for the 'entities' argument did not have the expected column names of type, path, pid, format_id and it must.", call. = FALSE)
  }

  entity_types <- c("dataTable", "spatialRaster", "spatialVector", "storedProcedure", "view", "otherEntity")

  if (!all(entities$type %in% entity_types)) {
    stop(call. = FALSE, paste0("The `type` column must only include values from: ", paste(entity_types, collapse = ", "), "."))
  }

  # Warn about existing entities
  for (type in entity_types) {
    if (type %in% entities$type && length(slot(doc@dataset, type)) > 0) {
      warning(paste0("You are adding one or more ", type, " elements. This function only adds entities and does not remove/replace them."))
    }
  }

  # Internal function to create a single entity
  eml_entity <- function(type, path, pid, format_id) {
    # Convert args to character vectors if needed
    if (is.factor(type)) type <- as.character(type)
    if (is.factor(path)) path <- as.character(path)
    if (is.factor(pid)) pid <- as.character(pid)
    if (is.factor(format_id)) format_id <- as.character(format_id)

    stopifnot(file.exists(path))
    stopifnot(is.character(path), nchar(path) > 0)
    stopifnot(is.character(pid), nchar(pid) > 0)
    stopifnot(is.character(format_id), nchar(format_id) > 0)

    file_name <- basename(path)

    entity <- new(type)
    entity@id <- new("xml_attribute", pid)
    entity@scope <- new("xml_attribute", "document")

    entity@entityName <- new("entityName", .Data = file_name)

    if (type == "otherEntity") {
      entity@entityType <- "Other"
    }

    # otherEntity/physical
    physical <- new("physical")
    physical@scope <- new("xml_attribute", "document")
    physical@objectName <- new("objectName", file_name)

    physical@size <- new("size", format(file.size(path), scientific = FALSE))
    physical@size@unit <- new("xml_attribute", "bytes")
    physical@authentication <- new("ListOfauthentication", list(new("authentication", digest::digest(path, algo = "sha1", file = TRUE))))
    physical@authentication[[1]]@method <- new("xml_attribute", "SHA-1")

    physical@dataFormat <- new("dataFormat")
    physical@dataFormat@externallyDefinedFormat <- new("externallyDefinedFormat")
    physical@dataFormat@externallyDefinedFormat@formatName <- format_id

    physical@distribution <- new("ListOfdistribution", list(new("distribution")))
    physical@distribution[[1]]@scope  <- new("xml_attribute", "document")
    physical@distribution[[1]]@online <- new("online")
    physical@distribution[[1]]@online@url <- new("url", paste0(resolve_base, pid))

    slot(physical@distribution[[1]]@online@url, "function") <- new("xml_attribute", "download")

    entity@physical <- new("ListOfphysical", list(physical))

    entity
  }

  # Create new entities
  new_entities <- lapply(entity_types, function(type) {
    lapply(which(entities$type == type), function(i) {
      eml_entity(entities[i, "type"],
                 entities[i,"path"],
                 entities[i,"pid"],
                 entities[i,"format_id"])
    })
  })

  names(new_entities) <- entity_types # Name the list so we can [[ by type

  # Merge new entities into existing
  for (type in entity_types) {
    slot(doc@dataset, type) <- new(paste0("ListOf", type), c(slot(doc@dataset, type),
                                                             new(paste0("ListOf", type), new_entities[[type]])))
  }

  doc
}


#' Convert otherEntities to dataTables
#'
#' Convert an EML 'otherEntity' object to a 'dataTable' object. This will convert an
#' otherEntity object as currently constructed - it does not add a physical or add attributes.
#' However, if these are already in their respective slots, they will be retained.
#'
#' @param eml (S4) An EML S4 object.
#' @param otherEntity (S4 / integer) Either an EML otherEntity object or the index
#'   of an otherEntity within a ListOfotherEntity. Integer input is recommended.
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
#' eml <- read_eml(system.file("example-eml.xml", package = "arcticdatautils"))
#'
#' # The following two calls are equivalent:
#' eml <- eml_otherEntity_to_dataTable(eml, eml@@dataset@@otherEntity[[1]])
#' eml <- eml_otherEntity_to_dataTable(eml, 1)
#'
#' # Integer input is recommended:
#' eml <- eml_otherEntity_to_dataTable(eml, 1)
#' }
eml_otherEntity_to_dataTable <- function(eml, otherEntity, validate_eml = TRUE) {
  ## Argument checks
  stopifnot(methods::is(eml, "eml"))
  stopifnot(any(is.numeric(otherEntity), methods::is(otherEntity, "otherEntity")))
  stopifnot(is.logical(validate_eml))

  ## Handle different inputs for 'otherEntity'
  if (is.numeric(otherEntity)) {
    index <- otherEntity
    otherEntity <- eml@dataset@otherEntity[[index]]
  } else {
    index <- which_in_eml(eml@dataset@otherEntity,
                          "entityName",
                          otherEntity@entityName)
    if (length(index) > 1) {
      stop("Duplicate 'entityName' found in 'eml@dataset@otherEntity', please use a numeric index (1, 2, etc.) to specify which 'otherEntity' you would like to convert.")
    }
  }

  ## convert otherEntity to dataTable
  dt <- utils::capture.output(otherEntity) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("otherEntity", "dataTable") %>%
    paste(sep = "", collapse = "") %>%
    EML::read_eml()

  ## Add dt to bottom of dt list
  type <- "dataTable"
  slot(eml@dataset, type) <- new(paste0("ListOf", type), c(slot(eml@dataset, type),
                                                           new(paste0("ListOf", type), list(dt))))

  ## delete otherEntity from list
  eml@dataset@otherEntity[[index]] <- NULL

  ## return eml
  if (validate_eml == TRUE) {
    eml_validate(eml)
  }
  return(eml)
}


#' Search through EMLs
#'
#' This function returns indices within an EML list that contain an instance where
#' `test == TRUE`. See examples for more information.
#'
#' @param eml_list (S4/List) An EML list object.
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
#' n <- which_in_eml(eml@@dataset@@creator, "surName", "Smith")
#' # Answer: eml@@dataset@@creator[n]
#'
#' # Question: Which dataTables have an entityName that begins with "2016"
#' n <- which_in_eml(eml@@dataset@@dataTable, "entityName", function(x) {grepl("^2016", x)})
#' # Answer: eml@@dataset@@dataTable[n]
#'
#' # Question: Which attributes in dataTable[[1]] have a numberType "natural"?
#' n <- which_in_eml(eml@@dataset@@dataTable[[1]]@@attributeList@@attribute, "numberType", "natural")
#' # Answer: eml@@dataset@@dataTable[[1]]@@attributeList@@attribute[n]
#'
#' #' # Question: Which dataTables have at least one attribute with a numberType "natural"?
#' n <- which_in_eml(eml@@dataset@@dataTable, "numberType", function(x) {"natural" %in% x})
#' # Answer: eml@@dataset@@dataTable[n]
#' }
which_in_eml <- function(eml_list, element, test) {

  stopifnot(isS4(eml_list))
  stopifnot(methods::is(eml_list,"list"))
  stopifnot(is.character(element))

  if (is.character(test)) {
    value = test
    test = function(x) {x == value}

  } else {
    stopifnot(is.function(test))
  }

  # Find location
  location <- unlist(lapply(seq_along(eml_list), function(i) {
    elements_test <- unlist(EML::eml_get(eml_list[[i]], element))

    if (is.null(elements_test)) {
      out <- NULL

    } else {
      result <- test(elements_test)

      if (length(result) > 1) {
        stop("Test must only return one value.")

      } else if (result == TRUE) {
        out <- i

      } else {
        out <- NULL
      }
    }
    return(out)
  }))

  return(location)
}


#' Set a reference to an EML object
#'
#' This function creates a new object with the same class as \code{element_to_replace}
#' using a reference to \code{element_to_reference}.
#'
#' @param element_to_reference (S4) An EML object to reference.
#' @param element_to_replace (S4) An EML object to replace with a reference.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' eml <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#'
#' # Set the first contact as a reference to the first creator
#' eml@@dataset@@contact[[1]] <- eml_set_reference(eml@@dataset@@creator[[1]],
#' eml@@dataset@@contact[[1]])
#'
#' # This is also useful when we want to set references to a subset of 'dataTable'
#'   or 'otherEntity' objects
#' # Add a few more objects first to illustrate the use:
#' eml@@dataset@@dataTable[[3]] <- eml@@dataset@@dataTable[[1]]
#' eml@@dataset@@dataTable[[4]] <- eml@@dataset@@dataTable[[1]]
#' # Add references to the second and third elements only (not the 4th):
#' for (i in 2:3) {
#'     eml@@dataset@@dataTable[[i]]@@attributeList <- eml_set_reference(eml@@dataset@@dataTable[[1]]@@attributeList,
#'                                                       eml@@dataset@@dataTable[[i]]@@attributeList)
#' }
#' # If we print the entire 'dataTable' list we see elements 2 and 3 have references while 4 does not.
#' eml@@dataset@@dataTable
#' }
eml_set_reference <- function(element_to_reference, element_to_replace) {
  if (length(element_to_reference@id) == 0) {
    stop('No id detected at element_to_reference@id. Please add an id in order to use references.')
  }
  id <- element_to_reference@id[1]
  class <- class(element_to_replace)[1]
  element_to_replace <- new(class, reference = id)
  return(element_to_replace)
}


#' Set shared attribute references
#'
#' This function sets shared attributes using the attributes of the first \code{type}
#' selected and creates references for all remaining objects of equivalent \code{type}.
#'
#' @param eml (eml) An EML object.
#' @param attributeList (attributeList) Optional. An EML attributeList object. If not provided
#'   then it will default to the attributeList of the first \code{type} element.
#' @param type (character) Optional. Specifies whether to replace 'dataTable' or 'otherEntity' attributeList
#'   objects with references. Defaults to 'dataTable'.
#'
#' @return (eml) The modified EML document.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' eml <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#' atts <- EML::set_attributes(EML::get_attributes(eml@@dataset@@dataTable[[1]]@@attributeList)$attributes)
#'
#' eml <- eml_set_shared_attributes(eml, atts, type = 'dataTable')
#' }
eml_set_shared_attributes <- function(eml, attributeList = NULL, type = 'dataTable') {
  stopifnot(methods::is(eml, 'eml'))
  if (!is.null(attributeList)) {
    stopifnot(methods::is(attributeList, 'attributeList'))
  }
  stopifnot(type %in% c('dataTable', 'otherEntity'))

  x <- slot(eml@dataset, type)
  n <- length(x)
  if (n <= 1) {
    stop('1 or fewer entities') # add message
  }

  # If a new attributeList is provided set it
  if (!is.null(attributeList)) {
    x[[1]]@attributeList <- attributeList
  }
  x[[1]]@attributeList@id <- new('xml_attribute', uuid::UUIDgenerate(TRUE))
  # Apply references to all other elements
  for (i in 2:n) {
    x[[i]]@attributeList <- eml_set_reference(x[[1]]@attributeList, x[[i]]@attributeList)
  }

  slot(eml@dataset, type) <- x
  return(eml)
}
