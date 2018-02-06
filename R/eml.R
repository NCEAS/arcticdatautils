#' eml.R
#'
#' Helpers for creating EML.


#' Create EML otherEntity objects for a set of PIDs
#'
#' Note this is a wrapper around sysmeta_to_other_entity which handles the task of
#' creating the EML otherEntity.
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pids (character) The PID of the object to create the sub-tree for.
#'
#' @return (list of otherEntity) The otherEntity object(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EML otherEntity objects for all the data in a package
#' pkg <- get_package(mn, pid)
#' pid_to_other_entity(mn, pkg$data)
#' }
pid_to_eml_other_entity <- function(mn, pids) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pids),
            all(nchar(pids)) > 0)

  sysmeta <- lapply(pids, function(pid) { getSystemMetadata(mn, pid) })
  sysmeta_to_eml_other_entity(sysmeta)
}

#' Create EML physical objects for the given set of PIDs
#'
#' Note this is a wrapper around sysmeta_to_eml_physical which handles the task of
#' creating the EML physical
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pids (character) The PID of the object to create the sub-tree for.
#'
#' @return (list of otherEntity) The otherEntity object(s)
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

  sysmeta <- lapply(pids, function(pid) { getSystemMetadata(mn, pid) })
  sysmeta_to_eml_physical(sysmeta)
}

#' Create an EML otherEntity for the given object from the System Metadata
#'
#' @param sysmeta (SystemMetadata) One or more System Metadata objects
#'
#' @return (list of otherEntity) The otherEntity object(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate EML otherEntity objects for all the data in a package
#' pkg <- get_package(mn, pid)
#' sm <- lapply(pkg$data, function(pid) { getSystemMetadata(mn, pid) })
#' sysmeta_to_other_entity(sm)
#'}
sysmeta_to_eml_other_entity <- function(sysmeta) {
  work <- function(x) {
    other_entity <- new("otherEntity")
    other_entity@id <- new("xml_attribute", x@identifier)
    other_entity@scope <- new("xml_attribute", "document")

    if (is.na(x@fileName)) {
      other_entity@entityName <- new("entityName", "NA")
    }
    else {
      other_entity@entityName <- new("entityName", x@fileName)
    }

    other_entity@entityType <- "Other"

    phys <- sysmeta_to_eml_physical(x)
    other_entity@physical <- new("ListOfphysical", phys)

    other_entity
  }


  if (!is(sysmeta, "list")) sysmeta <- list(sysmeta)

  lapply(sysmeta, work)
}


#' This function is deprecated. See \link{sysmeta_to_eml_other_entity}.
#'
#' @param sysmeta (SystemMetadata) A SystemMetadata object
#'
sysmeta_to_other_entity <- function(sysmeta) {
  .Deprecated("sysmeta_to_eml_other_entity",
              package = "arcticdtautils",
              old = "sysmeta_to_other_entity")
}

#' Create an EML physical object from System Metadata
#'
#' This function creates a pre-canned EML physical object from what's in the
#' System Metadata of an Object. Note that it sets an Online Distrubtion URL
#' of the DataONE v2 resolve service for the PID.
#'
#' @param sysmeta (SystemMetadata) One or more System Metadata objects
#'
#' @return (list of physical) The physical objects for each sysmeta
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

#' Creates and sets EML otherEntity elements to an existing EML document,
#' replacing any existing otherEntities
#'
#' This function is slow because it needs get the System Metadata for each
#' element of `pids` in order to get the fileName, checksum, etc.
#'
#' @param mn (MNode) The Member Node the objects exist on.
#' @param path (character) The location on disk of the EML file.
#' @param pids (character) One or more PIDs for the objects.
#'
#' @return (character) The path to the updated EML file.
#' @export
#'
#' @examples
#' \dontrun{
#' mn <- MNode(...) # Set up a connection to an MN
#' eml_path <- "/path/to/your/eml.xml"
#' set_other_entities(mn, eml_path, "a_data_pid")
#' }
set_other_entities <- function(mn, path, pids) {
  stopifnot(is(mn, "MNode"))
  stopifnot(file.exists(path))
  stopifnot(all(is.character(pids)),
            all(nchar(pids) > 0))

  if (length(pids) == 0) {
    message("Skipped adding EML otherEntity elements because no pids were specified.")
    return(path)
  }

  # Get the metadata document from the MN and load it as an EML document
  doc <- EML::read_eml(path)
  stopifnot(is(doc, "eml"))

  message("Setting EML otherEntity elements. This can take a while if there are lots of PIDs...")

  # Generate otherEntity elements
  other_entities <- pid_to_eml_other_entity(mn, pids)

  # Concatenate the existing and new otherEntity elements and put back in the
  # EML
  if (length(other_entities) > 0) {
    doc@dataset@otherEntity <- new("ListOfotherEntity", other_entities)
  }

  # Write the modified document back to disk and stop
  EML::write_eml(doc, path)
  stopifnot(EML::eml_validate(path) == TRUE)

  path
}

#' Get the Metacat docid for the given identifier
#'
#' @param sysmeta (SystemMetadata) The sysmeta of the object you want to find.
#'
#' @return (character) The docid
#'
#'
#'
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

#' Adds a step to the methods document
#'
#' @param doc (eml) The EML document to add the method step to.
#' @param title (character) The title of the method step.
#' @param description (character) The description of the method.
#'
#' @return (eml) The modified EML document
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

#' Clear all methods from the document.
#'
#' @param doc (eml) The document to clear methods from.
#'
#' @return (eml) The modified document.
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

#' Low-level helper for creating EML parties
#'
#' You usually will want to use the high-level functions such as
#'\code{\link{eml_creator}} and \code{\link{eml_contact}} but using this is
#' fine.
#'
#' The \code{userId} argument assumes an ORCID so be sure to adjust for that.
#'
#' @param type (character) The type of party (e.g. 'contact')
#' @param given_names (character) The party's given name(s)
#' @param sur_name (character) The party's surname
#' @param organization (character) The party's organization name
#' @param position (character) The party's position
#' @param email (character) The party's email address(es)
#' @param phone (character) The party's phone number(s)
#' @param address (character) The party's address(es)
#' @param userId (character) The party's ORCID, in format https://orcid.org/WWWW-XXXX-YYYY-ZZZZ
#' @param role (character) The party's role
#'
#' @return An instance of the party specified by the in \code{type} argument
#' @export
#'
#' @examples
#' eml_party("creator", "Test", "User")
#' eml_party("creator", "Bryce", "Mecum", userId = "https://orcid.org/0000-0002-0381-3766")
eml_party <- function(type="associatedParty",
                      given_names=NULL,
                      sur_name=NULL,
                      organization=NULL,
                      position=NULL,
                      email=NULL,
                      phone=NULL,
                      address=NULL,
                      userId=NULL,
                      role=NULL) {
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
    party@organizationName <- c(new("organizationName", .Data = organization))
  }

  # Position
  if (!is.null(position)) {
    party@positionName <- c(new("positionName", .Data = position))
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
#' See \code{\link{eml_party}} for details.
#'
#' @param ... Arguments passed on to eml_party
#'
#' @return (creator) The new creator
#' @export
#'
#' @examples
#' eml_creator("test", "user", email = "test@user.com")
eml_creator <- function(...) {
  eml_party("creator", ...)
}

#' Create an EML contact
#'
#' See \code{\link{eml_party}} for details.
#'
#' @param ... Arguments passed on to eml_party
#'
#' @return (contact) The new contact
#' @export
#'
#' @examples
#' eml_contact("test", "user", email = "test@user.com")
eml_contact <- function(...) {
  eml_party("contact", ...)
}


#' Create an EML metadataProvider
#'
#' See \code{\link{eml_party}} for details.
#'
#' @param ... Arguments passed on to eml_party
#'
#' @return (metadataProvider) The new metadataProvider
#' @export
#'
#' @examples
#' eml_metadata_provider("test", "user", email = "test@user.com")
eml_metadata_provider <- function(...) {
  eml_party("metadataProvider", ...)
}

#' Create an EML associatedParty
#'
#' See \code{\link{eml_party}} for details.
#'
#' @param ... Arguments passed on to eml_party
#'
#' @return (associatedParty) The new associatedParty
#' @export
#'
#' @examples
#' eml_associated_party("test", "user", email = "test@user.com", role = "Principal Investigator")
eml_associated_party <- function(...) {
  eml_party("associatedParty", ...)
}

#' Create an EML personnel
#'
#' See \code{\link{eml_party}} for details.
#'
#' @param ... Arguments passed on to eml_party
#' @param role (character) Personnel role, eg "principalInvestigator"
#' @return (personnel) The new personnel
#' @export
#'
#' @examples
#' eml_personnel("test", "user", email = "test@user.com", role = "principalInvestigator")
eml_personnel <- function(role = NULL, ...) {
  if(is.null(role)) {
    stop(call. = FALSE,
         "You must specify a role for a personnel.")
  }

  eml_party("personnel", role = role, ...)
}

#' Create an EML individualName section
#'
#' @param given_names (character) One or more given names.
#' @param sur_name (character) A sur (last) name.
#'
#' @return (individualName) The new individualName section
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

#' Create an eml-project section.
#'
#' Note - studyAreaDescription, designDescription, and relatedProject are not
#' fully fleshed out. Need to pass these objects in directly if you want to use
#' them.
#'
#' @param title (character) Title of the project (Required).
#' @param personnelList (list of personnel) Personnel involved with the project.
#' @param abstract (character) Project abstract. Can pass as a character vector
#' for separate paragraphs.
#' @param funding (character) Funding sources for the project such as grant and
#' contract numbers. Can pass as a character vector for separate paragraphs.
#' @param studyAreaDescription (studyAreaDescription)
#' @param designDescription (designDescription)
#' @param relatedProject (project)
#'
#' @return (project) The new project section.
#' @export
#'
#' @examples
#' proj <- eml_project("Some title",
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
            nchar(title) > 0)
  stopifnot(length(personnelList) > 0)

  # Project
  project <- new("project")

  # Title
  project@title <- c(as(title, "title"))

  # Personnel
  if(!all(sapply(personnelList, function(x) { is(x, "personnel") }))) {
    stop(call. = FALSE,
         "All personnel in the list must be of type 'personnel'")
  }

  project@personnel <- as(personnelList, "ListOfpersonnel")

  # Abstract
  if(!is.null(abstract)) {
    abstract_paras <- lapply(abstract, function(x) {
      as(list(xml2::xml_new_root("para", as.character(x))), "para")
    })
    project@abstract@para <- as(abstract_paras, "ListOfpara")
  }

  # Funding
  if(!is.null(funding)) {
    funding_paras <- lapply(funding, function(x) {
      as(list(xml2::xml_new_root("para", as.character(x))), "para")
    })
    project@funding@para <- as(funding_paras, "ListOfpara")
  }

  # Study area description
  if(!is.null(studyAreaDescription)) {
    project@studyAreaDescription <- studyAreaDescription
  }

  # Design description
  if(!is.null(designDescription)) {
    project@designDescription <- designDescription
  }

  # Related Project
  if(!is.null(relatedProject)) {
    project@relatedProject <- relatedProject
  }

  project
}


eml_geographic_coverage <- function(description, north, east, south, west) {
  cov <- new("geographicCoverage")

  cov@geographicDescription <- description

  cov@boundingCoordinates@northBoundingCoordinate <- new("northBoundingCoordinate", as.character(north))
  cov@boundingCoordinates@eastBoundingCoordinate <- new("eastBoundingCoordinate", as.character(east))
  cov@boundingCoordinates@southBoundingCoordinate <- new("southBoundingCoordinate", as.character(south))
  cov@boundingCoordinates@westBoundingCoordinate <- new("westBoundingCoordinate", as.character(west))

  cov
}


#' Create an EML address element.
#'
#' @param delivery_points (character) One or more delivery points.
#' @param city (character) City
#' @param administrative_area (character) Administrative area
#' @param postal_code (character) Postal code
#'
#' @return (address) An EML address object.
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



#' Set the abstract on an EML document
#'
#' @param doc (eml) An EML document
#' @param text (character) The abstract text. If \code{text} is length one, an
#' abstract without \code{<para>} or \code{section} elements will be created.
#' If \code{text} is greater than one in length, \code{para} elementes will be
#' used for each element.
#'
#' @return (eml) The modified EML document
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


#' Minimalistic helper function to generate EML abstracts
#'
#' @param text (character) Paragraphs of text, one paragraph per element in the
#' character vector
#'
#' @return (abstract) An EML abstract
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
#' with any and all validation issues.
#'
#' This is done by, for each attribute in the list, creating a minimum valid
#' EML document and adding a new otherEntity with a new attributeList containing
#' the single attribute to be validated.
#'
#' @param attributes (attributeList) An attributeList
#'
#' @return (boolean) Named vector of TRUE/FALSE indicating which attributes
#' are valid
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


#' Add new entity (otherEntity, dataTable, etc) elements to an EML document from a table.
#'
#' @param doc (eml) An EML document
#' @param entities (data.frame) A data.frame with columns type, path, pid, and
#' format_id
#' @param resolve_base (character) Optional. Specify a DataONE CN resolve base
#' URI which will be used for serializing download URLs into the EML. Most users
#'  should not override the default value.
#'
#' @return (eml) The modified EML document.
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
  for(type in entity_types) {
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

