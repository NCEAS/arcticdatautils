#' eml.R
#'
#' Helpers for creating EML.


#' Create an EML otherEntity sub-tree for the given PID.
#'
#' Note this is a wrapper around sysmeta_to_entity which handles the task of
#' creating the EML otherEntity sub-ree.
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pid (character) The PID of the object to create the sub-tree for.
#' @param sysmeta (SystemMetadata) Optional. Manually pass in System Metadata. This avoids an extra network request if the calling environment has already loaded the System Metadata.
#'
#' @return (otherEntity) The XML sub-tree.
#' @export
#'
#' @examples
pid_to_entity <- function(mn, pid, sysmeta=NULL) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  # Get the System Metadata if needed
  if (is.null(sysmeta)) {
    sysmeta <- dataone::getSystemMetadata(mn, pid)
  }

  if (is.null(sysmeta)) {
    stop("Failed to download System Metadata for PID '", pid, "'.")
  }

  # Check if the sysmeta has a fileName and stop execution if it does not
  if (is.na(sysmeta@fileName)) {
    stop(paste0("System Metadata for object with PID '", pid, "' did not have its fileName property set. This will result in 'NA' being set for the EML entityName and objectName (which we don't want). You need to give each data object a fileName property in its System Metadata. You can use the arcticdatautils::set_file_name() function to do this or you can use dataone::getSystemMetadata(), change the fileName property, and update it with dataone::updateSystemMetadata()"))
  }

  sysmeta_to_entity(sysmeta)
}

#' Create an EML otherEntity sub-tree for the given object.
#'
#' @param sysmeta (SystemMetadata) A SystemMedata instance for the object.
#'
#' @return (otherEntity) The XML sub-tree.
#' @export
#'
#' @examples
sysmeta_to_entity <- function(sysmeta) {
  stopifnot(class(sysmeta) == "SystemMetadata")

  # otherEntity
  other_entity <- new("otherEntity")
  other_entity@id <- new("xml_attribute", sysmeta@identifier)
  other_entity@scope <- new("xml_attribute", "document")

  if (is.na(sysmeta@fileName)) {
    other_entity@entityName <- new("entityName", "NA")
  }
  else {
    other_entity@entityName <- new("entityName", sysmeta@fileName)
  }

  other_entity@entityType <- "Other"

  # otherEntity/physical
  phys <- new("physical")
  phys@scope <- new("xml_attribute", "document")

  if (is.na(sysmeta@fileName)) {
    phys@objectName <- new("objectName", "NA")
  } else {
    phys@objectName <- new("objectName", sysmeta@fileName)
  }

  phys@size <- new("size", format(sysmeta@size, scientific = FALSE))
  phys@authentication <- new("ListOfauthentication", list(new("authentication", sysmeta@checksum)))
  phys@authentication[[1]]@method <- new("xml_attribute", sysmeta@checksumAlgorithm)

  phys@dataFormat <- new("dataFormat")
  phys@dataFormat@externallyDefinedFormat <- new("externallyDefinedFormat")
  phys@dataFormat@externallyDefinedFormat@formatName <- sysmeta@formatId

  phys@distribution <- new("ListOfdistribution", list(new("distribution")))
  phys@distribution[[1]]@scope  <- new("xml_attribute", "document")
  phys@distribution[[1]]@online <- new("online")
  phys@distribution[[1]]@online@url <- new("url", paste0("ecogrid://knb/", get_doc_id(sysmeta)))

  slot(phys@distribution[[1]]@online@url, "function") <- new("xml_attribute", "download")

  other_entity@physical <- new("ListOfphysical", list(phys))

  other_entity
}


#' Creates and adds EML otherEntity elements to an existing EML document.
#'
#' This function isn't that smart. It will remove existing otherEntity elements
#' if what's in their 'id' attribute isn't `pids`. It will then go on to add
#' any otherEntity elements it needs. If your EML document doesn't store the
#' PID for the otherEntity in the 'id' attribute, you may be confused when your
#' otherEntity is removed and then added back. In theory, this isn't too bad
#' but, in practice, this slows down the execution of the function because each
#' new otherEntity element requires a network call to find its 'docid'.
#'
#' @param mn (MNode) The Member Node the objects exist on.
#' @param path (character) The location on disk of the EML file.
#' @param pids (character) One or more PIDs for the objects.
#'
#' @return (character) The path to the updated EML file.
#' @export
#'
#' @examples
add_other_entities <- function(mn, path, pids) {
  stopifnot(class(mn) == "MNode")
  stopifnot(file.exists(path))
  stopifnot(all(is.character(pids)),
            all(nchar(pids) > 0))

  if (length(pids) == 0) {
    message("Skipped adding EML otherEntity elements because no pids were specified.")
    return(path)
  }

  # Get the metadata document from the MN and load it as an EML document
  doc <- EML::read_eml(path)
  stopifnot(class(doc) == "eml")

  message("Adding EML otherEntity elements...")

  current_entity_pids <- vapply(doc@dataset@otherEntity, function(x) x@id, "", USE.NAMES = FALSE)

  # Filter out any otherEntity elements for PIDs not in the `pids` argument
  filtered_other_entities <- Filter(function(x) { (x@id %in% pids)}, doc@dataset@otherEntity)
  doc@dataset@otherEntity <- new("ListOfotherEntity", filtered_other_entities)

  # Add any new otherEntity elements that weren't already in the EML
  new_entities <- lapply(pids[!(pids %in% current_entity_pids)], function(pid) pid_to_entity(mn, pid))

  if (length(new_entities) > 0) {
    doc@dataset@otherEntity <- new("ListOfotherEntity", c(doc@dataset@otherEntity, new_entities))
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
#' @export
#'
#' @examples
get_doc_id <- function(sysmeta) {
  stopifnot(class(sysmeta) == "SystemMetadata")

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
clear_methods <- function(doc) {
  stopifnot(is(doc, "eml"))

  # Clear the methods out
  doc@dataset@methods <- new("MethodsType")

  doc
}

#' Create an EML creator subtree from a first, last, and email.
#
#' @param given_names (character) One or more given (first) names.
#' @param sur_name (character) A sur (last) name.
#' @param organization (character) One or more organization names.
#' @param email (character) An email address.
#' @param phone (character) A phone number.
#' @param address (address) An object of type 'address' (EML).
#'
#' @return (creator) The new creator element.
#' @export
#'
#' @examples
#' creator("test", "user", test@user.com")
eml_creator <- function(given_names, sur_name, organization=NULL, email=NULL, phone=NULL, address=NULL) {
  stopifnot(all(sapply(c(given_names, sur_name), is.character)),
            all(lengths(c(given_names, sur_name)) > 0))
  if (!is.null(address)) stopifnot(inherits(address, "address"))

  # Create <creator>
  creator <- new("creator")

  # Individual Name
  creator@individualName <- new("ListOfindividualName", list(eml_individual_name(given_names, sur_name)))

  # Organization Name
  if (!is.null(organization)) {
    org <- new("organizationName")
    org@.Data <- organization
    creator@organizationName <- new("ListOforganizationName", list(org))
  }

  # Email
  if (!is.null(email)) {
    email_address <- new("electronicMailAddress")
    email_address@.Data <- email

    creator@electronicMailAddress <- new("ListOfelectronicMailAddress", list(email_address))
  }

  # Phone
  if (!is.null(phone)) {
    phn <- new("phone")
    phn@phonetype <- new("xml_attribute", "voice")
    phn@.Data <- phone

    creator@phone <- new("ListOfphone", list(phn))
  }

  # Address
  if (!is.null(address)) {
    creator@address <- new("ListOfaddress", list(address))
  }

  creator
}

#' Create an EML contact subtree from a first, last, and email.
#'
#' @param given_names (character) One or more given (first) names.
#' @param sur_name (character) A sur (last) name.
#' @param organization (character) One or more organization names.
#' @param email (character) An email address.
#' @param phone (character) A phone number.
#' @param address (address) An object of type 'address' (EML).
#'
#' @return (contact) The new contact sub-tree.
#' @export
#'
#' @examples
#' eml_contact("test", "user", test@user.com")
eml_contact <- function(given_names, sur_name, organization=NULL, email=NULL, phone=NULL, address=NULL) {
  stopifnot(all(sapply(c(given_names, sur_name), is.character)),
            all(lengths(c(given_names, sur_name)) > 0))
  if (!is.null(address)) stopifnot(inherits(address, "address"))

  # Create <contact>
  contact <- new("contact")

  # Individual Name
  contact@individualName <- new("ListOfindividualName", list(eml_individual_name(given_names, sur_name)))

  # Organization Name
  if (!is.null(organization)) {
    org <- new("organizationName")
    org@.Data <- organization
    contact@organizationName <- new("ListOforganizationName", list(org))
  }

  # Email
  if (!is.null(email)) {
    email_address <- new("electronicMailAddress")
    email_address@.Data <- email

    contact@electronicMailAddress <- new("ListOfelectronicMailAddress", list(email_address))
  }

  # Phone
  if (!is.null(phone)) {
    phn <- new("phone")
    phn@phonetype <- new("xml_attribute", "voice")
    phn@.Data <- phone

    contact@phone <- new("ListOfphone", list(phn))
  }

  # Address
  if (!is.null(address)) {
    contact@address <- new("ListOfaddress", list(address))
  }

  contact
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
#' eml_individual_name("some", "user)
eml_individual_name <- function(given_names, sur_name) {
  stopifnot(all(sapply(c(given_names, sur_name), is.character)),
            all(lengths(c(given_names, sur_name)) > 0))

  givens <- lapply(given_names, function(given_name) {
    x <- new("givenName")
    x@.Data <- given_name
    x
  })

  sur <- new("surName")
  sur@.Data <- sur_name

  # Create <individualName>
  indiv_name <- new("individualName")
  indiv_name@givenName <- new("ListOfgivenName", givens)
  indiv_name@surName <- sur

  indiv_name
}


#' Create an eml-project section.
#'
#' Note: This is super-limited right now.
#'
#' @param title (character) Title of the project.
#' @param awards (character) One or more awards for the project.
#' @param first (character) First name of the person with role `role`.
#' @param last (character) Last name of the person with role `role`.
#' @param organizations (character) Optional. One or more organization strings.
#' @param role (character) Optional. Specify an alternate role.
#'
#' @return (project) The new project section.
#' @export
#'
#' @examples
#' eml_project("Some title", "51231", "Some", "User")
eml_project <- function(title, awards, first, last, organizations = NULL, role = "originator") {
  stopifnot(all(sapply(c(title, awards, first, last), is.character)),
            all(lengths(c(title, awards, first, last)) > 0))

  # project
  project <- new("project")

  # title
  title_ele <- new("title")
  title_ele@.Data <- title
  project@title <- new("ListOftitle", list(title_ele))

  # personnel
  personnel <- new("personnel")

  # individualName
  personnel@individualName <- new("ListOfindividualName", list(eml_individual_name(first, last)))

  # organizationName
  if (!is.null(organizations)) {
    organizations <- lapply(organizations, function(org) { o <- new("organizationName"); o@.Data <- org; o } )
    personnel@organizationName <- new("ListOforganizationName", organizations)
  }

  # role
  personnel@role <- new("ListOfrole", list(new("role", role)))

  project@personnel <- new("ListOfpersonnel", list(personnel))

  # funding
  funding_paras <- lapply(awards, function(awd) { a <- new("para"); a@.Data <- list(awd); a } )
  project@funding@para <- new("ListOfpara", funding_paras)

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
