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
  other_entity@id <- new("xml_attribute", sysmeta@checksum)
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

  log_message("Adding EML otherEntity elements...")

  # Get the metadata document from the MN and load it as an EML document
  doc <- EML::read_eml(path)
  stopifnot(class(doc) == "eml")

  # Check for any existing otherEntity elements and quit if any are found
  if (length(doc@dataset@otherEntity) != 0) {
    log_message(paste0("Metadata file already contains one or more ",
                       "otherEntity elements. All existing otherEntity ",
                      "elements will be removed. If what you passed to ",
                      "data_old_pids was the complete list of data objects ",
                      "for the new package everythng is probably just fine."))
  }

  # Create the otherEntity elements as a list
  other_entities <- lapply(pids, function(pid) {
    pid_to_entity(mn, pid)
  })
  stopifnot(length(other_entities) > 0)

  doc@dataset@otherEntity <- new("ListOfotherEntity", other_entities)
  stopifnot(length(doc@dataset@otherEntity) == length(pids))

  # Write the modified document back to disk and stop if it isn't valid
  EML::write_eml(doc, path)
  stopifnot(EML::eml_validate(path) == TRUE)

  path
}

#' Get the Metacat docid for the given identifier
#'
#' @param sysmeta (SystemMetadata) The sysmeta of the object you want to find
#'
#' @return (character) The docid
#' @export
#'
#' @examples
get_doc_id <- function(sysmeta) {
  stopifnot(class(sysmeta) == "SystemMetadata")

  # Hack: Determine whether we should check production or dev Metacat
  if (sysmeta@originMemberNode == "urn:node:ARCTIC") {
    metacat_base_url <- "https://arcticdata.io/metacat/metacat"
  } else {
    metacat_base_url <- "https://dev.nceas.ucsb.edu/knb/metacat"
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
#' @param doc (eml) The EML document to add the method step to
#' @param title (character) The title of the method step
#' @param description (character) The description of the method
#'
#' @return (eml) The modified EML document
#' @export
#'
#' @examples
#' \dontrun{
#' # Add a method step to a fresh document
#' my_doc <- new("eml")
#' my_doc <- add_methods_step(my_doc, "some method", "how I did the method")
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
#' # First we create a new EML document and add a method
#' doc <- new("eml")
#' doc <- add_method_step(doc, "some lab method", "this is how we did it")
#' doc
#'
#' # Then we clear it
#' clear_methods(doc)
clear_methods <- function(doc) {
  stopifnot(is(doc, "eml"))

  # Clear the methods out
  doc@dataset@methods <- new("MethodsType")

  doc
}

#' Create an EML creator subtree from a first, last, and email.
#'
#' @param first (character) The first name
#' @param last (character) The last name
#' @param email (character) The email address
#'
#' @return (creator) The new creator sub-tree.
#' @export
#'
#' @examples
#' creator("test", "user", test@user.com")
eml_creator <- function(first, last, email) {
  stopifnot(all(is.character(c(first, last, email))),
            all(nchar(c(first, last, email)) > 0))

  indiv_name <- eml_individual_name(first, last)

  # Create <electronicMailAddress>
  email_address <- new("electronicMailAddress")
  email_address@.Data <- email

  # Create <creator>
  creator <- new("creator")
  creator@individualName <- new("ListOfindividualName", list(indiv_name))
  creator@electronicMailAddress <- new("ListOfelectronicMailAddress", list(email_address))

  creator
}

#' Create an EML contact subtree from a first, last, and email.
#'
#' @param first (character) The first name
#' @param last (character) The last name
#' @param email (character) The email address
#'
#' @return (contact) The new contact sub-tree.
#' @export
#'
#' @examples
#' eml_contact("test", "user", test@user.com")
eml_contact <- function(first, last, email) {
  stopifnot(all(is.character(c(first, last, email))),
            all(nchar(c(first, last, email)) > 0))

  indiv_name <- eml_individual_name(first, last)

  # Create <electronicMailAddress>
  email_address <- new("electronicMailAddress")
  email_address@.Data <- email

  # Create <contact>
  contact <- new("contact")
  contact@individualName <- new("ListOfindividualName", list(indiv_name))
  contact@electronicMailAddress <- new("ListOfelectronicMailAddress", list(email_address))

  contact
}

eml_individual_name <- function(first, last) {
  given <- new("givenName")
  given@.Data <- first

  sur <- new("surName")
  sur@.Data <- last

  # Create <individualName>
  indiv_name <- new("individualName")
  indiv_name@givenName <- new("ListOfgivenName", list(given))
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
