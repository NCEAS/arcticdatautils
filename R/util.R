# Various utility functions


#' Extract the local identifier for an ACADIS ISO metadata XML file
#'
#' @param type (character) A string, one of "gateway" or "field-projects".
#' @param file (character) A string, connection, or raw vector
#'   (same as [xml2::read_xml()]).
#'
#' @return (character) The identifier string.
#'
#' @noRd
extract_local_identifier <- function(type, file) {
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(type %in% c("gateway", "field-projects"))

  stopifnot(is.character(file), length(file) == 1)
  stopifnot(file.exists(file))

  xml_file <- xml2::read_xml(file)
  xml_namespaces <- xml2::xml_ns(xml_file)

  if (type == "gateway") {
    identifier_text <- xml2::xml_find_all(xml_file, "//gmd:fileIdentifier/gco:CharacterString/text()", xml_namespaces)
  }
  else if (type == "field-projects") {
    identifier_text <- xml2::xml_find_all(xml_file, "//gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:identifier/gmd:MD_Identifier/gmd:code/gco:CharacterString/text()", xml_namespaces)
  }

  stopifnot(length(identifier_text) == 1)

  if (type == "gateway") {
    identifier <- as.character(identifier_text)
  }
  else if (type == "field-projects") {
    identifier <- stringr::str_extract(identifier_text, "\\d+\\.\\d+")
  }

  if (is.na(identifier)) { stop("Failed to extract identifier.") }

  identifier
}


#' Print a random dataset
#'
#' @param inventory (data.frame) An inventory.
#' @param theme (character) Optional. A package theme name.
#' @param n (numeric) Optional. The number of files to show.
#'
#' @return `NULL`
#'
#' @noRd
show_random_dataset <- function(inventory, theme=NULL, n=10) {
  stopifnot(is.data.frame(inventory),
            all(c("file", "folder", "filename", "theme") %in% names(inventory)))

  themes <- c("has-versions", "many-files", "ready-to-go")

  # If theme was not set, don't filter
  # Otherwise, filter to theme
  if (is.null(theme)) {
    sampled_pkg <- inventory[inventory$package == sample(inventory$package, 1),]
  } else {
    stopifnot(is.character(theme),
              theme %in% themes,
              "theme" %in% names(inventory))
    sampled_pkg <- inventory[inventory$package == sample(inventory[inventory$theme == theme,"package"], 1),]
  }

  stopifnot(nrow(sampled_pkg) > 0)

  # Find the base directory so we can gsub it out of the rest of the files
  base_dir <- sampled_pkg[which(sampled_pkg$is_metadata == TRUE),"folder"]

  # startDebug
  if (length(base_dir) != 0) {
    browser()
  }
  # endDebug

  stopifnot(length(base_dir) == 1)

  # Grab the files
  files <- gsub(base_dir, "", sampled_pkg[,"file"])

  # Remove NAs
  files <- files[!is.na(files)]

  # Print it out
  cat(paste0("Theme: ", theme, "\n"))
  cat(paste0("nfiles: ", length(files), "\n"))
  cat(paste0("Base dir: ", base_dir, "\n"))
  print(head(files, n = n))
  if (length(files) > n) { cat(paste0("...and ", length(files) - n, " more files.\n")) }
}


#' Log a message to the console and to a logfile
#'
#' Reads from the environment variable 'LOG_PATH' and uses the value set there
#' to decide the location of the log file. If that envvar isn't set, it defaults
#' to 'arcticdata-log.txt'.
#'
#' @param message (character) Your log message.
#'
#' @return `NULL`
#'
#' @noRd
log_message <- function(message=NULL) {
  if (is.null(message) || !is.character(message) || nchar(message) < 1) {
    invisible(return(FALSE))
  }

  # Determine the logfile path
  logfile_path <- Sys.getenv("LOG_PATH")

  if (nchar(logfile_path) == 0) {
    logfile_path <- "arcticdata-log.txt"
  }

  # Prepare the message
  message <- paste0("[", as.POSIXlt(Sys.time(), "GMT"), "] ", stringr::str_replace_all(message, "[\n]", ""))

  # Write out the message as a warning() and log it to file
  message(paste0(message))
  write(message,
        file = logfile_path,
        append = TRUE)


  invisible(TRUE)
}


#' Check if an object exists on a Member Node
#'
#' This is a simple check for the HTTP status of a /meta/\{PID\} call on the
#' provided Member Mode.
#'
#' @param node (MNode) The Member Node to query.
#' @param pids (character) The PID(s) to check the existence of.
#'
#' @return (logical) Whether the object exists.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#' "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#'
#' object_exists(mn, pids)
#' }
object_exists <- function(node, pids) {
  stopifnot(class(node) %in% c("MNode", "CNode"),
            is.character(pids))

  result <- vector(mode = "logical", length = length(pids))

  for (i in seq_along(pids)) {
    sysmeta <- tryCatch({
      suppressWarnings(dataone::getSystemMetadata(node, pids[i]))
    },
    error = function(e) {
      e
    })

    if (inherits(sysmeta, "error") || !is(sysmeta, "SystemMetadata")) {
      result[i] <- FALSE
    } else {
      result[i] <- TRUE
    }
  }

  return(result)
}


#' Convert an ISO document to EML using an XSLT
#'
#' Leave style=NA if you want to use the default ISO-to-EML stylesheet.
#'
#' @param path (character) Path to the file to convert.
#' @param style (xslt) The XSLT object to be used for transformation.
#'
#' @return (character) Location of the converted file.
#'
#' @export
#'
#' @examples
#'\dontrun{
#'iso_path <- "~/Docuements/ISO_metadata.xml"
#'eml_path <- convert_iso_to_eml(iso_path)
#'}
convert_iso_to_eml <- function(path, style=NA) {
  # Load the XSLT if needed
  if (is.na(style)) {
    style <- xml2::read_xml(file.path(system.file(package = "arcticdatautils"), "iso2eml.xsl"))
  }

  stopifnot(file.exists(path))
  doc <- xml2::read_xml(path)

  transformed_doc <- xslt::xml_xslt(doc, style)

  outpath <- tempfile()
  xml2::write_xml(transformed_doc, outpath)

  outpath
}


#' Modify name structure for EML parties
#'
#' Extract the EML responsible-party blocks in a document and parse the
#' surName field to create proper givenName/surName structure.
#'
#' @param path (character) The path to the EML document to process.
#'
#' @return (character) The path to the converted EML file.
#'
#' @import XML
#'
#' @noRd
substitute_eml_party <- function(path) {
  # Read in the EML document
  doc = XML::xmlParse(path)

  # For each of the creator, contact, associatedParty elements
  # creators <- XML::getNodeSet(eml, '/eml:eml/dataset/creator', ns)
  # print(class(creators))
  # sapply(creators, change_eml_name)
  # associates <- XML::getNodeSet(eml, '/eml:eml/dataset/associatedParty', ns)
  # print(class(associates))
  # sapply(associates, change_eml_name)
  # contacts <- XML::getNodeSet(eml, '/eml:eml/dataset/contact', ns)
  # print(class(contacts))
  # sapply(contacts, change_eml_name)

  XML::xpathSApply(doc, "//dataset/creator", change_eml_name)
  XML::xpathSApply(doc, "//dataset/associatedParty", change_eml_name)
  XML::xpathSApply(doc, "//dataset/contact", change_eml_name)
  XML::xpathSApply(doc, "//dataset/project/personnel", change_eml_name)


  # Serialize the EML document to disk
  XML::saveXML(doc, file = path)

  # Return the EML path
  return(path)
}


#' Change EML name
#'
#' Utility function to extract a name string from an XML individualName node,
#' parse it into tokens,and reformat the individualName with new children nodes.
#'
#' @param party The XML node containing a subclass of eml:ResponsibleParty.
#'
#' @return The modified XML node.
#'
#' @import XML
#'
#' @noRd
change_eml_name <- function(party) {
  # Check if there is an individualName element exists
  if (length(XML::getNodeSet(party, "./individualName")) == 0) {
    return(party)
  }

  # Check if there is already a <givenName> tag and do nothing if there is
  if (length(XML::getNodeSet(party, "./individualName/givenName")) > 0) {
    log_message("Doing nothing...")
    return(party)
  }

  # Parse out the surName and givenName(s) of the party
  user_name <- XML::xpathSApply(party, "./individualName/surName", XML::xmlValue)

  if (length(user_name) != 1) {
    cat(paste0("For some reason, there was not a single surName value, but instead there were ", length(user_name), ".\n"))
    return(party)
  }

  if (nchar(user_name) == 0) {
    cat(paste0("Length of user_name was zero.\n"))
    return(party)
  }

  # Replace commas with spaces
  user_name <- stringr::str_replace_all(user_name, ",", "")

  if (!requireNamespace("humaniformat")) {
    stop("The package 'humaniformat' is required to run this function. ",
         "Please install it.")
  }

  parsed_name <- humaniformat::parse_names(user_name)

  # Create the new node to hold the parts of the name
  name_node <- XML::newXMLNode("individualName")

  if (nchar(parsed_name['first_name'] > 0)) {
    XML::addChildren(name_node, XML::newXMLNode("givenName", parsed_name['first_name']))
  }

  if (nchar(parsed_name['suffix']) > 0) {
    XML::addChildren(name_node,  XML::newXMLNode("givenName", parsed_name['suffix']))
  }

  if (nchar(parsed_name['salutation']) > 0) {
    XML::addChildren(name_node,  XML::newXMLNode("givenName", parsed_name['salutation']))
  }

  if (nchar(parsed_name['middle_name']) > 0) {
    XML::addChildren(name_node,  XML::newXMLNode("givenName", parsed_name['middle_name']))
  }

  if (nchar(parsed_name['last_name'] > 0)) {
    XML::addChildren(name_node,  XML::newXMLNode("surName", parsed_name['last_name']))
  }

  individ_node <- XML::getNodeSet(party, "./individualName")[[1]]

  XML::removeChildren(party,individ_node )
  XML::addChildren(party, name_node, at = 0)

  party
}


#' Replace EML packageId with value
#'
#' Replace the EML 'packageId' attribute on the root element with a
#' certain value.
#'
#' @param path (character) Path to the XML file to edit.
#' @param replacement (character) The new value.
#'
#' @noRd
replace_package_id <- function(path, replacement) {
  stopifnot(file.exists(path))
  stopifnot(is.character(replacement),
            nchar(replacement) > 0)

  doc <- EML::read_eml(path)
  stopifnot(is(doc, "eml"))

  doc@packageId <- new("xml_attribute", replacement)
  doc@system <- new("xml_attribute", "arcticdata")

  EML::write_eml(doc, path)

  path
}


#' Add a string to the title element in the given file
#'
#' @param path (character) Path to the XML file to edit.
#' @param string (character) The new value.
#'
#' @noRd
add_string_to_title <- function(path, string) {
  stopifnot(file.exists(path))
  stopifnot(is.character(string),
            nchar(string) > 0)

  result <- tryCatch({
    xmldoc <- XML::xmlParseDoc(file = path)
    title_nodes <- XML::getNodeSet(xmldoc, "//dataset/title")
    stopifnot(length(title_nodes) == 1)

    XML::xmlValue(title_nodes[[1]]) <- paste0(XML::xmlValue(title_nodes[[1]]),
                                              string)
    XML::saveXML(xmldoc, path)
  },
  error = function(e) {
    e
  })

  if (inherits(result, "error")) {
    log_message(result)
  }

  path
}


#' Add a set of additional identifiers to an EML document
#'
#' @param path (character) Path to the EML document.
#' @param identifiers (character) Set of identifiers to add.
#'
#' @return (character) Path to the modified document.
#'
#' @noRd
add_additional_identifiers <- function(path, identifiers) {
  stopifnot(is.character(path),
            nchar(path) > 0,
            file.exists(path),
            is.character(identifiers),
            all(lengths(identifiers) > 0))

  # Make identifiers unique
  identifiers <- unique(identifiers)

  # Get the doc
  doc <- EML::read_eml(path)

  # Add the identifiers
  doc@dataset@alternateIdentifier <- new("ListOfalternateIdentifier", lapply(identifiers, function(identifier) new("alternateIdentifier", identifier)))

  # Save document
  EML::write_eml(doc, path)

  path
}


#' Intelligently join possibly redundant path parts together
#'
#' Joins path strings like "./" to "./my/dir" as "./my/dir" instead of as
#' "././my/dir.
#'
#' @param path_parts (character)
#'
#' @return (character) The joined path string.
#'
#' @noRd
path_join <- function(path_parts=c("")) {
  result <- paste0(path_parts, collapse = "")

  # Change duplicated './' to just a single './'
  result <- gsub("[\\.\\/]{2,}", "\\.\\/", result)

  # Remove mid-path "/./"
  # e.g. ./asdf/./fdas
  result <- gsub("\\/\\.\\/", "\\/", result)

  # "~/src/arcticdata./inst/asdf"

  # Remove ./ from the middle of the string
  # e.g. ./asdf/./fdas
  result <- gsub("(.)\\.\\/", "\\1\\/", result)

  result
}


#' Determine whether the object with the given PID is a resource map
#'
#' @param node (MNode|CNode) The Coordinating/Member Node to run the query on.
#' @param pids (character) Vector of PIDs.
#'
#' @return (logical) Whether or not the object(s) are resource maps.
#'
#' @noRd
is_resource_map <- function(node, pids) {
  is_format_id(node, pids, "http://www.openarchives.org/ore/terms")
}


#' Test whether the object is obsoleted by another object
#'
#' @param node (MNode|CNode) The Coordinating/Member Node to run the query on.
#' @param pids (character) One or more PIDs to query against.
#'
#' @return (logical) Whether or not the object is obsoleted by another object.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pid <- "urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1"
#'
#' is_obsolete(mn, pid)
#'}
is_obsolete <- function(node, pids) {
  stopifnot(is(node, "MNode") || is(node, "CNode"))
  stopifnot(is.character(pids))

  response <- vector(mode = "logical", length = length(pids))

  for (i in seq_along(pids)) {
    pid <- pids[i]
    sysmeta <- dataone::getSystemMetadata(node, pid)
    response[i] <- is.na(sysmeta@obsoletedBy)
  }

  response
}


#' Return the subject of the set dataone_test_token
#'
#' @return (character) The token subject.
#'
#' @noRd
get_token_subject <- function() {
  info <- dataone::getTokenInfo(dataone::AuthenticationManager())

  # Throw an error for the dataone package so we stop when there are no tokens.
  if (nrow(info) == 0) {
    stop("No tokens defined.")
  }

  # Throw an warning if multiple tokens are set
  if (nrow(info) > 1) {
    warning(paste0("Multiple tokens are set: ", paste(info$name, collapse = ", "), ". The subject of the first token, ", info[1,"subject"], " was used."))
  }

  info[1,"subject"]
}


#' Get the identifier from a DataONE response
#'
#' Example response:
#'
#' <d1:identifier xmlns:d1="http://ns.dataone.org/service/types/v1">
#'   urn:uuid:12aaf494-5840-434d-9cdb-c2597d58543e
#' </d1:identifier>
#'
#' @param dataone_response ("XMLInternalDocument"/"XMLAbstractDocument")
#'
#' @return (character) The PID.
#'
#' @noRd
get_identifier <- function(dataone_response) {
  stopifnot("XMLInternalDocument" %in% class(dataone_response))
  XML::xmlValue(XML::getNodeSet(dataone_response, "//d1:identifier/text()", namespaces = c("d1" = "http://ns.dataone.org/service/types/v1"))[[1]])
}


#' Generate a new UUID PID
#'
#' Generate a new UUID PID.
#'
#' @return (character) A new UUID PID.
#'
#' @export
#'
#' @examples
#' id <- new_uuid()
new_uuid <- function() {
  paste0("urn:uuid:", uuid::UUIDgenerate())
}


#' Get the current package version
#'
#' This function parses the installed DESCRIPTION file to get the latest
#' version of the package.
#'
#' @return (character) The current package version.
#'
#' @noRd
get_current_version <- function() {
  desc_file <- file.path(system.file("DESCRIPTION", package = "arcticdatautils"))
  desc_lines <- readLines(desc_file)
  gsub("Version: ", "", desc_lines[grep("Version:", desc_lines)])
}


#' Use the GitHub API to find the latest release for the package
#'
#' @return (character) The latest release.
#'
#' @noRd
get_latest_release <- function() {
  req <- httr::GET("https://api.github.com/repos/NCEAS/arcticdatautils/releases")
  content <- httr::content(req)

  releases <- do.call(rbind, lapply(content, function(r) data.frame(name = r$name, published = r$published_at, stringsAsFactors = FALSE)))
  latest <- releases[order(releases$published, decreasing = TRUE)[1], "name"]

  gsub("v", "", latest)
}


#' Warn if package version is outdated
#'
#' Warns if the currently-installed version of the package is not the same
#' version as the latest release on GitHub.
#'
#' @noRd
warn_current_version <- function() {
  current <- get_current_version()
  latest <- get_latest_release()

  if (current != latest) {
    warning(paste0("Your version of the arcticdatautils package is ", current, " but the latest version is ", latest, ".\n  You should upgrade as there may be important bug fixes in newer versions of the package.\n  See https://github.com/NCEAS/arcticdatautils#installing for instructions."))
  }
}


#' Get the PIDs of all versions of an object
#'
#' Get the PIDs of all versions of an object.
#'
#' @param node (MNode) The Member Node to query.
#' @param pid (character) Any object in the chain.
#'
#' @return (character) A vector of PIDs in the chain, in order.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pid <- "urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1"
#'
#' ids <- get_all_versions(mn, pid)
#' }
get_all_versions <- function(node, pid) {
  stopifnot(class(node) %in% c("MNode", "CNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  pids <- c(pid)

  # Walk backward
  sm <- getSystemMetadata(node, pid)

  while (!is.na(sm@obsoletes)) {
    oldsm <- sm # Save a copy for better warning messages
    sm <- getSystemMetadata(node, sm@obsoletes)

    if (is.null(sm)) {
      warning(call. = FALSE,
              paste0("An incomplete version chain has been returned. ", oldsm@identifier, " obsoletes ", oldsm@obsoletes, " but ", oldsm@obsoletes, " could not be found. This can be due to the object not existing or not having correct permission to view it."))
      break
    }

    pids <- c(sm@identifier, pids)
  }

  # Then forward from the start pid
  sm <- getSystemMetadata(node, pid)

  while (!is.na(sm@obsoletedBy)) {
    oldsm <- sm # Save a copy for better warning messages
    sm <- getSystemMetadata(node, sm@obsoletedBy)

    if (is.null(sm)) {
      warning(call. = FALSE,
              paste0("An incomplete version chain has been returned. ", oldsm@identifier, " is obsoleted by ", oldsm@obsoletedBy, " but ", oldsm@obsoletedBy, " could not be found. This can be due to the object not existing or not having correct permission to view it."))
      break
    }

    pids <- c(pids, sm@identifier)
  }

  pids
}


#' Get a structured list of PIDs for the objects in a package
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' Please use dataone::getDataPackage() when possible
#'
#' Get a structured list of PIDs for the objects in a package,
#' including the resource map, metadata, and data objects.
#'
#' @param node (MNode/CNode) The Coordinating/Member Node to run the query on.
#' @param pid (character) The the resource map PID of the package.
#' @param file_names (logical) Whether to return file names for all objects.
#' @param rows (numeric) The number of rows to return in the query. This is only
#'   useful to set if you are warned about the result set being truncated. Defaults to 5000.
#'
#' @return (list) A structured list of the members of the package.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' #Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pid <- "resource_map_urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1"
#'
#' ids <- get_package(mn, pid)
#' }
get_package <- function(node, pid, file_names=FALSE, rows=5000) {
  lifecycle::deprecate_soft("1.0.0", "get_package()", "dataone::getDataPackage()")

  stopifnot(is(node, "MNode") || is(node, "CNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)
  stopifnot(is.numeric(rows) || is.numeric(as.numeric(rows)),
            rows >= 0)

  if (is_resource_map(node, pid)) {
    resource_map_pids <- pid
  } else {
    warning(call. = FALSE,
            paste0("The PID '", pid, "' is not for a Resource Map Object so the most likely candidate was found. This is usually fine! Specify a Resource Map PID instead to stop getting this warning."))
    resource_map_pids <- find_newest_resource_map(node, pid)
  }

  # Stop if no resource map was found
  if (length(resource_map_pids) == 0) {
    stop(paste0("No resource map was found for ", pid, ". This may be because none existed or all that exist are obsoleted."))
  }

  # Warn user if multiple resource maps were found
  if (length(resource_map_pids) > 1) {
    warning(paste0("Multiple (", length(resource_map_pids), ") non-obsolete resource maps were found. This is valid but is rare so this warning is being issued just as a precaution."))
  }

  packages <- lapply(resource_map_pids, function(pid) get_package_direct(node, pid, file_names, rows))

  if (length(packages) == 1) {
    return(packages[[1]])
  } else {
    return(packages)
  }
}


#' Get a structured list of PIDs for the objects in a package
#'
#' This function is used within [get_package()].
#'
#' @param node (MNode/CNode) The Coordinating/Member Node to run the query on.
#' @param pid (character) The the metadata PID of the package.
#' @param file_names (logical) Whether to return file names for all objects.
#' @param rows (numeric) The number of rows to return in the query. This is only
#'   useful to set if you are warned about the result set being truncated. Defaults to 5000.
#'
#' @noRd
get_package_direct <- function(node, pid, file_names = FALSE, rows = 5000) {
  stopifnot(is(node, "MNode") || is(node, "CNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)
  stopifnot(is.numeric(rows) || is.numeric(as.numeric(rows)),
            rows >= 0)
  stopifnot(is_resource_map(node, pid))

  # Query for the package members
  pid_esc <- stringi::stri_replace_all_fixed(pid, ":", "\\:")

  # Dynamically create the 'fields' argument to the query
  if (file_names) {
    query_fields <- "identifier,formatType,fileName"
  } else {
    query_fields <- "identifier,formatType"
  }

  query_params <- list(q = paste0("resourceMap:", pid_esc),
                       rows = as.character(rows),
                       fl = query_fields)

  response <- dataone::query(node, query_params, as = "list")

  # Warn if there might be more results
  if (length(response) >= rows) {
    warning("Query returned the maximum number of results. It's possible there are more results to get. You can specify a custom number of results with the `rows` argument.")
  }

  # Stop now if no results were returned
  if (length(response) == 0) {
    warning(paste0("No results were found when searching for a package with resource map '", pid, "'.\nThis can be caused by a mis-typed PID, the resource map not existing, or by not having appropriate access to read the resource map."))
    return(response)
  }

  # Set up the names on the response vector if they are needed
  if (file_names) {
    names(response) <- sapply(response, function(x) ifelse("fileName" %in% names(x), x$fileName, NA))
  }

  # Collect the package's PIDs

  # Fix pids with no formatType by adding one manually. This is a quick hack
  # to make the rest of this code work when an object doesn't have a valid
  # formatId.
  response <- lapply(response, function(r) { if (!("formatType" %in% names(r))) { r[["formatType"]] = "UNKNOWN" }; r })

  metadata_pids <- vapply(response[sapply(response, function(x) { x$formatType == "METADATA"})], function(x) x$identifier, "")
  data_pids <- vapply(response[sapply(response, function(x) { x$formatType == "DATA"})], function(x) x$identifier, "")
  child_pids <- vapply(response[sapply(response, function(x) { x$formatType == "RESOURCE"})], function(x) x$identifier, "")
  unknown_pids <- vapply(response[sapply(response, function(x) { x$formatType == "UNKNOWN"})], function(x) x$identifier, "")

  response <- list(metadata = metadata_pids,
                   resource_map = pid,
                   data = data_pids,
                   child_packages = child_pids)

  if (length(unknown_pids) != 0) {
    warning(call. = FALSE,
            "Some Objects in this package didn't have their formatType set in the Solr index and have been marked as 'unknown_pids'. They are likely DATA and you likely want to consider them as such but it wasn't possible to tell. Proceed with caution.")
    response[["unknown_pids"]] <- unknown_pids
  }

  response
}


#' Get the resource map(s) for the given object
#'
#' @param node (MNode/CNode) The Member Node to query.
#' @param pid (character) The object to get the resource map(s) for.
#' @param rows (numeric) Optional. The number of query results to return.
#'   The default, 1000, is very likely to be more than enough.
#'
#' @return (character) The resource map(s) that contain `pid`.
#'
#' @noRd
find_newest_resource_map <- function(node, pid, rows = 1000) {
  stopifnot(class(node) %in% c("MNode", "CNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)
  stopifnot(is.numeric(rows) || is.numeric(as.numeric(rows)),
            rows >= 0)

  pid_esc <- stringi::stri_replace_all_fixed(pid, ":", "\\:")
  query_params <- list(q = paste0("id:", pid_esc),
                       rows = as.character(rows),
                       fl = "resourceMap")

  response <- dataone::query(node, query_params, as = "list")

  if (length(response) == 0) {
    stop(paste0("No resource map found for ", pid, "."))
  }

  if (length(response) > 1) {
    stop(paste0("More than one Solr document was returned which is unexpected."), call. = FALSE)
  }

  all_resource_map_pids <- unlist(lapply(response, function(x) {
    if ("resourceMap" %in% names(x)) {
      return(x$resourceMap)
    } else {
      return(NA)
    }
  }))

  all_resource_map_pids <- as.character(na.omit(all_resource_map_pids))

  if (length(all_resource_map_pids) == 0) {
    stop(paste0("No resource map(s) found for ", pid, "."), call. = FALSE)
  }

  if (length(all_resource_map_pids) > 1) {
    warning("Multiple possible resource maps found for. Choosing the newest based on dateUploaded. This is probably what you want.")
  }

  find_newest_object(node, all_resource_map_pids)
}


#' Find the newest object within the given set of objects
#'
#' Find the newest object, based on dateUploaded, within the given set of objects.
#'
#' @param node (MNode/CNode) The Member Node to query.
#' @param identifiers (character) One or more identifiers.
#' @param rows (numeric) Optional. Specify the size of the query result set.
#'
#' @return (character) The PID of the newest object. In the case of a tie (very
#'   unlikely) the first element, in natural order, is returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mn <- MNode(...)
#' find_newest_object(mn, c("PIDX", "PIDY", "PIDZ"))
#' }
find_newest_object <- function(node, identifiers, rows=1000) {
  stopifnot(class(node) %in% c("MNode", "CNode"))
  stopifnot(is.character(identifiers),
            length(identifiers) > 0,
            all(nchar(identifiers)) > 0)
  stopifnot(is.numeric(rows) || is.numeric(as.numeric(rows)),
            rows >= 0)

  response <- dataone::query(node, list(q = paste0("identifier:", paste0("\"", identifiers, "\""), collapse = " OR "),
                                        fl = "identifier,dateUploaded",
                                        rows = as.character(rows),
                                        sort = "dateUploaded+desc"),
                             as = "data.frame")




  if (nrow(response) == 0) {
    stop("No objects found for ", paste(identifiers, collapse = ", "), call. = FALSE)
  }

  response[1,"identifier"]

}


#' Filters PIDs that are obsolete
#'
#' Whether or not a PID is obsolete is determined by whether its "obsoletedBy"
#' property is set to another PID (`TRUE`) or is `NA` (`FALSE`).
#'
#' @param node (MNode|CNode) The Member Node to query.
#' @param pids (character) PIDs to check the obsoletion state of.
#'
#' @return (character) PIDs that are not obsoleted by another PID.
#'
#' @noRd
filter_obsolete_pids <- function(node, pids) {
  pids[is.na(sapply(pids, function(pid) { dataone::getSystemMetadata(node, pid)@obsoletedBy }, USE.NAMES = FALSE))]
}

#' Show the indexing status of a set of PIDs
#'
#' Show the indexing status of a set of PIDs.
#'
#' @param mn (MNode) The Member Node to query.
#' @param pids (character/list) One or more PIDs.
#'
#' @return `NULL`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a package then check its indexing status
#' library(dataone)
#' mn <- MNode(...)
#' pkg <- create_dummy_package(mn)
#' show_indexing_status(mn, pkg)
#' }
show_indexing_status <- function(mn, pids) {
  # Automatically try to convert a list of pids to a vector of pids so the user
  # can pass in the results of publish_update / get_package / etc
  if (is(pids, "list")) {
    pids <- unlist(pids, use.names = FALSE)
  }

  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pids),
            length(pids) > 0)

  expected_pids <- unlist(pids, use.names = FALSE)
  indexed_pids <- c() # Accumulates the PIDs we find in the index

  pb <- txtProgressBar(min = 0, max = length(expected_pids), style = 3)

  while (!all(expected_pids %in% indexed_pids)) {
    unresolved_pids <- setdiff(expected_pids, indexed_pids)

    # Query for the pids in chunks of 10, with rate-limiting
    find_pids_in_index <- function(mn, pids, delay = 0.1, group_size = 10) {
      groups <- split(pids, ceiling(seq_along(pids)/group_size  ))

      result <- lapply(groups, function(group) {
        r <- query(mn, paste0("q=identifier:(", paste(paste0('"', unlist(group, use.names = FALSE), '"'), collapse="+OR+"), ")&fl=identifier"))
        Sys.sleep(delay)
        return(unlist(r))
      })

      unlist(result, use.names = FALSE)
    }

    result <- find_pids_in_index(mn, unresolved_pids)
    indexed_pids <- c(indexed_pids, unlist(result))

    setTxtProgressBar(pb, length(indexed_pids))

    Sys.sleep(1)
  }

  close(pb)
}

#' Set public READ access on all versions of PIDs in data package.
#'
#' Set public READ access on all versions of PIDs in data package.
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The resource map identifier (PID).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn_staging <- CNode('STAGING')
#' adc_test <- getMNode(cn_staging,'urn:node:mnTestARCTIC')
#' # Create a dummy package then create another version with 'publish_update()'
#' pkg <- create_dummy_package(adc_test)
#' remove_public_read(mn, unlist(pkg))
#' pkg_v2 <- publish_update(adc_test, pkg$metadata, pkg$resource_map, pkg$data, public = FALSE)
#' # Set public read on all versions
#' set_public_read_all_versions(adc_test, pkg$resource_map)
#' }
set_public_read_all_versions <- function(mn, resource_map_pid) {
  stopifnot(is(mn, 'MNode'))
  stopifnot(is_token_set(mn))
  stopifnot(is.character(resource_map_pid))
  stopifnot(arcticdatautils:::is_resource_map(mn, resource_map_pid))

  pids <- get_package(mn, resource_map_pid) %>%
    unlist()
  versions <- lapply(pids, get_all_versions, node = mn) %>%
    unlist() %>%
    unique()
  set_public_read(mn, versions)

  return(invisible())
}
