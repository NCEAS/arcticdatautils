#' util.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' General utility functions that may be later merged into other files.


#' Extracts the local identifier for an ACADIS ISO metadata XML file.
#'
#' @param type (character) A string, one of "gateway" or "field-projects".
#' @param file (character) A string, a connection, or raw vector (same as xml2::read_xml).
#'
#' @returns The identifier string. (character)

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


dataone_format_mappings <- list("avi" = "ideo/avi",
                                "bmp" = "image/bmp",
                                "bz2" = "application/x-bzip2",
                                "csv" = "text/csv",
                                "fasta" = "application/x-fasta",
                                "gif" = "image/gif",
                                "gz" = "application/x-gzip",
                                "html" = "text/html",
                                "jpg" = "image/jpeg",
                                "jpeg" = "image/jpeg",
                                "kml" = "application/vnd.google-earth.kml xml",
                                "mp4" = "video/mp4",
                                "mpg" = "video/mpeg",
                                "mpeg" = "video/mpeg",
                                "n3" = "	text/n3",
                                "nc" = "netCDF-3",
                                "pdf" = "application/pdf",
                                "png" = "image/png",
                                "ppt" = "application/vnd.ms-powerpoint",
                                "py" = "application/x-python",
                                "rdf" = "application/rdf xml",
                                "tar" = "application/x-tar",
                                "tif" = "image/tiff",
                                "tiff" = "image/tiff",
                                "ttl" = "	text/turtle",
                                "txt" = "text/plain",
                                "wmv" = "video/x-ms-wmv",
                                "xls" = "application/vnd.ms-excel",
                                "xlsx" = "application/vnd.ms-excel",
                                "xml" = "application/xml",
                                "zip" = "application/zip")


#' Guess format from filename for a vector of filenames.
#'
#' @param filenames (character)
#'
#' @return (character) DataOne format identifiers strings.
#' @export
#'
#' @examples
guess_format_id <- function(filenames) {
  extensions <- tolower(tools::file_ext(filenames))
  filetypes <- vector(mode = "character", length = length(extensions))

  for (i in seq_len(length(extensions))) {
    extension <- extensions[i]

    if (extension %in% names(dataone_format_mappings)) {
      filetypes[i] <- dataone_format_mappings[extension][[1]]
    } else {
      filetypes[i] <- "application/octet-stream"
    }
  }

  filetypes
}


#' Determine the DataONE format ID for the NetCDF file provided by path.
#'
#' @param path (character) Full or relative path to the file in question.
#'
#' @return (character) The DataONE format ID.
#' @export
#'
#' @examples
get_netcdf_format_id <- function(path) {
  stopifnot(is.character(path),
            nchar(path) > 0,
            file.exists(path))

  # Try to open the file, capturing errors
  cdf_file <- try({
    ncdf4::nc_open(path)
  })

  # If we failed to open the file, we can assume it's not a valid NetCDF file
  # and we just return application/octet-stream as the format ID
  if (inherits(cdf_file, "try-error")) {
    return("application/octet-stream")
  }

  # Since we got this far, continue detecting the format
  stopifnot("format" %in% names(cdf_file))
  format_string <- cdf_file$format
  stopifnot(is.character(format_string),
            nchar(format_string) > 0)
  format_id = ""

  if (format_string == "NC_FORMAT_CLASSIC") {
    format_id = "netCDF-3"
  } else if (format_string == "NC_FORMAT_NETCDF4") {
    format_id = "netCDF-4"
  } else {
    stop("Unknown NetCDF format discovered.")
  }

  return(format_id)
}


#' Print a random dataset.
#'
#' @param inventory (data.frame) An inventory.
#' @param theme (character) Optional. A package theme name.
#' @param n (numeric) Optional. The number of files to show.
#'
#' @return Nothing.
#' @export
#'
#' @examples
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
  if (length(base_dir) != 0){
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


#' Log a message to the console and to a logfile.
#'
#' Reads from the environment variable 'LOG_PATH' and uses the value set there
#' to decide the location of the log file. If that envvar isn't set, it defaults
#' to 'arcticdata-log.txt'.
#'
#' @param message (character) Your log message.
#'
#' @return Nothing.
#' @export
#'
#' @examples
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


#' Check if an object exists on a Member Node.
#'
#' This is a simple check for the HTTP status of a /meta/{PID} call on the
#' provided member node.
#'
#' @param mn_base_url (character) Member Node to query.
#' @param pid (character) PID to check the existence of.
#'
#' @return (logical) Whether the object exists.
#' @export
#'
#' @examples
object_exists <- function(mn, pids) {
  stopifnot(class(mn) == "MNode",
            is.character(pids))

  result <- vector(mode = "logical", length = length(pids))

  for (i in seq_along(pids)) {
    sysmeta <- tryCatch({
      suppressWarnings(dataone::getSystemMetadata(mn, pids[i]))
    },
    error = function(e) {
      e
    })

    if (inherits(sysmeta, "error") || class(sysmeta) != "SystemMetadata") {
      result[i] <- FALSE
    } else {
      result[i] <- TRUE
    }
  }

  return(result)
}


#' Convert and ISO document to EML using an XSLT.
#'
#' This is a bit of a nasty function right now but it essentially
#' takes a file in, loads an XSLT file and does the conversion. The nasty part
#' is that I have the XSLT file in a hard-linked directory on my computer
#' because we're still actively developing the XSLT.
#'
#' @param full_path (character) Path to the file to convert.
#' @param isotoeml (xslt) the XSLT object to be used for transformation.
#'
#' @return (character) Location of the converted file.
#' @export
#'
#' @examples
convert_iso_to_eml <- function(full_path, isotoeml=NA) {
  # Load the XSLT from the default location
  if (is.na(isotoeml)) {
    xsl_path <- file.path(system.file(package = "arcticdatautils"), "iso2eml.xsl")
    stopifnot(file.exists(xsl_path))
    isotoeml <- xslt::read_xslt(xsl_path)
  }

  stopifnot(file.exists(full_path))
  stopifnot(inherits(isotoeml, "xslt_document"))

  tmpfile <- tempfile(fileext = ".xml")

  doc <- tryCatch({
    xml2::read_xml(full_path)
  },
  warning = function(w) {
    log_message(w)
  },
  error = function(e) {
    log_message(e)
  })

  transformed_document <- tryCatch({
    xslt::xslt_transform(doc, isotoeml)
  },
  warning = function(w) {
    log_message(w)
  },
  error = function(e) {
    log_message(e)
  })

  if (!inherits(transformed_document, "xml_document")) {
    log_message(paste0("Full path is ", full_path))
    log_message(paste0("isotoeml is ", isotoeml))
    log_message(paste0("doc is ", doc))
    log_message(paste0("transformed doc is ", transformed_document))
  } else {
    xml2::write_xml(transformed_document, tmpfile)
  }


  tmpfile
}

#' Extract the EML responsible-party blocks in a document, and parse the
#' surName field to create proper givenName/surName structure
#'
#' @param path file path to the EML document to process (character)
#'
#' @return path (character) Path to the converted EML file.
#' @import XML
#' @export
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

#' Utility function to extract a name string from an XML individualName node,
#' parse it into tokens,and reformat the individualName with new children nodes
#'
#' @param party the XML node containing a subclass of eml:ResponsibleParty
#'
#' @return the modified XML node
#'
#' @import XML
#' @export
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


#' Replace the EML 'packageId' attribute on the root element with a
#' certain value.
#'
#' @param path (character) Path to the XML file to edit.
#' @param replacement (character) The new value.
#'
#' @return
#' @export
#'
#' @examples
replace_package_id <- function(path, replacement) {
  stopifnot(file.exists(path))
  stopifnot(is.character(replacement),
            nchar(replacement) > 0)

  doc <- EML::read_eml(path)
  stopifnot(class(doc) == "eml")

  doc@packageId <- new("xml_attribute", replacement)
  doc@system <- new("xml_attribute", "arcticdata")

  EML::write_eml(doc, path)

  path
}

#' Adds a string to the title element in the given file.
#'
#' @param path (character) Path to the XML file to edit.
#' @param string (character) The new value.
#'
#' @return
#' @export
#'
#' @examples
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


#' Add a set of additional identifiers to an EML document.
#'
#' @param path (character) Path to the EML document.
#' @param identifiers (character) Set of identifiers to add.
#'
#' @return (character) Path to the modified document.
#' @export
#'
#' @examples
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


#' (Intelligently) join (possibly redudant) path parts together.
#'
#' Joins path strings like "./" to "./my/dir" as "./my/dir" instead of as
#' "././my/dir.
#'
#' @param path_parts (character)
#'
#' @return (character)The joined path string.
#' @export
#'
#' @examples
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

#' Test whether an object is a particular format ID.
#'
#' @param mn (MNode)
#' @param pid (character)
#' @param format_id (character)
#'
#' @return (logical)
#' @export
#'
#' @examples
is_format_id <- function(mn, pid, format_id) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pid),
            nchar(pid) > 0)
  stopifnot(is.character(format_id),
            nchar(format_id) > 0)

  # Get System Metadata
  sysmeta <- tryCatch({
    log_message(paste0("Getting System Metadata for PID ", pid, "..."))
    dataone::getSystemMetadata(mn, pid)
  },
  error = function(e) {
    log_message(paste0("Failed to get System Metadata for PID of ", pid, "."))
    log_message(e)
    e
  })

  if (inherits(sysmeta, "error")) {
    return(FALSE)
  }

  if (sysmeta@formatId == format_id) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Determines whether the object with the given PID is a resource map.
#'
#' @param mn (MNode)
#' @param pid (character)
#'
#' @return (logical)
#' @export
#'
#' @examples
is_resource_map <- function(mn, pid) {
  is_format_id(mn, pid, "http://www.openarchives.org/ore/terms")
}


#' Returns the subject of the set dataone_test_token
#'
#' @return (character) The token subject.
#' @export
#'
#' @examples
get_token_subject <- function() {
  info <- dataone::getTokenInfo(dataone::AuthenticationManager())

  # Throw an error for the dataone package so we stop when there are no tokens.
  if (nrow(info) == 0) {
    stop("No tokens defined.")
  }

  me <- info[which(info$name == 'dataone_test_token'),]$subject

  if (info[which(info$name == 'dataone_test_token'),]$expired != FALSE) {
    stop("Stopped processing becuase your token is expired. Please provide a new dataone_test_token.")
  }

  return(me)
}


#' Get the identifier from a DataONE response.
#'
#' Example resposne:
#'
#' <d1:identifier xmlns:d1="http://ns.dataone.org/service/types/v1">
#'   urn:uuid:12aaf494-5840-434d-9cdb-c2597d58543e
#' </d1:identifier>
#'
#' @param dataone_response ("XMLInternalDocument" "XMLAbstractDocument")
#'
#' @return (character) The PID.
#' @export
#'
#' @examples
get_identifier <- function(dataone_response) {
  stopifnot("XMLInternalDocument" %in% class(dataone_response))
  XML::xmlValue(XML::getNodeSet(dataone_response, "//d1:identifier/text()", namespaces = c("d1"="http://ns.dataone.org/service/types/v1"))[[1]])
}


#' Helper function to generate a new UUID PID.
#'
#' @return (character) A new UUID PID.
#' @export
#'
#' @examples
new_uuid <- function() {
  paste0("urn:uuid:", uuid::UUIDgenerate())
}


#' Get the current package version.
#'
#' This function parses the installed DESCRIPTION file to get the latest
#' version.
#'
#' @return (character) The current package version.
#' @export
#'
#' @examples
get_current_version <- function() {
  desc_file <- file.path(system.file("DESCRIPTION", package = "arcticdatautils"))
  desc_lines <- readLines(desc_file)
  gsub("Version: ", "", desc_lines[grep("Version:", desc_lines)])
}


#' Use the GitHub API to find the latest release for the package.
#'
#' @return (character) The latest release.
#' @export
#'
#' @examples
get_latest_release <- function() {
  req <- httr::GET("https://api.github.com/repos/NCEAS/arcticdatautils/releases")
  content <- httr::content(req)

  releases <- do.call(rbind, lapply(content, function(r) data.frame(name = r$name, published = r$published_at, stringsAsFactors = FALSE)))
  latest <- releases[order(releases$published, decreasing = TRUE)[1], "name"]

  gsub("v", "", latest)
}


#' Warns if the currently-installed version of the package is not the same
#' version as the latest release on GitHub.
#'
#' @return
#' @export
#'
#' @examples
warn_current_version <- function() {
  cat("Checking version...")
  current <- get_current_version()
  latest <- get_latest_release()

  if (current != latest) {
    warning(paste0("Your version of the arcticdatautils package is ", current, " but the latest version is ", latest, ".\n  You should upgrade as there may be important bug fixes in newer versions of the package.\n  See https://github.com/NCEAS/arcticdatautils#installing for instructions."))
  }
}


