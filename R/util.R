#' util.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' General utility functions that may be later merged into other files.


#' Extracts the local identifier for an ACADIS ISO metadata XML file.
#'
#' @param type: A string, one of "gateway" or "field-projects". (character)
#' @param file: A string, a connection, or raw vector (same as xml2::read_xml).
#'  (character)
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


#' Guess format from filename for a vector of filenames.
#'
#' @param filenames (character)
#'
#' @return DataOne format identifiers (character)
#' @export
#'
#' @examples
#'
# TODO: NETCDF and other formats that aren't obvious from file ext
# TODO: Put this with the package as data
# TODO CLS DAT ASC NETCDF
# TODO CHECK IF I DEAL WITH .TAR.GZ well OR IF I HAVE TO
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
#' @param path Full or relative path to the file in question. (character)
#'
#' @return The DataONE format ID (character)
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
#' @param inventory An inventory (data.frame)
#' @param theme (optional) A package theme name (character)
#' @param n (optional) The number of files to show (numeric)
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
#' @param message Your log message (character)
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

  # Write it out to stdout and a log
  cat(paste0(message, "\n"))
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
#' @param mn_base_url Member Node to query (character)
#' @param pid PID to check the existence of (character)
#'
#' @return Whether the object exists (logical)
#' @export
#'
#' @examples
object_exists <- function(mn_base_url, pid) {
  stopifnot(is.character(mn_base_url),
            nchar(mn_base_url) > 0,
            is.character(pid),
            nchar(pid) > 0)

  url <- paste0(mn_base_url, "/meta/", pid)
  response <- httr::GET(url)

  if (!inherits(response, "response") || response$status_code != 200){
    return(invisible(FALSE))
  }

  invisible(TRUE)
}


#' Convert and ISO document to EML using an XSLT.
#'
#' This is a better of a nasty function right now but it essentially
#' takes a file in, loads an XSLT file and does the conversion. The nasty part
#' is that I have the XSLT file in a hard-linked directory on my computer
#' because we're still actively developing the XSLT.
#'
#' @param full_path Path to the file to convert. (character)
#' @param isotoeml the XSLT object to be used for transformation
#'
#' @return Location of the converted file (character)
#' @export
#'
#' @examples
convert_iso_to_eml <- function(full_path, isotoeml=isotoeml) {
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
#' @return path to the converted EML file (character)
#' @import XML
#' @export
substitute_eml_party <- function(path) {

  # Read in the EML document
  ns <- c(eml='eml://ecoinformatics.org/eml-2.1.1')
  eml = XML::xmlParse(path)

  # For each of the creator, contact, associatedParty elements
  creators <- XML::getNodeSet(eml, '/eml:eml/dataset/creator', ns)
  rplist <- sapply(creators, change_eml_name)
  associates <- XML::getNodeSet(eml, '/eml:eml/dataset/associatedParty', ns)
  rplist <- sapply(associates, change_eml_name)
  contacts <- XML::getNodeSet(eml, '/eml:eml/dataset/contact', ns)
  rplist <- sapply(contacts, change_eml_name)

  # Serialize the EML document to disk
  XML::saveXML(eml, file=path)

  # Return the EML path
  return(path)
}

#' Utility function to extract a name string from an XML individualName node,
#' parse it into tokens,and reformat the individualName with new children nodes
#'
#' @param party the XML node containing a subclass of eml:ResponsibleParty
#' @return the modified XML node
#' @import XML
#' @export
change_eml_name <- function(party) {
  # Parse out the surName and givenName of the party
  user_name <- XML::xpathApply(party, "./individualName/surName", XML::xmlValue)

  # TODO: if no individualName exists, just move on

  parts <- stringr::str_split(user_name, ' ')
  eml_suffix = as.character(NA)

  # Check for suffix (Jr., III)
  last <- length(parts[[1]])
  if (parts[[1]][last]=="Jr." || parts[[1]][last]=="III") {
    eml_suffix <- parts[[1]][length(parts[[1]])]
    last <- last-1
  }
  eml_surname <- parts[[1]][last]
  eml_givenname <- paste(parts[[1]][1:(last-1)], collapse=' ')

  # Create a new individualName Node
  child_nodes <- list(XML::xmlNode("givenName", eml_givenname))
  if (!is.na(eml_suffix)) {
    suffix <- XML::xmlNode("givenName", eml_suffix)
    child_nodes[[length(child_nodes)+1]] <- suffix
  }
  sn <- XML::xmlNode("surName", eml_surname)
  child_nodes[[length(child_nodes)+1]] <- sn
  name_node <- XML::xmlNode("individualName", .children=child_nodes)

  # Substitute in a new name structure for the old in the EML document
  XML::replaceNodes(party[['individualName']], name_node)
}


#' Replace the EML 'packageId' attribute on the root element with a
#' certain value.
#'
#' Note: I am just using string manipulation (replacement) because I don't want
#' the XML package to have re-format the XML when I edit it and write it back
#' to disk.
#'
#' @param path Path to the XML file to edit. (character)
#' @param replacement The new value (character)
#'
#' @return
#' @export
#'
#' @examples
replace_package_id <- function(path, replacement) {
  stopifnot(file.exists(path))
  stopifnot(is.character(replacement),
            nchar(replacement) > 0)

  result <- tryCatch({
    xmldoc <- XML::xmlParseDoc(file = path)
    root <- XML::xmlRoot(xmldoc)
    XML::xmlAttrs(root) <- c(packageId = replacement)
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

add_additional_identifiers <- function(file,
                                       eml_file_path,
                                       identifiers) {
  stopifnot(is.character(file),
            nchar(file) > 0,
            file.exists(eml_file_path),
            is.data.frame(identifiers),
            nrow(identifiers) == 1)

  # Gather the identifiers into a usable format
  identifier_types <- c("file_identifier", "identifier", "primary_key", "alternative_identifier")
  alternate_identifiers <- c()

  for (ident_type in identifier_types) {
    # Skip NA cells
    if (!(ident_type %in% names(identifiers)) || is.na(identifiers[1,ident_type])) {
      next
    }

    alternate_identifiers <- c(alternate_identifiers, identifiers[1,ident_type])
  }

  doc <- XML::xmlParseDoc(file = eml_file_path)
  datasets <- XML::getNodeSet(doc, "//dataset/title")

  # Stop here if there are more than one <dataset> elements
  if (length(datasets) != 1) {
    return(eml_file_path)
  }

  for (identifier in alternate_identifiers) {
    log_message(paste0("Adding alternate identifier of ", identifier, "\n"))
    new_node <- XML::newXMLNode("alternateIdentifier", identifier)
    XML::addSibling(datasets[[1]], new_node, after = FALSE)
  }

  XML::saveXML(doc, eml_file_path, indent = TRUE)

  eml_file_path
}

create_alternate_identifier_node <- function(identifier) {
  stopifnot(is.character(identifier),
            nchar(identifier) > 0)


}


#' (Intelligently) join (possibly redudant) path parts together.
#'
#' Joins path strings like "./" to "./my/dir" as "./my/dir" instead of as
#' "././my/dir.
#'
#' @param path_parts (character)
#'
#' @return The joined path string (character)
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

