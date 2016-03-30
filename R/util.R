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
#' @param path Path to the file to convert. (character)
#'
#' @return Location of the converted file (character)
#' @export
#'
#' @examples
convert_iso_to_eml <- function(path) {
  tmpfile <- tempfile(fileext = ".xml")
  full_path <- paste0(env$base_path, "/", path)
  stopifnot(file.exists(full_path))

  doc <- tryCatch({
    xml2::read_xml(full_path)
  },
  warning = function(w) {
    log_message(w)
  },
  error = function(e) {
    log_message(e)
  })

  # Hack fix, I can change this later so I don't have to do WD stuff
  old_wd <- getwd()
  setwd("~/src/iso2eml/src")

  transformed_document <- tryCatch({
    xslt::xslt_transform(doc, isotoeml)
  },
  warning = function(w) {
    log_message(w)
  },
  error = function(e) {
    log_message(e)
  })

  xml2::write_xml(transformed_document, tmpfile)

  setwd(old_wd)

  tmpfile
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
  stopifnot(file.exists(path),
            is.character(replacement),
            nchar(replacement) > 0)

  lines <- readLines(con = path)

  package_id_line <- which(str_detect(lines, "packageId") == TRUE)
  stopifnot(length(package_id_line) == 1)

  lines[package_id_line] <- str_replace(lines[package_id_line],
                                        "packageId=\"(.+)\"",
                                        paste0("packageId=\"", replacement ,"\""))
  writeLines(lines, con = path)
}

