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
dataone_format_mappings <- list("txt" = "text/plain",
                                "csv" = "text/csv",
                                "bmp" = "image/bmp",
                                "gif" = "image/gif",
                                "jpeg" = "image/jpeg",
                                "png" = "image/png",
                                "xml" = "http://www.isotc211.org/2005/gmd",
                                "bz2" = "application/x-bzip2",
                                "zip" = "application/zip",
                                "tar" = "application/x-tar")

guess_format_id <- function(filenames) {
  extensions <- tolower(tools::file_ext(filenames))
  filetypes <- vector(mode = "character", length = length(extensions))

  for (i in seq_len(length(extensions))) {
    if (extensions[i] %in% names(dataone_format_mappings)) {
      filetypes[i] <- dataone_format_mappings[extensions[i]][[1]]
    } else {
      filetypes[i] <- "application/octet-stream"
    }
  }

  filetypes
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

  # Prepare the message
  message <- paste0("[", as.POSIXlt(Sys.time(), "GMT"), "] ", stringr::str_replace_all(message, "[\n]", ""))

  # Write it out to stdout and a log
  cat(paste0(message, "\n"))
  write(message,
        file = "arcticdata-log.txt",
        append = TRUE)


  invisible(TRUE)
}
