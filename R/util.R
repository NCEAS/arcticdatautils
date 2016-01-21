#' util.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' General utility functions that may be later merged into other files.

library(stringr)
library(xml2)


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

  xml_file <- read_xml(file)
  xml_ns <- xml_ns(xml_file)

  if (type == "gateway") {
    identifier_text <- xml_find_all(xml_file, "//gmd:fileIdentifier/gco:CharacterString/text()", xml_ns)
  }
  else if (type == "field-projects") {
    identifier_text <- xml_find_all(xml_file, "//gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:identifier/gmd:MD_Identifier/gmd:code/gco:CharacterString/text()", xml_ns)
  }

  stopifnot(length(identifier_text) == 1)

  if (type == "gateway") {
    identifier <- as.character(identifier_text)
  }
  else if (type == "field-projects") {
    identifier <- str_extract(identifier_text, "\\d+\\.\\d+")
  }

  if (is.na(identifier)) { stop("Failed to extract identifier.") }

  identifier
}
