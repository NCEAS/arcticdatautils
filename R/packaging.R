#' package.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Code related to packaging datasets.

library(whisker)

#' Create a Resource Map XML string suitable for use in an MN.Create() call
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
create_resource_map <- function(variables) {
  "some_xml"
}


#' Generate a System Metadata XML string suitable for use in an MN.Create() call
#'
#' Relevant documentation:
#' https://jenkins-ucsb-1.dataone.org/job/API%20Documentation%20-%20trunk/ws/api-documentation/build/html//design/SystemMetadata.html#id3
#'
#'
#' @param identifier
#' @param size
#' @param checksum
#' @param submitter
#' @param rightsHolder
#' @param checksumAlgorithm
#' @param formatID
#' @param replication
#'
#' @return The text of the file (character)
#' @export
#'
#' @examples
#' generate_system_metadata(identifier="IDENT",
#'                        size="1234",
#'                        checksum="ae626a6d626a6d626a6d6",
#'                        submitter="some_submitter",
#'                        rightsHolder = "some_submitter")

generate_system_metadata <- function(identifier,
                                     size,
                                     checksum,
                                     submitter,
                                     rightsHolder,
                                     checksumAlgorithm="SHA256",
                                     formatID="http://www.isotc211.org/2005/gmd",
                                     replication="true") {
  filepath <- system.file("data/sysmeta_template.xml", package="arcticdata")
  stopifnot(file.exists(filepath))

  template <- readChar(filepath, file.info(filepath)$size)
  stopifnot(nchar(template) > 0)

  values <- list(identifier = identifier,
                 formatID = formatID,
                 size = size,
                 checksumAlgorithm = checksumAlgorithm,
                 checksum = checksum,
                 submitter = submitter,
                 rightsHolder = rightsHolder,
                 replication = replication,

  text <- whisker.render(template, values)
  cat(text)
}


