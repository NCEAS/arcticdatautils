# Utility functions for modifying System Metadata objects


#' Replace subjects in the accessPolicy section of a System Metadata entries
#'
#' This function was written out to fix capitalization errors in a set of
#' existing System Metadata entries but can be used to replace any subject.
#'
#' @param sysmeta (SystemMetadata) The System Metadata object.
#' @param from (character) The DN string to replace.
#' @param to (character) The DN string to put in place of 'from'.
#'
#' @return (SystemMetadata) The modified System Metadata.
#'
#' @noRd
replace_subject <- function(sysmeta,
                            from = "cn=arctic-data-admins,dc=dataone,dc=org",
                            to = "CN=arctic-data-admins,DC=dataone,DC=org") {
  if (!inherits(sysmeta, "SystemMetadata")) {
    message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
    return(sysmeta)
  }

  # Get the access policy data.frame
  ap <- sysmeta@accessPolicy

  # Convert subject column from factor to string
  # We do this so we can assign new values to it without dealing with factor
  # nonsense.
  ap$subject <- as.character(ap$subject)

  # Replace the subjects
  ap[which(ap$subject == from),"subject"] <- to
  sysmeta@accessPolicy <- ap

  sysmeta
}


#' Clear the replication policy from a System Metadata object
#'
#' @param sysmeta (SystemMetadata) The System Metadata object to clear the replication policy of.
#'
#' @return (SystemMetadata) The modified System Metadata object.
#'
#' @noRd
clear_replication_policy <- function(sysmeta) {
  if (!(is(sysmeta, "SystemMetadata"))) {
    stop("First argument was not of class SystemMetadata.")
  }

  slot(sysmeta, "replicationAllowed") <- FALSE
  slot(sysmeta, "numberReplicas") <- 0
  slot(sysmeta, "blockedNodes") <- list('urn:node:KNB', 'urn:node:mnUCSB1')

  sysmeta
}

