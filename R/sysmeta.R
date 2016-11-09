#' sysmeta.R
#'
#' Utility functions for modifying System Metadata objects.


#' Add access rules to the sysmeta object
#'
#' This is a function because I add a set of standard set of access rules to
#' every object and the access rules don't differ across objects.
#'
#' @param sysmeta (SystemMetadata) The SystemMetadata to add rules to.
#'
#' @return The modified SystemMetadata object
#' @export
#'
#' @examples
add_access_rules <- function(sysmeta) {
  if (!inherits(sysmeta, "SystemMetadata")) {
    stop(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
  }

  # Add myself explicitly as changePermission/write so I can update objects
  # in the dev environment
  if (env_get() == "development") {
    sysmeta <- datapack::addAccessRule(sysmeta, env_load(skip_mn = TRUE)$submitter, "changePermission")
  }

  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "read")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")

  sysmeta
}


#' Remove all public read access rules from a System Metadata document
#'
#' @param sysmeta (SystemMetadata) The System Metadata document. to change.
#'
#' @return (SystemMetadata) The potentially modified System Metadata document.
#'
#' @examples
#' library(datapack)
#' sm <- new("SystemMetadata)
#' sm <- addAccessRule(sm, "public", "read")
#' sm@accessPolicy
#' sm <- remove_public_access(sm)
#' sm@accessPolicy
remove_public_access <- function(sysmeta) {
  if (!inherits(sysmeta, "SystemMetadata")) {
    stop(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
  }

  sysmeta@accessPolicy <- sysmeta@accessPolicy[!(grepl("public", sysmeta@accessPolicy$subject) & grepl("read", sysmeta@accessPolicy$permission)),]

  sysmeta
}


#' Adds access to the given System Metadata for the arctic-data-admins group
#'
#' @param sysmeta
#'
#' @return
#' @export
#'
#' @examples
add_admin_group_access <- function(sysmeta) {
  if (!inherits(sysmeta, "SystemMetadata")) {
    log_message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
    return(sysmeta)
  }

  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "read")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")

  sysmeta
}


#' Replace subjects in the accessPolicy section of a System Metadata entries.
#'
#' This function was written out to fix capitalization errors but in a set of
#' existing System Metadata entries but can be used to replace any subject.
#'
#'
#' @param sysmeta (SystemMetadata) The System Metadata object.
#' @param from (character) The DN string to replace.
#' @param to (character) The DN string to put in place of `from`.
#'
#' @return The modified System Metadata (SystemMetadata)
#' @export
#'
#' @examples
replace_subject <- function(sysmeta,
                            from="cn=arctic-data-admins,dc=dataone,dc=org",
                            to="CN=arctic-data-admins,DC=dataone,DC=org") {
  if (!inherits(sysmeta, "SystemMetadata")) {
    log_message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
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
#' @export
#'
#' @examples
clear_replication_policy <- function(sysmeta) {
  if (!(class(sysmeta) == "SystemMetadata")) {
    stop("First argument was not of class SystemMetadata.")
  }

  slot(sysmeta, "replicationAllowed") <- FALSE
  slot(sysmeta, "numberReplicas") <- 3

  sysmeta
}
