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
    log_message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
    return(sysmeta)
  }

  # Add myself explicitly as changePermission/write so I can update objects
  # in the dev environment
  if (env_get() == "development") {
    sysmeta <- datapack::addAccessRule(sysmeta, env$submitter, "changePermission")
  }

  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "read")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")

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


#' Get the related PIDs for a given PID (i.e. data files, resource maps).
#'
#' @param mn
#' @param pid
#'
#' @return (character) Named character vector.
#' @export
#'
#' @examples
get_related_pids <- function(mn, pid) {
  stopifnot(class(mn) == "MNode",
            is.character(pid),
            length(pid) == 1,
            nchar(pid) > 0)

  # Prepare the query parameters
  pid_esc <- stringi::stri_replace_all_fixed(pid, ":", "\\:")
  queryParams <- list(q = paste0("id:", pid_esc),
                      rows = "1000",
                      fl = "identifier,resourceMap,documents")

  # Wrap in a try-catch to handle ParseExceptionsn and the like
  response <- tryCatch({
    dataone::query(mn, queryParams, as = "list")
  },
  error = function(e) {
    log_message("Error occurred during query.")
    log_message(e)
    e
  })

  # Unlist the response
  response <- unlist(response)

  # Manually set the response to a zero-length character vector when it's NULL
  if (is.null(response)) {
    response <- vector(mode = "character", length = 0L)
  }

  response
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
