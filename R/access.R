#' access.R
#'
#' Utility functions for getting and setting access rules for DataONE objects.


#' Add access rules to the sysmeta object
#'
#' This is a function because I add a set of standard set of access rules to
#' every object and the access rules don't differ across objects.
#'
#' @param sysmeta The SystemMetadata to add rules to (SystemMetadata)
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
#' @param flatten Whether to flatten the result into a single character vector (logical)
#'
#' @return Either a character vector or list
#' @export
#'
#' @examples
get_related_pids <- function(mn, pid, flatten = TRUE) {
  stopifnot(class(mn) == "MNode",
            is.character(pid),
            nchar(pid) > 0)

  # Escape the PID so we can send it to Solr
  # Here I escape colons only, and with a ? instead of
  pid_esc <- gsub(":", "?", pid)
  # pid_esc <- URLencode(pid, reserved = TRUE)

  response <- solr::solr_search(q = sprintf("id:%s", pid_esc),
                                fl = "identifier,resourceMap,documents",
                                rows = 1000,
                                base = paste0(mn@endpoint, "/query/solr"))

  if (is.null(response)) {
    warning(paste0("Response was NULL for pid ", pid, "."))
    return(character(length = 0))
  }

  if (!is.data.frame(response)) {
    warning(paste0("Response was not a data.frame but was of class ", class(response), " instead."))
    return(character(length = 0))
  }

  if (nrow(response) != 1) {
    warning(paste0("Response did not have one row, as expected, but had ", nrow(response), " rows."))
    return(character(length = 0))
  }

  if (flatten == TRUE) {
    return(unique(unlist(c(response$identifier,
                    stringr::str_split(response$documents, ","),
                    response$resourceMap))))
  } else {
    return(list(pid = response$identifier,
                documents = stringr::str_split(response$documents, ","),
                resource_map = response$resourceMap))
  }

}


#' Change the rightsHolder field for a given PID.
#'
#' Update the rights holder to the provided subject for the object identified in
#' the provided system metadata document on the given Member Node.
#'
#' @param mn the MNode instance to be changed (MNode)
#' @param pids the identifiers for the object to be changed (character)
#' @param subject the identifier of the new rightsHolder, often an ORCID or DN (character)
#' @import dataone
#' @import datapack
#' @export
update_rights_holder <- function(mn, pids, subject) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pids),
            all(nchar(pid) > 0))
  stopifnot(is.character(subject),
            nchar(subject) > 0)

  result <- vector(mode = "logical", length = length(pids))

  for (i in seq_along(pids)) {
    pid <- pids[i]

    # Get System Metadata
    sysmeta <- dataone::getSystemMetadata(mn, pid)

    # Change rightsHolder (if needed)
    if (sysmeta@rightsHolder == subject) {
      log_message(paste0("rightsHolder field is already set to ", subject, ". System Metadata not updated."))
      result[i] <- TRUE
    } else {
      sysmeta@rightsHolder <- subject

      # Update System Metadata
      log_message(paste0("Updating rightsHolder for PID ", pid, " to ", subject, "."))
      response <- tryCatch({
        dataone::updateSystemMetadata(mn,
                                      pid = pid,
                                      sysmeta = sysmeta)
      },
      error = function(e) {
        log_message(e)
        e
      })

      if (inherits(response, "error")) {
        result[i] <- FALSE
      } else {
        result[i] <- TRUE
      }
    }
  }

  return(result)
}


#' Set the given subject as the rightsHolder and subject with write and
#' changePermission access for the given PID.
#'
#' This function only updates the existing System Metadata if a change is
#' needed.
#'
#' @param mn The Member Node to send the query (MNode)
#' @param pid The PID to set the access rule for (character)
#' @param subject The subject of the rule(s) (character)
#' @param permissions The permissions for the rule (character)
#'
#' @return Whether an updated was needed.
#' @export
#'
#' @examples
set_rights_and_access <- function(mn, pid, subject, permissions) {
  stopifnot(class(mn) == "MNode",
            is.character(pid),
            nchar(pid) > 0,
            is.character(subject),
            is.character(permissions))

  sysmeta <- tryCatch({
    dataone::getSystemMetadata(mn, pid)
  },
  error = function(e) {
    log_message(paste0("Failed to get system metadata for PID '", pid, "' on MN '", env$mn_base_url, "'.\n"))
    log_message(e)
    e
  })

  if (inherits(sysmeta, "error")) {
    stop("Failed to get System Metadat.")
  }

  # Track whether we have changed the record to avoid an uncessary update call
  changed <- FALSE

  # Set rights holder if needed
  if (subject != sysmeta@rightsHolder) {
    changed <- TRUE

    cat("Setting rights holder to ", subject, ".\n")
    sysmeta@rightsHolder <- subject
  } else {
    cat("Skipping setting rightsHolder as rightsHolder is already ", sysmeta@rightsHolder, ".\n")
  }

  for (permission in permissions) {
    if (datapack::hasAccessRule(sysmeta, subject, permission)) {
      cat(paste0("Skipping the addition of permission '", permission, "' for subject '", subject, "'\n"))
      next
    }

    changed <- TRUE

    cat(paste0("Adding permission '", permission, "' for subject '", subject, "'\n"))
    sysmeta <- datapack::addAccessRule(sysmeta, subject, permission)
  }

  if (changed == TRUE) {
    cat("Updating sysmeta.\n")

    update_response <- tryCatch({
      dataone::updateSystemMetadata(mn, pid, sysmeta)
    },
    error = function(e) {
      log_message(paste0("Failed to update System Metadata for PID '", pid, "'.\n"))
      log_message(e)
      e
    })

    if (inherits(update_response, "error")) {
      stop("Failed update.")
    }
  } else {
    cat("No changes needed.\n")
  }

  changed
}

#' Replace subjects in the accessPolicy section of a System Metadata entries.
#'
#' This function was written out to fix capitalization errors but in a set of
#' existing System Metadata entries but can be used to replace any subject.
#'
#'
#' @param sysmeta The System Metadata object (SystemMetadata)
#' @param from The DN string to replace (character)
#' @param to The DN string to put in place of `from` (character)
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
