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

  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")

  sysmeta
}


get_related_pids <- function(mn, pid) {
  stopifnot(is.character(mn),
            is.character(pid),
            nchar(pid) > 0)

  # Escape the PID so we can send it to Solr
  # Here I escape colons only, and with a ? instead of
  pid_esc <- gsub(":", "?", pid)
  # pid_esc <- URLencode(pid, reserved = TRUE)

  response <- solr::solr_search(q = sprintf("id:%s", pid_esc),
                                fl = "identifier,resourceMap,documents",
                                rows = 1000,
                                base = paste0(mn, "/query/solr"))

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

  cat("\nDebugging response..\n")
  print(response)
  cat("\n\n")

  unlist(c(response$identifier,
           stringr::str_split(response$documents, ","),
           response$resourceMap))
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
