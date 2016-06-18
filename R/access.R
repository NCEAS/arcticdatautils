#' access.R
#'
#' Utility functions for getting and setting access rules for DataONE objects.


#' Get a structured list of PIDs for the objects in a package.
#'
#' @param mn (MNode) The Member Node to run the query on.
#' @param pid (character) The the metadata PID of the package.
#'
#' @return
#' @export
#'
#' @examples
get_package <- function(mn, pid) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  # Warn if `pid` looks like a resource map
  if(grepl("resource", pid)) {
    warning(paste0("Value of argument PID is ", pid, " which looks like a resource map. This function expects a metadata PID."))
  }

  # Prepare the query parameters
  pid_esc <- stringi::stri_replace_all_fixed(pid, ":", "\\:")
  queryParams <- list(q = paste0("id:", pid_esc),
                      rows = "1000",
                      fl = "identifier,resourceMap,documents")

  response <- dataone::query(mn, queryParams, as = "list")

  if (length(response) == 0) {
    return(response)
  }

  # Get all the PIDs we need
  identifier <- unlist(response[[1]]$identifier)
  resource_map = unlist(response[[1]]$resourceMap)
  documents <- unlist(response[[1]]$documents)
  data_pids <- documents[(!grepl(identifier, documents) & !grepl("resource", documents))]
  child_packages <- documents[(!grepl(identifier, documents) & grepl("resource", documents))]

  list(metadata = identifier,
       resource_map = resource_map,
       data = data_pids,
       child_packages = child_packages)
}


#' Set the rightsHolder field for a given PID.
#'
#' Update the rights holder to the provided subject for the object identified in
#' the provided system metadata document on the given Member Node.
#'
#' @param mn (MNode) The MNode instance to be changed.
#' @param pids (character) The identifiers for the object to be changed.
#' @param subject (character) The identifier of the new rightsHolder, often an ORCID or DN.
#'
#' @import dataone
#' @import datapack
#' @export
set_rights_holder <- function(mn, pids, subject) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pids),
            all(nchar(pids) > 0))
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


#' Set the access policy for a set of objects.
#'
#' For each permission, this function checks if the permission is already set
#' and moves on. System Metadata are only updated when a change was needed.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The object(s) to set the permissions on.
#' @param subjects (character) The subject(s) to set permissions for.
#' @param permissions (character) Optional. Vector of permissions.
#'
#' @return (logical) Named
#' @export
#'
#' @examples
set_access <- function(mn, pids, subjects, permissions=c("read", "write", "changePermission")) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pids),
            nchar(pids) > 0)
  stopifnot(is.character(subjects),
            nchar(subjects) > 0)
  stopifnot(all(permissions %in% c("read", "write", "changePermission")))

  result <- c()

  for (pid in pids) {
    changed <- FALSE

    sysmeta <- dataone::getSystemMetadata(mn, pid)

    for (subject in subjects) {
      for (permission in permissions) {
        if (!datapack::hasAccessRule(sysmeta, subject, permission)) {
          sysmeta <- datapack::addAccessRule(sysmeta, subject, permission)
          changed <- TRUE
        }
      }
    }

    if (changed) {
      result[pid] <- TRUE
      log_message(paste0("Updating System Metadata for ", pid, "."))
      dataone::updateSystemMetadata(mn, pid, sysmeta)
    } else {
      log_message(paste0("No changes needed for ", pid, ". Not updating."))
      result[pid] <- FALSE
    }
  }

  # Name the result vector
  names(result) <- pids

  result
}


#' Set public access on a set of objects.
#'
#' @param mn (MNode)
#' @param pids (character) A vector of PIDs to set public access on
#'
#' @return
#' @export
#'
#' @examples
set_public_read <- function(mn, pids) {
  set_access(mn, pids, "public", "read")
}

#' Remove public access on a set of objects.
#'
#' @param mn (MNode)
#' @param pids (character) A vector of PIDs to set public access on
#'
#' @return
#' @export
#'
#' @examples
remove_public_read <- function(mn, pids) {
  stopifnot(class(mn) == "MNode",
            all(is.character(pids)),
            all(nchar(pids) > 0))

  # Store the results of each attempted update
  results <- c()

  # Remove public access for each PID
  for (pid in pids) {
    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    },
    error = function(e) {
      log_message(paste0("Failed to get system metadata for PID '", pid, "' on MN '", mn@endpoint, "'.\n"))
      log_message(e)
      e
    })

    if (inherits(sysmeta, "error")) {
      stop("Failed to get System Metadata.")
    }

    # Track whether we have changed the record to avoid an uncessary update call
    changed <- FALSE

    if (!datapack::hasAccessRule(sysmeta, "public", "read")) {
      log_message(paste0("Skipping setting public read because ", pid, " is not public."))
      next
    }

    changed <- TRUE

    log_message(paste0("Removing public read access on ", pid, "."))
    sysmeta@accessPolicy <- sysmeta@accessPolicy[!(grepl("public", sysmeta@accessPolicy$subject) & grepl("read", sysmeta@accessPolicy$permission)),]

    # Update the sysmeta
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

    # Save the result for this PID
    results[pid] <- changed
  }

  results
}


#' Set the given subject as the rightsHolder and subject with write and
#' changePermission access for the given PID.
#'
#' This function only updates the existing System Metadata if a change is
#' needed.
#'
#' @param mn (MNode) The Member Node to send the query.
#' @param pids (character) The PID(s) to set the access rule for.
#' @param subject (character)The subject of the rule(s).
#' @param permissions (character) The permissions for the rule.
#'
#' @return Whether an update was needed.
#' @export
#'
#' @examples
set_rights_and_access <- function(mn, pids, subject, permissions) {
  stopifnot(class(mn) == "MNode",
            all(is.character(pids)),
            all(nchar(pids) > 0),
            is.character(subject),
            is.character(permissions))

  # Store the results of each attempted update
  results <- c()

  # Set rights and access for each PID
  for (pid in pids) {
    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    },
    error = function(e) {
      log_message(paste0("Failed to get system metadata for PID '", pid, "' on MN '", mn@endpoint, "'.\n"))
      log_message(e)
      e
    })

    if (inherits(sysmeta, "error")) {
      stop("Failed to get System Metadata.")
    }

    # Track whether we have changed the record to avoid an uncessary update call
    changed <- FALSE

    # Set rights holder if needed
    if (subject != sysmeta@rightsHolder) {
      changed <- TRUE

      cat("Setting rights holder to ", subject, ".\n")
      sysmeta@rightsHolder <- subject
    } else {
      log_message(paste0("Skipping setting rightsHolder as rightsHolder is already ", sysmeta@rightsHolder, ".\n"))
    }

    for (permission in permissions) {
      if (datapack::hasAccessRule(sysmeta, subject, permission)) {
        log_message(paste0("Skipping the addition of permission '", permission, "' for subject '", subject, "'\n"))
        next
      }

      changed <- TRUE

      log_message(paste0("Adding permission '", permission, "' for subject '", subject, "'\n"))
      sysmeta <- datapack::addAccessRule(sysmeta, subject, permission)
    }

    if (changed == TRUE) {
      log_message(paste0("Updating System Metadata for ", pid, "."))

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
      log_message(paste0("No changes needed for ", pid, "."))
    }

    # Save the result for this PID
    results[pid] <- changed
  }

  results
}
