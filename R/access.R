# High-level utility functions for getting and setting access rules for DataONE objects


#' Add access rules to the sysmeta object
#'
#' This is a function to add a standard set of access rules to
#' every object so that the access rules don't differ across objects.
#'
#' @param sysmeta (SystemMetadata) The SystemMetadata to add rules to.
#'
#' @return The modified SystemMetadata object.
#'
#' @noRd
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


#' Add access to the given System Metadata for the arctic-data-admins group
#'
#' @param sysmeta (sysmeta) System Metadata object.
#'
#' @noRd
add_admin_group_access <- function(sysmeta) {
  if (!inherits(sysmeta, "SystemMetadata")) {
    message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
    return(sysmeta)
  }

  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "read")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")

  sysmeta
}


#' Set the rights holder for an object
#'
#' Set the rights holder to the given subject for the given objects on the
#' given Member Node. This function checks if the rights holder is already set
#' and only updates the System Metadata when a change is needed.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to set the rights holder for.
#' @param subject (character) The identifier of the new rights holder, typically an ORCID or DN.
#'
#' @return (logical) Whether an update was needed.
#'
#' @import dataone
#' @import datapack
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#' "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' set_rights_holder(mn, pids, subjects = "http://orcid.org/0000-000X-XXXX-XXXX")
#'}
set_rights_holder <- function(mn, pids, subject) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subject),
           nchar(subject) > 0)) {
    stop("Argument 'subject' must be character class with non-zero number of characters.")
  }

  if (grepl("^https:\\/\\/orcid\\.org", subject)) {
    stop("Argument 'subject' cannot contain 'https:', use 'http:' instead.")
  }


  result <- vector(mode = "logical", length = length(pids))

  for (i in seq_along(pids)) {
    pid <- pids[i]

    # Get System Metadata
    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    }, warning = function(w) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      w
    }, error = function(e) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      message(e)
      e
    })

    if (!inherits(sysmeta, "SystemMetadata")) {
      stop("Failed to get System Metadata.")
    }

    # Change rightsHolder (if needed)
    if (sysmeta@rightsHolder == subject) {
      message(paste0("rightsHolder field is already set to ", subject, ". System Metadata not updated."))
      result[i] <- TRUE
    } else {
      # Update System Metadata
      message(paste0("Updating rightsHolder for PID ", pid, " from ", sysmeta@rightsHolder, " to ", subject, "."))

      sysmeta@rightsHolder <- subject

      response <- tryCatch({
        dataone::updateSystemMetadata(mn,
                                      pid = pid,
                                      sysmeta = sysmeta)
      },
      error = function(e) {
        message(e)
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


#' Set the access policy for an object
#'
#' Set the access policy for the given subjects for the given objects on the given Member Node.
#' For each type of permission, this function checks if the permission is already set
#' and only updates the System Metadata when a change is needed.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to set permissions for.
#' @param subjects (character) The identifiers of the subjects to set permissions for, typically an ORCID or DN.
#' @param permissions (character) Optional. The permissions to set. Defaults to
#'   read, write, and changePermission.
#'
#' @return (logical) Whether an update was needed.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#'    "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' set_access(mn, pids, subjects = "http://orcid.org/0000-000X-XXXX-XXXX",
#'    permissions = c("read", "write", "changePermission"))
#'}
set_access <- function(mn, pids, subjects, permissions = c("read", "write", "changePermission")) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subjects),
           all(nchar(subjects)) > 0)) {
    stop("Argument 'subjects' must be character class with non-zero number of characters.")
  }

  if (any(grepl("^https:\\/\\/orcid\\.org", subjects))) {
    stop("Argument 'subjects' cannot contain 'https:', use 'http:' instead.")
  }

  if (!all(permissions %in% c("read", "write", "changePermission"))) {
    stop("Argument 'permissions' must be one or more of: 'read', 'write', 'changePermission'")
  }


  result <- c()

  for (pid in pids) {
    changed <- FALSE

    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    }, warning = function(w) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      w
    }, error = function(e) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      message(e)
      e
    })

    if (!inherits(sysmeta, "SystemMetadata")) {
      stop("Failed to get System Metadata.")
    }

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
      message(paste0("Updating System Metadata for ", pid, "."))
      dataone::updateSystemMetadata(mn, pid, sysmeta)
    } else {
      message(paste0("No changes needed for ", pid, ". Not updating."))
      result[pid] <- FALSE
    }
  }

  # Name the result vector
  names(result) <- pids

  result
}

#' Remove a subject from an object's access policy
#'
#' Remove the given subjects from the access policy for the given objects on the given Member Node.
#' For each type of permission, this function checks if the permission is already set
#' and only updates the System Metadata when a change is needed.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to set permissions for.
#' @param subjects (character) The identifiers of the subjects to set permissions for, typically an ORCID or DN.
#' @param permissions (character) Optional. The permissions to set. Defaults to
#'   read, write, and changePermission.
#'
#' @return (logical) Whether an update was needed.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#'    "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' remove_access(mn, pids, subjects = "http://orcid.org/0000-000X-XXXX-XXXX",
#'    permissions = c("read", "write", "changePermission"))
#'}
remove_access <- function(mn, pids, subjects, permissions = c("read", "write", "changePermission")) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subjects),
           all(nchar(subjects)) > 0)) {
    stop("Argument 'subjects' must be character class with non-zero number of characters.")
  }

  if (any(grepl("^https:\\/\\/orcid\\.org", subjects))) {
    stop("Argument 'subjects' cannot contain 'https:', use 'http:' instead.")
  }

  if (!all(permissions %in% c("read", "write", "changePermission"))) {
    stop("Argument 'permissions' must be one or more of: 'read', 'write', 'changePermission'")
  }


  result <- c()

  for (pid in pids) {
    changed <- FALSE

    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    }, warning = function(w) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      w
    }, error = function(e) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      message(e)
      e
    })

    if (!inherits(sysmeta, "SystemMetadata")) {
      stop("Failed to get System Metadata.")
    }

    for (subject in subjects) {
      for (permission in permissions) {
        if (datapack::hasAccessRule(sysmeta, subject, permission)) {
          sysmeta <- datapack::removeAccessRule(sysmeta, subject, permission)
          changed <- TRUE
        }
      }
    }

    if (changed) {
      result[pid] <- TRUE
      message(paste0("Updating System Metadata for ", pid, "."))
      dataone::updateSystemMetadata(mn, pid, sysmeta)
    } else {
      message(paste0("No changes needed for ", pid, ". Not updating."))
      result[pid] <- FALSE
    }
  }

  # Name the result vector
  names(result) <- pids

  result
}


#' Set rights holder with access policy for an object
#'
#' Set the given subject as the rights holder and with given permissions
#' for the given objects. This function only updates the existing
#' System Metadata when a change is needed.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to set the rights holder and access policy for.
#' @param subject (character) The identifier of the new rights holder, typically an ORCID or DN.
#' @param permissions (character) Optional. The permissions to set. Defaults to
#'   read, write, and changePermission.
#'
#' @return (logical) Whether an update was needed.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#'     "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' set_rights_and_access(mn, pids, "http://orcid.org/0000-000X-XXXX-XXXX",
#'     permissions = c("read", "write", "changePermission"))
#'}
set_rights_and_access <- function(mn, pids, subject, permissions = c("read", "write", "changePermission")) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subject),
           nchar(subject) > 0)) {
    stop("Argument 'subject' must be character class with non-zero number of characters.")
  }

  if (grepl("^https:\\/\\/orcid\\.org", subject)) {
    stop("Argument 'subjects' cannot contain 'https:', use 'http:' instead.")
  }

  if (!all(permissions %in% c("read", "write", "changePermission"))) {
    stop("Argument 'permissions' must be one or more of: 'read', 'write', 'changePermission'")
  }

  # Store the results of each attempted update
  results <- c()

  # Set rights and access for each PID
  for (pid in pids) {
    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    }, warning = function(w) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      w
    }, error = function(e) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      message(e)
      e
    })

    if (!inherits(sysmeta, "SystemMetadata")) {
      stop("Failed to get System Metadata.")
    }

    # Track whether we have changed the record to avoid an unnecessary update call
    changed <- FALSE

    # Set rights holder if needed
    if (subject != sysmeta@rightsHolder) {
      changed <- TRUE

      message(paste0("Setting rights holder to ", subject, "."))
      sysmeta@rightsHolder <- subject
    } else {
      message(paste0("Skipping setting rightsHolder as rightsHolder is already ", sysmeta@rightsHolder, ".\n"))
    }

    for (permission in permissions) {
      if (datapack::hasAccessRule(sysmeta, subject, permission)) {
        message(paste0("Skipping the addition of permission '", permission, "' for subject '", subject, "'\n"))
        next
      }

      changed <- TRUE

      message(paste0("Adding permission '", permission, "' for subject '", subject, "'\n"))
      sysmeta <- datapack::addAccessRule(sysmeta, subject, permission)
    }

    if (changed == TRUE) {
      message(paste0("Updating System Metadata for ", pid, "."))

      update_response <- tryCatch({
        dataone::updateSystemMetadata(mn, pid, sysmeta)
      },
      error = function(e) {
        message(paste0("Failed to update System Metadata for PID '", pid, "'.\n"))
        message(e)
        e
      })

      if (inherits(update_response, "error")) {
        stop("Failed update.")
      }
    } else {
      message(paste0("No changes needed for ", pid, "."))
    }

    # Save the result for this PID
    results[pid] <- changed
  }

  results
}


#' Set public read access for an object
#'
#' Set public read access for an object.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to set public read access for.
#'
#' @return (logical) Whether an update was needed.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#'    "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' set_public_read(mn, pids)
#'}
set_public_read <- function(mn, pids) {
  set_access(mn, pids, "public", "read")
}


#' Remove public read access for an object
#'
#' Remove public read access for an object.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to remove public read access for.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#' "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' remove_public_read(mn, pids)
#'}
remove_public_read <- function(mn, pids) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }


  # Store the results of each attempted update
  results <- c()

  # Remove public read access for each PID
  for (pid in pids) {
    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    }, warning = function(w) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      w
    }, error = function(e) {
      message(paste0("Failed to get System Metadata for PID '", pid, "'\non MN '", mn@endpoint, "'.\n"))
      message(e)
      e
    })

    if (!inherits(sysmeta, "SystemMetadata")) {
      stop("Failed to get System Metadata.")
    }

    # Track whether we have changed the record to avoid an uncessary update call
    changed <- FALSE

    if (!datapack::hasAccessRule(sysmeta, "public", "read")) {
      message(paste0("Skipping setting public read because ", pid, " is not public."))
      next
    }

    changed <- TRUE

    message(paste0("Removing public read access on ", pid, "."))
    sysmeta@accessPolicy <- sysmeta@accessPolicy[!(grepl("public", sysmeta@accessPolicy$subject) & grepl("read", sysmeta@accessPolicy$permission)), ]

    # Update the sysmeta
    update_response <- tryCatch({
      dataone::updateSystemMetadata(mn, pid, sysmeta)
    },
    error = function(e) {
      message(paste0("Failed to update System Metadata for PID '", pid, "'.\n"))
      message(e)
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


#' Check whether an object has public read access
#'
#' Check whether objects have public read access.
#' No token needs to be set to use this function.
#'
#' @param mn (MNode) The Member Node.
#' @param pids (character) The PIDs of the objects to check for public read access.
#' @param use.names (logical) If `TRUE`, PIDs will
#'   be used as names for the result unless PIDs have names already, in which case
#'   those names will be used for the result.
#'
#' @return (logical) Whether an object has public read access.
#'
#' @importFrom httr content
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#'     "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' is_public_read(mn, pids)
#'}
is_public_read <- function(mn, pids, use.names = TRUE) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!is.logical(use.names)) {
    stop(paste0("Argument 'use.names' must be logical class, but was a ", class(use.names), " instead."))
  }

  vapply(pids, USE.NAMES = use.names, FUN.VALUE = logical(1), FUN = function(pid) {

    url       <-  paste(mn@endpoint, "meta", utils::URLencode(pid, reserved = TRUE), sep = "/")
    response  <-  dataone:::auth_get(url, node = mn)

    if (response$status_code != "200") {
      error_desc <- dataone:::getErrorDescription(response)
      if (grepl("READ not allowed", error_desc, ignore.case = TRUE)) {
        return(FALSE)
      } else {
        stop(error_desc)
      }
    }

    sysmeta <- datapack:::SystemMetadata(XML::xmlRoot(suppressMessages(XML::xmlParse((httr::content(response, as = "text"))))))
    return(datapack::hasAccessRule(sysmeta, "public", "read"))
  })
}
