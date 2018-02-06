#' access.R
#'
#' High-level utility functions for getting and setting access rules for DataONE
#' objects.


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
           all(nchar(pids) > 0))){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subject),
           nchar(subject) > 0)){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (grepl("^https:\\/\\/orcid\\.org", subject)) {
    stop("Argument 'subjects' cannot contain 'https:', use 'http:' instead.")
  }


  result <- vector(mode = "logical", length = length(pids))

  for (i in seq_along(pids)) {
    pid <- pids[i]

    # Get System Metadata
    sysmeta <- dataone::getSystemMetadata(mn, pid)

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
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#'    "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' set_access(mn, pids, subjects = "http://orcid.org/0000-000X-XXXX-XXXX",
#'    permissions = c("read", "write", "changePermission"))
#'}
set_access <- function(mn, pids, subjects, permissions=c("read", "write", "changePermission")) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subjects),
           all(nchar(subjects)) > 0)){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
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


#' Set public access on a set of objects.
#'
#' @param mn (MNode)
#' @param pids (character) A vector of PIDs to set public access on
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

#' Remove public access on a set of objects.
#'
#' @param mn (MNode)
#' @param pids (character) A vector of PIDs to set public access on
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
           all(nchar(pids) > 0))){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }


  # Store the results of each attempted update
  results <- c()

  # Remove public access for each PID
  for (pid in pids) {
    sysmeta <- tryCatch({
      dataone::getSystemMetadata(mn, pid)
    },
    error = function(e) {
      message(paste0("Failed to get system metadata for PID '", pid, "' on MN '", mn@endpoint, "'.\n"))
      message(e)
      e
    })

    if (inherits(sysmeta, "error")) {
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
    sysmeta@accessPolicy <- sysmeta@accessPolicy[!(grepl("public", sysmeta@accessPolicy$subject) & grepl("read", sysmeta@accessPolicy$permission)),]

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


#' Set the given subject as the rightsHolder and subject with write and
#' changePermission access for the given PID.
#'
#' This function only updates the existing System Metadata if a change is
#' needed.
#'
#' @param mn (MNode) The Member Node to send the query.
#' @param pids (character) The PID(s) to set the access rule for.
#' @param subject (character)The subject of the rule(s).
#' @param permissions (character) The permissions for the rule. Defaults to
#' read, write, and changePermission.
#'
#' @return Whether an update was needed.
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
set_rights_and_access <- function(mn, pids, subject, permissions=c("read", "write", "changePermission")) {
  if (!is(mn, "MNode")) {
    stop(paste0("Argument 'mn' is not an MNode but was a ", class(mn), " instead."))
  }

  if (!all(is.character(pids),
           all(nchar(pids) > 0))){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  if (!all(is.character(subject),
           nchar(subject) > 0)){
    stop("Argument 'pids' must be character class with non-zero number of characters.")
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
    },
    error = function(e) {
      message(paste0("Failed to get system metadata for PID '", pid, "' on MN '", mn@endpoint, "'.\n"))
      message(e)
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
