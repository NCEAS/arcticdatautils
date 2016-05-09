#' editing.R
#'
#' High-level functions for managing content.
#'
#'


#' Replace an existing Resource Map with a new one.
#'
#' This function is intended to be used to add a few new child packages to a
#' parent package. For exmaple, if you have:
#'
#' Parent
#'   A
#'   B
#'
#'  and want to add C as a sibling package to A and B, e.g.
#'
#' Parent
#'   A
#'   B
#'   C
#'
#' you could use this function.
#'
#' Note: This function currently replaces the rightsHolder on the Resource Map
#' temporarily to allow updating but sets it back to the rightsHolder that was
#' in place before the update.
#'
#' @param mn
#' @param resource_map_pid
#' @param metadata_pid
#' @param data_pids
#' @param child_pids


update_children <- function(mn,
                            resource_map_pid,
                            metadata_pid,
                            data_pids,
                            child_pids) {

  # Check arguments
  stopifnot(is.character(resource_map_pid),
            nchar(resource_map_pid) > 0)

  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  stopifnot(all(sapply(data_pids, is.character)))

  stopifnot(all(sapply(child_pids, is.character)))

  # Verify the first param is a resouce map
  old_rm_sm <- tryCatch({
    dataone::getSystemMetadatda(mn, resource_map_pid)
  },
  error = function(e) {
    log_message(paste0("Failed to get System Metadata for resource_map_pid of ", resource_map_pid, "."))
    log_message(e)
    e
  })

  if (inherits(old_rm_sm, "error")) {
    return(FALSE)
  }

  if (old_rm_sm@formatId != "http://www.openarchives.org/ore/terms") {
    log_message(paste0("Format ID of resource map pid ", resource_map_pid, " was not http://www.openarchives.org/ore/terms but was ", old_rm_sm@formatId, ". Ensure the argument 'resource_map_pid' is a Resource Map PID."))
    return(FALSE)
  }

  # Get the existing rightsHolder and change it to us
  previous_rights_holder <- NA

  if (old_rm_sm@rightsHolder != "CN=arctic-data-admins,DC=dataone,DC=org") {
    log_message("Changing rightsHolder to CN=arctic-data-admins,DC=dataone,DC=org")

    previous_rights_holder <- old_rm_sm@rightsHolder
    old_rm_sm@rightsHolder <- "CN=arctic-data-admins,DC=dataone,DC=org"

    response <- tryCatch({
      dataone::updateSystemMetadata(mn,
                                    pid = resource_map_pid,
                                    sysmeta = old_rm_sm)
    },
    error = function(e) {
      log_message(paste0("Failed to update rightsHolder on ", resource_map_pid, " from ", previous_rights_holder, " to CN=arctic-data-admins,DC=dataone,DC=org."))
      log_message(e)
    })

    if (inherits(response, "error")) {
      return(FALSE)
    }
  } else {
    log_message("rightsHolder was already set to CN=arctic-data-admins,DC=dataone,DC=org so there is no need to update the System Metadata prior to updating.")
  }

  # Create the replacement resource map
  new_rm_pid <- paste0("resource_map_urn:uuid:", uuid::UUIDgenerate())
  new_rm_path <- generate_resource_map(metadata_pid = metadata_pid,
                                       data_pids = data_pids,
                                       child_pids = child_pids,
                                       resource_map_pid = new_rm_pid)


  new_rm_sysmeta <- old_rm_sm
  old_rm_sm@identifier <- new_rm_pid
  old_rm_sm@size <- file.size(new_rm_path)
  old_rm_sm@checksum <- digest::digest(new_rm_path, algo = "sha256")
  old_rm_sm@checksumAlgorithm <- "SHA256"
  old_rm_sm@rightsHolder <- previous_rights_holder

  # Update it
  resmap_update_response <- tryCatch({
    dataone::updateObject(mn,
                          pid = resource_map_pid,
                          newpid = new_rm_pid,
                          sysmeta = new_rm_sysmeta,
                          file = new_rm_path
    )
  },
  error = function(e) {
    log_message("There was an error while updating the resource map object.")
    log_message(e)
  })

  file.remove(new_rm_path)

  if (inherits(resmap_update_response, "error")) {
    return(FALSE)
  }

  return(TRUE)
}
