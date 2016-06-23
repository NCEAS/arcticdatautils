#' editing.R
#'
#' High-level functions for managing content.


#' Publish an object on a member node
#'
#' Use sensible defaults to publish an object on a member node. If identifier is provided,
#' use it, otherwise generate a UUID.  If clone_id is provided, then retrieve the
#' system metadata for that identifier and use it to provide rightsHolder, accessPolicy,
#' and replicationPolicy metadata. Note that this function only uploads the object to
#' the Member Node, and does not add it to a data package, which can be done separately.
#'
#' @param mn (MNode) The Member Node to publish the object to.
#' @param path the path to the file to be published
#' @param format_id (character) Optional. The format ID to set for the object.
#' @param pid (character) Optional. The PID to use with the object.
#' @param sid (character) Optional. The SID to use with the new object.
#' @param clone_pid (character) PID of objet to clone System Metadata from
#'
#' @import dataone
#' @import datapack
#'
#' @export
publish_object <- function(mn,
                           path,
                           format_id=NULL,
                           pid=NULL,
                           sid=NULL,
                           clone_pid=NULL,
                           public=TRUE) {

  stopifnot(class(mn) == "MNode")
  stopifnot(file.exists(path))

  # Decide the format_id
  if (is.null(format_id)) {
    format_id <- guess_format_id(path)
    log_message(paste0("Guessed format ID of ", format_id, "."))
  }

  # Set up some variables for use later on
  ########################################
  me <- get_token_subject()

  # Get the clone_pid sysmeta to use for the rightsHolder and accessPolicy, and replicationPolicy
  if (!is.null(clone_pid)) {
    log_message(paste0("Cloning System Metadata for new object from ", clone_pid, "."))
    clone_sysmeta <- dataone::getSystemMetadata(mn, clone_pid)
  }

  # Generate an identifier if not provided
  if (is.null(pid)) {
    pid <- new_uuid()
  }

  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = format_id,
                 size = file.size(path),
                 checksum = digest::digest(path, algo="sha256"),
                 checksumAlgorithm = "SHA256",
                 submitter = me,
                 rightsHolder = me,
                 fileName = basename(path))

  if (!is.null(sid)) {
    log_message(paste0("Setting SID to '", sid, "'."))
    sysmeta@seriesId <- sid
  }

  sysmeta@originMemberNode <- mn@identifier
  sysmeta@authoritativeMemberNode <- mn@identifier

  if (exists("clone_sysmeta")) {
    sysmeta@rightsHolder <- clone_sysmeta@rightsHolder
    sysmeta@accessPolicy <- clone_sysmeta@accessPolicy
    sysmeta@replicationAllowed <- clone_sysmeta@replicationAllowed
    sysmeta@numberReplicas <- clone_sysmeta@numberReplicas
    sysmeta@preferredNodes <- clone_sysmeta@preferredNodes
    sysmeta@blockedNodes <- clone_sysmeta@blockedNodes
  }

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")
  sysmeta@fileName <- basename(path)

  create_response <- dataone::createObject(mn,
                                           pid = pid,
                                           file = path,
                                           sysmeta = sysmeta)

  log_message(paste0("Published file with identifier: ", create_response))

  return(create_response)
}

#' Update an object with a new file.
#'
#' This is a convenience wrapper around `dataone::updateObject` which copies in
#' fields from the old object's System Metadata such as the rightsHolder and
#' accessPolicy and updates only what needs to be changed.
#'
#' @param mn (MNode) The Member Node to update the object on.
#' @param pid (character) The PID of the object to update.
#' @param path (character) The full path to the file to update with.
#' @param format_id (character) Optional. Specify the format ID manually. Use this when `guess_format_id` wont' guess your format correctly.
#'
#' @return (character) The PID of the updated object.
#' @export
#'
#' @examples
update_object <- function(mn, pid, path, format_id=NULL) {
  stopifnot(is(mn, "MNode"))
  stopifnot(object_exists(mn, pid))
  stopifnot(file.exists(path))

  # Decide the format_id
  if (is.null(format_id)) {
    format_id <- guess_format_id(path)
    log_message(paste0("Guessed format ID of ", format_id, "."))
  }

  log_message(paste0("Updating object ", pid, " with the file at ", path, "."))

  # Generate a PID
  new_pid <- paste0("urn:uuid:", uuid::UUIDgenerate())

  # Grab and modify the old object's sysmeta
  sysmeta <- dataone::getSystemMetadata(mn, pid)
  sysmeta@identifier <- new_pid
  sysmeta@formatId <- format_id
  sysmeta@size <- file.size(path)
  sysmeta@checksum <- digest::digest(path, algo = "sha256")
  sysmeta@checksumAlgorithm <- "SHA256"
  slot(sysmeta, "obsoletes", check = FALSE) <- NA
  slot(sysmeta, "obsoletedBy", check = FALSE) <- NA
  sysmeta@fileName <- basename(path)

  # Make the update
  dataone::updateObject(mn,
                        pid = pid,
                        file = path,
                        newpid = new_pid,
                        sysmeta = sysmeta)
}


#' Publish an updated data package.
#'
#' This function can be used for a variety of tasks:
#'
#' \itemize{
#'   \item Publish an existing package with a DOI
#'   \item Update a package with new data objects
#'   \item Update a package with new metadata
#' }
#'
#' The metadata_pid and resource_map_pid provide the identifier of an EML metadata
#' document and associated resource map, and the data_pids vector provides a list
#' of PIDs of data objects in the package.  Update the metadata file and resource map
#' by generating a new identifier (a DOI if use_doi is TRUE) and updating the Member
#' Node with a public version of the object.  If metadata_file is not missing, it
#' should be an edited version of the metadata to be used to update the original. If
#' parent_resmap_pid is not missing, it indicates the PID of a parent package that
#' should be updated as well, using the parent_medata_pid, parent_data_pids, and
#' parent_child_pids as members of the updated package. In all cases, the objects
#' are made publicly readable.
#'
#' @param mn (MNode) The Member Node to update the object on.
#' @param metadata_pid (character) The PID of the EML metadata document to be updated.
#' @param resource_map_pid (character)  The PID of the resource map for the package.
#' @param data_pids (character)  PID(s) of data objects that will go in the updated package.
#' @param identifier (character) Manually specify the identifier for the new metadata object.
#' @param use_doi (logical) Generate and use a DOI as the identifier for the updated metadata object.
#' @param parent_resmap_pid  (character)  Optional. PID of a parent package to be updated.
#' @param parent_metadata_pid (character)  Optional. identifier for the metadata document of the parent package.
#' @param parent_data_pids(character)  Optional. Identifier(s) of data in the parent package.
#' @param parent_child_pids (character) Optional. Resource map identifier(s) of child packages in the parent package.
#' @param child_pids (character) Optional. Child packages resource map PIDs.
#' @param metadata_path (character) Optional. Path to a metadata file to update with. If this is not set, the existing metadata document will be used.
#' @param public (logical) Optional. Make the update public.
#' This applies to the new metadata PID and its resource map and data object.
#' access policies are not affected.
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as aruments exist on the MN before continuing. This speeds up the function, especially when `data_pids` has many elements.
#'
#' @import dataone
#' @import datapack
#' @import EML
#'
#' @export
publish_update <- function(mn,
                           metadata_pid,
                           resource_map_pid,
                           data_pids=NULL,
                           child_pids=NULL,
                           metadata_path=NULL,
                           identifier=NULL,
                           use_doi=FALSE,
                           parent_resmap_pid=NULL,
                           parent_metadata_pid=NULL,
                           parent_data_pids=NULL,
                           parent_child_pids=NULL,
                           public=TRUE,
                           check_first=TRUE) {

  # Do a simple sanity check on the PIDs passed in
  all_pids <- c(metadata_pid, resource_map_pid, data_pids, child_pids,
                identifier, parent_resmap_pid, parent_metadata_pid,
                parent_data_pids, parent_child_pids)
  duped <- duplicated(all_pids)

  if (any(duped)) {
    stop(paste("The PIDs used in the arguments to this function should all be unique.\nOne or more dupes was found:\n\n",
               paste(all_pids[duped], collapse=", ")))
  }

  rm(all_pids)
  rm(duped)

  if (check_first) {
    # Check that objects exist
    stopifnot(object_exists(mn, metadata_pid))
    stopifnot(object_exists(mn, resource_map_pid))
    if (!is.null(data_pids))
      stopifnot(object_exists(mn, data_pids))
    if (!is.null(child_pids))
      stopifnot(object_exists(mn, child_pids))
    if (!is.null(parent_resmap_pid))
      stopifnot(object_exists(mn, parent_resmap_pid))
    if (!is.null(parent_metadata_pid))
      stopifnot(object_exists(mn, parent_metadata_pid))
    if (!is.null(parent_data_pids))
      stopifnot(object_exists(mn, parent_data_pids))
    if (!is.null(parent_child_pids))
      stopifnot(object_exists(mn, parent_child_pids))
  }

  # Prepare the response object
  response <- list()

  # Set up some variables for use later on
  me <- get_token_subject()

  # Get some things from the node
  if (is.null(metadata_path)) {
    # Get the metadata doc
    log_message("Getting metadata from the MN.")
    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, metadata_pid)), asText = TRUE)
  } else {
    # Alternatively, read an edited metadata file from disk if provided
    if (!file.exists(metadata_path)) {
      stop(paste0("Metadata doesn't exist: ", metadata_path))
    }

    log_message(paste0("Getting metadata from the path: ", metadata_path, "."))
    eml <- EML::read_eml(metadata_path)
  }

  # get the metadata sysmeta from the node
  metadata_sysmeta <- dataone::getSystemMetadata(mn, metadata_pid)
  #eml_acl <- sysmeta_orig@accessPolicy
  # TODO: error check: md and sm existence

  # get the resource_map (not used for now, could be used to get the list of data pids)
  # resmap <- rawToChar(dataone::getObject(mn, resource_map_pid))
  # resmap_sysmeta <- dataone::getSystemMetadata(mn, resource_map_pid)
  # TODO: error check: resmap existence, and ensure we don't fail hard

  # Get the list of data files from the resource map
  # TODO: parse these from the resource map, rather than taking them as input
  # TODO: data_pids <- as.vector(c("pid1", "pid2", "pid3"))

  log_message("Downloaded EML and sysmeta...")

  # Generate PIDs for our updated objects
  if (is.null(identifier)) {
    if (use_doi) {
      log_message("Minting a new DOI")
      metadata_updated_pid <- dataone::generateIdentifier(mn, scheme = "DOI")
      log_message(paste0("Minted a new DOI of ", metadata_updated_pid, "."))
    } else {
      metadata_updated_pid <- new_uuid()
      log_message(paste0("Using generated UUID PID of ", metadata_updated_pid, "."))
    }
  } else {
    log_message(paste0("Using manually-specified identifier of ", identifier, "."))
    metadata_updated_pid <- identifier
  }

  # Generate a resource map PID from the new metadata PID
  resmap_updated_pid <- paste0("resource_map_",  metadata_updated_pid)

  # Update the metadata object

  # Replace packageId
  eml@packageId <- new("xml_attribute", metadata_updated_pid)

  # Replace system if needed
  if (eml@system != "https://arcticdata.io") {
    eml@system <- new("xml_attribute", "https://arcticdata.io")
  }

  # Write out the document to disk. We do this in part because
  # add_other_entities takes a path to the doc.
  eml_path <- tempfile()
  EML::write_eml(eml, eml_path)

  # Add other entity fields (if appropriate)
  if (!is.null(data_pids)) {
    eml <- add_other_entities(mn, eml_path, data_pids)
  }

  # Create System Metadata for the updated EML file
  metadata_updated_sysmeta <- new("SystemMetadata",
                                  identifier = metadata_updated_pid,
                                  formatId = "eml://ecoinformatics.org/eml-2.1.1",
                                  size = file.size(eml_path),
                                  checksum = digest::digest(eml_path, algo = "sha256"),
                                  checksumAlgorithm = "SHA256",
                                  submitter = me,
                                  rightsHolder = metadata_sysmeta@rightsHolder,
                                  obsoletes = metadata_pid,
                                  fileName = "science_metadata.xml")

  # Set the SID if one existed on old metadata object
  if (!is.na(metadata_sysmeta@seriesId)) {
    metadata_updated_sysmeta@seriesId <- metadata_sysmeta@seriesId
  }

  metadata_updated_sysmeta@accessPolicy <- metadata_sysmeta@accessPolicy

  if (public) {
    metadata_updated_sysmeta <- datapack::addAccessRule(metadata_updated_sysmeta, "public", "read")
  }

  set_rights_holder(mn, metadata_pid, me)

  dataone::updateObject(mn,
                        pid = metadata_pid,
                        newpid = metadata_updated_pid,
                        file = eml_path,
                        sysmeta = metadata_updated_sysmeta)

  response[["metadata"]] <- metadata_updated_pid

  # Clean up temporary EML file
  file.remove(eml_path)

  # Set rightsHolder back
  set_rights_holder(mn,
                    metadata_pid,
                    metadata_sysmeta@rightsHolder)

  log_message("Updated metadata document.")

  # Update the resource map
  #########################
  response[["resource_map"]] <- update_resource_map(mn,
                                                    old_resource_map_pid = resource_map_pid,
                                                    new_resource_map_pid = resmap_updated_pid,
                                                    metadata_pid = metadata_updated_pid,
                                                    data_pids = data_pids,
                                                    child_pids = child_pids,
                                                    public = public,
                                                    check_first = check_first)

  set_rights_holder(mn, response[["resource_map"]], metadata_sysmeta@rightsHolder)

  log_message("Updated resource map")

  # Update the parent resource map to add the new package
  #######################################################
  if (!is.null(parent_resmap_pid)) {
    if (is.null(parent_metadata_pid)) {
      stop("Missing required parameters to update parent package.")
    }

    log_message("Updating parent resource map...")

    # Check to see if the just-updated package is in the list of
    # parent_child_pids, notify the user, and add it to the list
    if (!(resmap_updated_pid %in% parent_child_pids)) {
      log_message("Adding the new resource map to the list of child PIDs in the parent package.")
      parent_child_pids <- c(parent_child_pids, resmap_updated_pid)
    }

    response[["parent_resource_map"]] <- update_resource_map(mn,
                                                             old_resource_map_pid = parent_resmap_pid,
                                                             metadata_pid = parent_metadata_pid,
                                                             data_pids = parent_data_pids,
                                                             child_pids = parent_child_pids,
                                                             public = public,
                                                             check_first = check_first)

    set_rights_holder(mn, response[["parent_resource_map"]], metadata_sysmeta@rightsHolder)
  }

  return(response)
}


#' Create a resource map.
#'
#' Similar to but different than update_resource_map in that it uses a create
#' call instead of an update call.
#'
#' @param mn
#' @param metadata_pid
#' @param data_pids
#' @param child_pids
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as aruments exist on the MN before continuing. This speeds up the function, especially when `data_pids` has many elements.
#'
#' @return The created resource map's PID (character)
#' @export
#'
#' @examples
create_resource_map <- function(mn,
                                metadata_pid,
                                data_pids=NULL,
                                child_pids=NULL,
                                check_first=TRUE) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  if (check_first) {
    log_message("Checking all the object passed in as arguments exist before going on...")

    stopifnot(object_exists(mn, metadata_pid))
    if (!is.null(data_pids))
      stopifnot(all(object_exists(mn, data_pids)))
    if (!is.null(child_pids))
      stopifnot(all(object_exists(mn, child_pids)))

    pid <- paste0("resource_map_urn:uuid:", uuid::UUIDgenerate())
  }


  stopifnot(is.character(pid),
            nchar(pid) > 0)

  path <- generate_resource_map(metadata_pid = metadata_pid,
                                data_pids = data_pids,
                                child_pids = child_pids,
                                resource_map_pid = pid)

  stopifnot(file.exists(path))

  actual <- publish_object(mn,
                           path,
                           pid,
                           format_id = "http://www.openarchives.org/ore/terms")

  stopifnot(pid == actual)

  return(pid)
}


#' Update an existing Resource Map with a new one.
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
#' @param metadata_pid
#' @param data_pids
#' @param old_resource_map_pid
#' @param new_resource_map_pid
#' @param child_pids
#' @param public Whether or not to make the new resource map public read (logical)
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as aruments exist on the MN before continuing. This speeds up the function, especially when `data_pids` has many elements.
#'
#' @export
update_resource_map <- function(mn,
                                old_resource_map_pid,
                                new_resource_map_pid=NULL,
                                metadata_pid,
                                data_pids=NULL,
                                child_pids=NULL,
                                public=FALSE,
                                check_first=TRUE) {

  # Check arguments
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(old_resource_map_pid),
            nchar(old_resource_map_pid) > 0)
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  if (check_first) {
    log_message("Checking all the object passed in as arguments exist before going on...")

    stopifnot(object_exists(mn, old_resource_map_pid))
    stopifnot(object_exists(mn, metadata_pid))
    if (!is.null(data_pids))
      stopifnot(object_exists(mn, data_pids))
    if (!is.null(child_pids))
      stopifnot(object_exists(mn, child_pids))
    stopifnot(is_resource_map(mn, old_resource_map_pid))
  }


  # Get the current rightsHolder
  sysmeta <- dataone::getSystemMetadata(mn, old_resource_map_pid)
  stopifnot(class(sysmeta) == "SystemMetadata")

  previous_rights_holder <- sysmeta@rightsHolder

  # Set the rightsHolder to us temporarily
  me <- get_token_subject()
  set_rights_holder(mn, old_resource_map_pid, me)

  # Create the replacement resource map
  if (is.null(new_resource_map_pid)) {
    new_resource_map_pid <- paste0("resource_map_", new_uuid())
  }

  new_rm_path <- generate_resource_map(metadata_pid = metadata_pid,
                                       data_pids = data_pids,
                                       child_pids = child_pids,
                                       resource_map_pid = new_resource_map_pid)
  stopifnot(file.exists(new_rm_path))

  rm(sysmeta)

  log_message(paste("Getting updated copy of System Metadata for ", old_resource_map_pid))
  sysmeta <- dataone::getSystemMetadata(mn, old_resource_map_pid)
  stopifnot(class(sysmeta) == "SystemMetadata")

  new_rm_sysmeta <- sysmeta
  new_rm_sysmeta@identifier <- new_resource_map_pid
  new_rm_sysmeta@size <- file.size(new_rm_path)
  new_rm_sysmeta@checksum <- digest::digest(new_rm_path, algo = "sha256")
  new_rm_sysmeta@checksumAlgorithm <- "SHA256"
  new_rm_sysmeta@rightsHolder <- previous_rights_holder
  new_rm_sysmeta@obsoletes <- old_resource_map_pid
  slot(new_rm_sysmeta, "obsoletedBy", check = FALSE) <- NA

  new_rm_sysmeta <- add_admin_group_access(new_rm_sysmeta)

  if (public == TRUE) {
    new_rm_sysmeta <- datapack::addAccessRule(new_rm_sysmeta, "public", "read")
  }

  # Update it
  log_message(paste0("Updating resource map..."))
  resmap_update_response <- dataone::updateObject(mn,
                                                  pid = old_resource_map_pid,
                                                  newpid = new_resource_map_pid,
                                                  sysmeta = new_rm_sysmeta,
                                                  file = new_rm_path
  )

  # Set the rightsHolder back
  set_rights_holder(mn, old_resource_map_pid, previous_rights_holder)

  if (file.exists(new_rm_path)) {
    file.remove(new_rm_path)
  }

  log_message(paste0("Successfully updated ", old_resource_map_pid, " with ", new_resource_map_pid, "."))

  return(resmap_update_response)
}
