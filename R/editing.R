#' editing.R
#'
#' High-level functions for managing content.
#'
#'

#' Publish an object on a member node
#'
#' Use sensible defaults to publish an object on a member node. If identifier is provided,
#' use it, otherwise generate a UUID.  If clone_id is provided, then retrieve the
#' system metadata for that identifier and use it to provide rightsHolder, accessPolicy,
#' and replicationPolicy metadata. Note that this function only uploads the object to
#' the Member Node, and does not add it to a data package, which can be done separately.
#' @param mn The Member Node to publish the object to (MNode)
#' @param filepath the path to the file to be published
#' @param formatId the dataone format identifier
#' @param identifier optional string to be used as an identifer
#' @param clone_id optional string identifier or an object whose sysmeta should be cloned
#' @import dataone
#' @import datapack
#' @export
publish_object <- function(mn,
                           filepath,
                           format_id,
                           identifier=as.character(NA),
                           clone_id=as.character(NA)) {

  if (missing(mn) || missing(filepath) || missing(format_id)) {
    stop("mn, filepath, and format_id are required parameters.")
  }

  # Set up some variables for use later on
  ########################################
  me <- get_token_subject()

  # Get the clone_id sysmeta to use for the rightsHolder and accessPolicy, and replicationPolicy
  if (!is.na(clone_id)) {
    log_message(paste0("Cloning System Metadata for new object from ", clone_id, "."))
    clone_sysmeta <- dataone::getSystemMetadata(mn, clone_id)
  }

  # Generate an identifier if not provided
  if (is.na(identifier)) {
    identifier <- new_uuid()
  }

  sysmeta <- new("SystemMetadata",
                 identifier = identifier,
                 formatId = format_id,
                 size = file.size(filepath),
                 checksum = digest::digest(filepath, algo="sha256"),
                 checksumAlgorithm = "SHA256",
                 submitter = me,
                 rightsHolder = me)

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
  sysmeta@fileName <- basename(filepath)

  create_response <- dataone::createObject(mn,
                                           pid = identifier,
                                           file = filepath,
                                           sysmeta = sysmeta)

  new_pid <- get_identifier(create_response)
  log_message(paste0("Published file with identifier: ", new_pid))
  return(new_pid)
}


#' Publish an updated data package.
#'
#' The metadata_old_pid and resmap_old_pid provide the identifier of an EML metadata
#' document and associated resource map, and the data_old_pids vector provides a list
#' of PIDs of data objects in the package.  Update the metadata file and resource map
#' by generating a new identifier (a DOI if use_doi is TRUE) and updating the Member
#' Node with a public version of the object.  If metadata_file is not missing, it
#' should be an edited version of the metadata to be used to update the original. If
#' parent_resmap_pid is not missing, it indicates the PID of a parent package that
#' should be updated as well, using the parent_medata_pid, parent_data_pids, and
#' parent_child_pids as members of the updated package. In all cases, the objects
#' are made publicly readable.
#' @param mn The Member Node to update the object on (MNode)
#' @param metadata_old_pid The PID of the EML metadata document to be updated
#' @param resmap_old_pid The PID of the resource map for the package
#' @param data_old_pids a vector of PIDs of data objects in the package
#' @param metadata_file optional filename  of a replacement EML file for the metadata
#' @param use_doi boolean indicating if a DOI should be used for the metadata
#' @param parent_resmap_pid optional PID of a parent package to be updated
#' @param parent_metadata_pid optional identifier for the metadata document of the parent package
#' @param parent_data_pids optional vector of identifiers of data in the parent package
#' @param parent_child_pids optional vector of identifiers of child packages in the parent package
#' @import dataone
#' @import datapack
#' @import EML
#' @export
publish_update <- function(mn,
                           metadata_old_pid,
                           resmap_old_pid,
                           data_old_pids,
                           metadata_file_path=as.character(NA),
                           use_doi=FALSE,
                           parent_resmap_pid=as.character(NA),
                           parent_metadata_pid=as.character(NA),
                           parent_data_pids=as.character(NA),
                           parent_child_pids=as.character(NA)) {

  # Set up some variables for use later on
  ########################################
  me <- get_token_subject()

  # Get some things from the node
  ###############################
  if (is.na(metadata_file_path)) {
    # Get the metadata doc
    log_message("Getting metadata from the MN.")
    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, metadata_old_pid)), asText = TRUE)
  } else {
    # Alternatively, read an edited metadata file from disk if provided
    if (!file.exists(metadata_file_path)) {
      stop(paste0("Metadata doesn't exist: ", metadata_file_path))
    }

    log_message(paste0("Getting metadata from the path: ", metadata_file_path, "."))
    eml <- EML::read_eml(metadata_file_path)
  }

  # get the metadata sysmeta from the node
  metadata_sysmeta <- dataone::getSystemMetadata(mn, metadata_old_pid)
  #eml_acl <- sysmeta_orig@accessPolicy
  # TODO: error check: md and sm existence

  # get the resource_map (not used for now, could be used to get the list of data pids)
  # resmap <- rawToChar(dataone::getObject(mn, resmap_old_pid))
  # resmap_sysmeta <- dataone::getSystemMetadata(mn, resmap_old_pid)
  # TODO: error check: resmap existence, and ensure we don't fail hard

  # Get the list of data files from the resource map
  # TODO: parse these from the resource map, rather than taking them as input
  # TODO: data_old_pids <- as.vector(c("pid1", "pid2", "pid3"))

  log_message("Downloaded EML and sysmeta...")

  # Generate PIDs for our updated objects
  ##################################
  if (use_doi) {
    log_message("Minting a new DOI")
    metadata_updated_pid <- dataone::generateIdentifier(mn, scheme = "DOI")
    log_message(paste0("Minted a new DOI of ", metadata_updated_pid))
  } else {
    metadata_updated_pid <- new_uuid()
  }

  # Generate a resource map PID from the new metadata PID
  resmap_updated_pid <- paste0("resource_map_",  metadata_updated_pid)

  # Update the metadata object
  ############################

  eml@packageId <- new("xml_attribute", metadata_updated_pid)
  # TODO: remove any EML access sections, as these are handled in sysmeta
  # slot(eml, "access") <- new("access)
  # slot(eml@dataset@otherEntity[[1]]@physical[[1]]@distribution[[1]], "access") <- new("access")
  eml_file <- paste0(tempdir(), "/metadata.xml")
  EML::write_eml(eml, eml_file)

  metadata_updated_sysmeta <- new("SystemMetadata",
                                  identifier = metadata_updated_pid,
                                  formatId = "eml://ecoinformatics.org/eml-2.1.1",
                                  size = file.size(eml_file),
                                  checksum = digest::digest(eml_file, algo = "sha256"),
                                  checksumAlgorithm = "SHA256",
                                  submitter = me,
                                  rightsHolder = metadata_sysmeta@rightsHolder,
                                  obsoletes = metadata_old_pid)

  metadata_updated_sysmeta@originMemberNode <- mn@identifier
  metadata_updated_sysmeta@authoritativeMemberNode <- mn@identifier
  metadata_updated_sysmeta@accessPolicy <- metadata_sysmeta@accessPolicy
  metadata_updated_sysmeta <- datapack::addAccessRule(metadata_updated_sysmeta, "public", "read")

  update_rights_holder(mn, metadata_sysmeta@identifier, me)

  dataone::updateObject(mn,
                        pid = metadata_old_pid,
                        newpid = metadata_updated_pid,
                        file = eml_file,
                        sysmeta = metadata_updated_sysmeta)

  update_rights_holder(mn, metadata_sysmeta@identifier, metadata_sysmeta@rightsHolder)

  log_message("Updated metadata document.")

  # Update the resource map
  #########################
  update_resource_map(mn,
                      old_resource_map_pid = resmap_old_pid,
                      new_resource_map_pid = resmap_updated_pid,
                      metadata_pid = metadata_updated_pid,
                      data_pids = data_old_pids,
                      public = TRUE)

  message("Updated resource map")

  # Update the parent resource map to add the new package
  #######################################################
  if (!is.na(parent_resmap_pid)) {
    if (is.na(parent_metadata_pid) ||
        is.na(parent_child_pids)) {
      stop("Missing required parameters to update parent package.")
    }

    update_resource_map(mn,
                        old_resource_map_pid = parent_resmap_pid,
                        metadata_pid = parent_metadata_pid,
                        data_pids = parent_data_pids,
                        child_pids = parent_child_pids,
                        public = TRUE)
  }
}


#' Change the rightsHolder field for a given PID.
#'
#' Update the rights holder to the provided subject for the object identified in
#' the provided system metadata document on the given Member Node.
#'
#' @param mn the MNode instance to be changed (MNode)
#' @param pid the identifier for the object to be changed (character)
#' @param subject the identifier of the new rightsHolder, often an ORCID or DN (character)
#' @import dataone
#' @import datapack
#' @export
update_rights_holder <- function(mn, pid, subject) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pid),
            nchar(pid) > 0)
  stopifnot(is.character(subject),
            nchar(subject) > 0)

  log_message(paste0("Updating rightsHolder for PID ", pid, " to ", subject, "."))

  # Get System Metadata
  log_message(paste0("Getting System Metadata for PID ", pid, "..."))
  sysmeta <- dataone::getSystemMetadata(mn, pid)

  # Change rightsHolder (if needed)
  if (sysmeta@rightsHolder == subject) {
    log_message(paste0("No change to System Metadata needed because the rightsHolder is already ", subject, "."))
    return(TRUE)
  } else {
    sysmeta@rightsHolder <- subject
  }

  # Update System Metadata
  log_message(paste0("Updating System Metadata for PID ", pid, "..."))
  dataone::updateSystemMetadata(mn,
                                pid = pid,
                                sysmeta = sysmeta)

  log_message(paste0("Successfully updated System Metadata for PID ", pid, "..."))

  return(TRUE)
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
update_resource_map <- function(mn,
                                old_resource_map_pid,
                                new_resource_map_pid=NA,
                                metadata_pid,
                                data_pids,
                                child_pids=c(),
                                public=FALSE) {

  # Check arguments
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(old_resource_map_pid),
            nchar(old_resource_map_pid) > 0)
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)
  stopifnot(all(sapply(data_pids, is.character)))
  stopifnot(all(sapply(child_pids, is.character)))
  stopifnot(is_resource_map(mn, old_resource_map_pid))

  # Get the current rightsHolder
  sysmeta <- dataone::getSystemMetadata(mn, old_resource_map_pid)
  previous_rights_holder <- sysmeta@rightsHolder

  # Set the rightsHolder to us temporarily
  me <- get_token_subject()
  update_rights_holder(mn, old_resource_map_pid, me)

  # Create the replacement resource map
  if (is.na(new_resource_map_pid)) {
    new_resource_map_pid <- paste0("resource_map_", new_uuid())
  }

  new_rm_path <- generate_resource_map(metadata_pid = metadata_pid,
                                       data_pids = data_pids,
                                       child_pids = child_pids,
                                       resource_map_pid = new_resource_map_pid)

  rm(sysmeta)

  log_message(paste("Getting updated copy of System Metadata for ", old_resource_map_pid))
  sysmeta <- dataone::getSystemMetadata(mn, old_resource_map_pid)

  new_rm_sysmeta <- sysmeta
  new_rm_sysmeta@identifier <- new_resource_map_pid
  new_rm_sysmeta@size <- file.size(new_rm_path)
  new_rm_sysmeta@checksum <- digest::digest(new_rm_path, algo = "sha256")
  new_rm_sysmeta@checksumAlgorithm <- "SHA256"
  new_rm_sysmeta@rightsHolder <- previous_rights_holder
  new_rm_sysmeta@obsoletes <- old_resource_map_pid
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

  if (file.exists(new_rm_path)) {
    file.remove(new_rm_path)
  }

  new_pid <- get_identifier(resmap_update_response)
  log_message(paste0("Successfully updated ", old_resource_map_pid, " with ", new_resource_map_pid, "."))

  return(new_pid)
}
