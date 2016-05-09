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
#' @param mn_uri base URI for the Member Node
#' @param filepath the path to the file to be published
#' @param formatId the dataone format identifier
#' @param identifier optional string to be used as an identifer
#' @param clone_id optional string identifier or an object whose sysmeta should be cloned
#' @import dataone
#' @import datapack
#' @export
publish_object <- function(mn_uri, filepath, formatId,
                           identifier=as.character(NA), clone_id=as.character(NA)) {

  if (missing(mn_uri) || missing(filepath) || missing(formatId)) {
    stop("mn_uri, filepath, and formatId are required parameters.")
  }

  # Set up some variables for use later on
  ########################################
  mn <- MNode(mn_uri)
  # TODO: check that the MN connection was successful

  info <- getTokenInfo(AuthenticationManager())
  me <- info[which(info$name=='dataone_test_token'),]$subject
  if (info[which(info$name=='dataone_test_token'),]$expired!=FALSE) {
    stop("Stopped processing becuase your token is expired. Please provide a new dataone_test_token.")
  }

  # TODO: factor these out as params
  group <- "CN=arctic-data-admins,DC=dataone,DC=org"

  # Get the clone_id sysmeta to use for the rightsHolder and accessPolicy, and replicationPolicy
  if (!is.na(clone_id)) {
    clone_sysmeta <- getSystemMetadata(mn, clone_id)
  }

  # Generate an identifier if not provided
  if (is.na(identifier)) {
    identifier <- paste0("urn:uuid:", uuid::UUIDgenerate())
  }

  sysmeta <- new("SystemMetadata",
                 identifier = identifier,
                 formatId = formatId,
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
  sysmeta <- addAccessRule(sysmeta, "public", "read")
  sysmeta <- addAccessRule(sysmeta, group, "read")
  sysmeta <- addAccessRule(sysmeta, group, "write")
  sysmeta <- addAccessRule(sysmeta, group, "changePermission")
  sysmeta@fileName <- basename(filepath)

  newId <- createObject(mn, pid = identifier, file = filepath, sysmeta = sysmeta)
  message(paste0("Published data file with identifier: ", identifier))
  return(newId)
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
#' @param mn_uri The base URI for the Member Node to be updated
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
publish_update <- function(mn_uri, metadata_old_pid, resmap_old_pid, data_old_pids,
                           metadata_file=as.character(NA), use_doi=FALSE,
                           parent_resmap_pid=as.character(NA),
                           parent_metadata_pid=as.character(NA),
                           parent_data_pids=as.character(NA),
                           parent_child_pids=as.character(NA)) {

  # Set up some variables for use later on
  ########################################
  mn <- MNode(mn_uri)
  # TODO: check that the MN connection was successful

  info <- getTokenInfo(AuthenticationManager())
  me <- info[which(info$name=='dataone_test_token'),]$subject
  if (info[which(info$name=='dataone_test_token'),]$expired!=FALSE) {
    stop("Stopped processing becuase your token is expired. Please provide a new dataone_test_token.")
  }

  # TODO: factor these out as params
  group <- "CN=arctic-data-admins,DC=dataone,DC=org"

  # Get some things from the node
  ###############################
  if (is.na(metadata_file)) {
    # Get the metadata doc
    eml <- read_eml(rawToChar(getObject(mn, metadata_old_pid)), asText=TRUE)
  } else {
    # Alternatively, read an edited metadata file from disk if provided
    metadata_updated_path <- metadata_file
    eml <- read_eml(metadata_file)
  }

  # get the metadata sysmeta from the node
  eml_sm <- getSystemMetadata(mn, metadata_old_pid)
  #eml_acl <- sysmeta_orig@accessPolicy
  # TODO: error check: md and sm existence

  # get the resource_map (not used for now, could be used to get the list of data pids)
  resmap <- rawToChar(getObject(mn, resmap_old_pid))
  resmap_sm <- getSystemMetadata(mn, resmap_old_pid)
  # TODO: error check: resmap existence, and ensure we don't fail hard

  # Get the list of data files from the resource map
  # TODO: parse these from the resource map, rather than taking them as input
  # TODO: data_old_pids <- as.vector(c("pid1", "pid2", "pid3"))

  message("Downloaded EML and sysmeta...")

  # Generate PIDs for our updated objects
  ##################################
  if (use_doi) {
    metadata_updated_pid <- generateIdentifier(mn, scheme="DOI")
  } else {
    metadata_updated_pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  }
  resmap_updated_pid <- paste0("resourceMap_",  metadata_updated_pid)

  # Update the metadata object
  ############################

  eml@packageId <- new("xml_attribute", metadata_updated_pid)
  # TODO: remove any EML access sections, as these are handled in sysmeta
  # slot(eml, "access") <- new("access)
  # slot(eml@dataset@otherEntity[[1]]@physical[[1]]@distribution[[1]], "access") <- new("access")
  eml_file <- paste0(tempdir(), "/metadata.xml")
  write_eml(eml, eml_file)
  metadata_updated_sysmeta <- new("SystemMetadata",
                                  identifier = metadata_updated_pid,
                                  formatId = "eml://ecoinformatics.org/eml-2.1.1",
                                  size = file.size(eml_file),
                                  checksum = digest::digest(eml_file, algo="sha256"),
                                  checksumAlgorithm = "SHA256",
                                  submitter = me,
                                  rightsHolder = eml_sm@rightsHolder,
                                  obsoletes = metadata_old_pid)  # Note that I'm setting this here and on the rest of the objects
  metadata_updated_sysmeta@originMemberNode <- mn@identifier
  metadata_updated_sysmeta@authoritativeMemberNode <- mn@identifier
  metadata_updated_sysmeta@accessPolicy <- eml_sm@accessPolicy
  metadata_updated_sysmeta <- addAccessRule(metadata_updated_sysmeta, "public", "read")

  update_RightsHolder(mn, me, eml_sm@identifier)
  updateObject(mn,
               pid = metadata_old_pid,
               newpid = metadata_updated_pid,
               file = eml_file,
               sysmeta = metadata_updated_sysmeta)
  update_RightsHolder(mn, eml_sm@rightsHolder, eml_sm@identifier)
  message("Updated metadata document...")

  # Update the resource map
  #########################
  # Use the arcticdata package's helper function to create a resource map

  resmap_updated_path <- generate_resource_map(metadata_pid = metadata_updated_pid,
                                               data_pids = c(data_old_pids),
                                               resource_map_pid = resmap_updated_pid)
  resmap_updated_sysmeta <- new("SystemMetadata",
                                identifier = resmap_updated_pid,
                                formatId = "http://www.openarchives.org/ore/terms",
                                size = file.size(resmap_updated_path),
                                checksum = digest::digest(resmap_updated_path, algo="sha256"),
                                checksumAlgorithm = "SHA256",
                                submitter = me,
                                rightsHolder = resmap_sm@rightsHolder,
                                obsoletes = resmap_old_pid)
  resmap_updated_sysmeta@originMemberNode <- mn@identifier
  resmap_updated_sysmeta@authoritativeMemberNode <- mn@identifier
  resmap_updated_sysmeta@accessPolicy <- resmap_sm@accessPolicy
  resmap_updated_sysmeta <- addAccessRule(resmap_updated_sysmeta, "public", "read")

  update_RightsHolder(mn, me, resmap_sm@identifier)
  updateObject(mn,
               pid = resmap_old_pid,
               newpid = resmap_updated_pid,
               file = resmap_updated_path,
               sysmeta = resmap_updated_sysmeta)
  update_RightsHolder(mn, resmap_sm@rightsHolder, resmap_sm@identifier)
  message("Updated resource map")

  # Update the parent resource map to add the new package
  #######################################################
  tested <- FALSE  # Flag this section as untested so it won't be run; TODO: test this and remove flag
  if (tested && !is.na(parent_resmap_pid)) {

    if(is.na(parent_metadata_pid) || is.na(parent_data_pids) || is.na(parent_child_pids)) {
      stop("Missing required parameters to update parent package.")
    }
    parent_resmap_updated_pid <- paste0("resourceMap_urn:uuid:", uuid::UUIDgenerate())
    parent_resmap_sm <- getSystemMetadata(mn, parent_resmap_pid)

    # TODO: Remove these hardcoded IDs and child pids
    parent_resmap_updated_path <- generate_resource_map(metadata_pid = parent_metadata_pid,
                                                        data_pids = parent_data_pids,
                                                        child_pids = parent_child_pids,
                                                        resource_map_pid = parent_resmap_updated_pid)

    parent_resmap_updated_sysmeta <- new("SystemMetadata",
                                         identifier = parent_resmap_updated_pid,
                                         formatId = "http://www.openarchives.org/ore/terms",
                                         size = file.size(parent_resmap_updated_path),
                                         checksum = digest::digest(parent_resmap_updated_path, algo="sha256"),
                                         checksumAlgorithm = "SHA256",
                                         submitter = me,
                                         rightsHolder = parent_resmap_sm@rightsHolder,
                                         obsoletes = parent_resmap_pid)
    parent_resmap_updated_sysmeta@originMemberNode <- mn@identifier
    parent_resmap_updated_sysmeta@authoritativeMemberNode <- mn@identifier
    parent_resmap_updated_sysmeta@accessPolicy <- eml_sm@accessPolicy
    parent_resmap_updated_sysmeta <- addAccessRule(parent_resmap_updated_sysmeta, "public", "read")

    update_RightsHolder(mn, me, parent_resmap_sm@identifier)
    updateObject(mn,
                 pid = parent_resmap_pid,
                 newpid = parent_resmap_updated_pid,
                 file = parent_resmap_updated_path,
                 sysmeta = parent_resmap_updated_sysmeta)
    update_RightsHolder(mn, parent_resmap_sm@rightsHolder, parent_resmap_sm@identifier)
  }
}

#' Change the rightsHolder field
#'
#' Update the rights holder to the provided subject for the object identified in the
#' provided system metadata document on the given Member Node.
#' @param mn the MNode instance to be changed
#' @param subject the identifier of the new rightsHolder, often an ORCID or DN
#' @param sysmeta_pid the identifier for the object to be changed
#' @import dataone
#' @import datapack
#' @export
update_RightsHolder <- function(mn, subject, sysmeta_pid) {
  # Copy the sysmeta and change the rightsHolder
  sysmeta_me <- getSystemMetadata(mn, resmap_old_pid)
  sysmeta_me@rightsHolder <- subject
  updateSystemMetadata(mn, sysmeta_me@identifier, sysmeta_me)
}

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
