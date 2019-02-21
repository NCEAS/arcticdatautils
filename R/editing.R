# High-level functions for managing content


#' Publish an object on a Member Node
#'
#' Use sensible defaults to publish an object on a Member Node. If identifier is provided,
#' use it, otherwise generate a UUID. If clone_id is provided, then retrieve the
#' system metadata for that identifier and use it to provide rightsHolder, accessPolicy,
#' and replicationPolicy metadata. Note that this function only uploads the object to
#' the Member Node, and does not add it to a data package, which can be done separately.
#'
#' @param mn (MNode) The Member Node to publish the object to.
#' @param path (character) The path to the file to be published.
#' @param format_id (character) Optional. The format ID to set for the object.
#'   When not set, [guess_format_id()] will be used to guess the format ID.
#'   Should be a \href{https://cn.dataone.org/cn/v2/formats}{DataONE format ID}.
#' @param pid (character) Optional. The PID to use with the object.
#' @param sid (character) Optional. The SID to use with the new object.
#' @param clone_pid (character) PID of object to clone System Metadata from.
#' @param public (logical) Whether object should be given public read access.
#'
#' @return pid (character) The PID of the published object.
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
#' my_path <- "/home/Documents/myfile.csv"
#' pid <- publish_object(mn, path = my_path, format_id = "text/csv", public = FALSE)
#'}
publish_object <- function(mn,
                           path,
                           format_id = NULL,
                           pid = NULL,
                           sid = NULL,
                           clone_pid = NULL,
                           public = TRUE) {

  stopifnot(is(mn, "MNode"))
  stopifnot(file.exists(path))

  # Decide the format_id
  if (is.null(format_id)) {
    format_id <- guess_format_id(path)

    if (format_id == "application/xml") {
      stop(call. = FALSE, "No format_id was specified and this appears to be an XML document. Please specify the format_id and run this call again.")
    }

    warning(paste0("No format_id was specified so a guess was made based upon the file extension: ", format_id, "."))
  }

  check_format(format_id)

  # Set up some variables for use later on
  ########################################
  me <- get_token_subject()

  # Get the clone_pid sysmeta to use for the rightsHolder and accessPolicy, and replicationPolicy
  if (!is.null(clone_pid)) {
    message(paste0("Cloning System Metadata for new object from ", clone_pid, "."))
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
                 checksum = digest::digest(path, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = basename(path))

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  if (!is.null(sid)) {
    message(paste0("Setting SID to '", sid, "'."))
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
  if (public == TRUE) {
    sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")
  }
  sysmeta@fileName <- basename(path)

  dataone::createObject(mn,
                        pid = pid,
                        file = path,
                        sysmeta = sysmeta)
}


#' Update an object with a new file
#'
#' This is a convenience wrapper around [dataone::updateObject()] which copies in
#' fields from the old object's System Metadata such as the rightsHolder and
#' accessPolicy and updates only what needs to be changed.
#'
#' @param mn (MNode) The Member Node to update the object on.
#' @param pid (character) The PID of the object to update.
#' @param path (character) The full path to the file to update with.
#' @param format_id (character) Optional. The format ID to set for the object.
#'   When not set, [guess_format_id()] will be used to guess the format ID.
#'   Should be a \href{https://cn.dataone.org/cn/v2/formats}{DataONE format ID}.
#' @param new_pid (character) Optional. Specify the PID for the new object.
#'   Defaults to automatically generating a new, random UUID-style PID.
#' @param sid (character) Optional. Specify a Series ID (SID) to use for the new object.
#'
#' @return (character) The PID of the updated object.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pid <- "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe"
#' my_path <- "/home/Documents/myfile.csv"
#' new_pid <- update_object(mn, pid, my_path, format_id = "text/csv")
#'}
update_object <- function(mn, pid, path, format_id = NULL, new_pid = NULL, sid = NULL) {
  stopifnot(is(mn, "MNode"))
  stopifnot(object_exists(mn, pid))
  stopifnot(file.exists(path))

  # Decide the format_id
  if (is.null(format_id)) {
    format_id <- guess_format_id(path)

    if (format_id == "application/xml") {
      stop(call. = FALSE, "No format_id was specified and this appears to be an XML document. Please specify the format_id and run this call again.")
    }

    warning(paste0("No format_id was specified so a guess was made based upon the file extension: ", format_id, "."))
  }

  check_format(format_id)

  message(paste0("Updating object ", pid, " with the file at ", path, "."))

  # Generate a PID if needed
  if (is.null(new_pid)) {
    new_pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  }

  # Grab and modify the old object's sysmeta
  sysmeta <- dataone::getSystemMetadata(mn, pid)
  sysmeta@identifier <- new_pid

  # Set a SID if needed
  if (!is.null(sid)) {
    sysmeta@seriesId <- sid
  }

  sysmeta@formatId <- format_id
  sysmeta@size <- file.size(path)
  sysmeta@checksum <- digest::digest(path, algo = "sha1", serialize = FALSE, file = TRUE)
  sysmeta@checksumAlgorithm <- "SHA1"
  slot(sysmeta, "obsoletes", check = FALSE) <- NA
  slot(sysmeta, "obsoletedBy", check = FALSE) <- NA
  sysmeta@fileName <- basename(path)

  # Set the replication policy back to default
  sysmeta <- clear_replication_policy(sysmeta)

  # Add packageId to metadata if the object is an xml file
  if (grepl("^eml:\\/\\/ecoinformatics.org\\/eml", format_id)) {
    eml <- EML::read_eml(path)
    eml@packageId <- new("xml_attribute", new_pid)
    path <- tempfile()
    EML::write_eml(eml, path)
    # File changed - update checksum
    sysmeta@checksum <- digest::digest(path, algo = "sha1", serialize = FALSE, file = TRUE)
  }

  # Make the update
  dataone::updateObject(mn,
                        pid = pid,
                        file = path,
                        newpid = new_pid,
                        sysmeta = sysmeta)
}


#' Publish an updated data package
#'
#' Publish an update to a data package after updating data files or metadata.
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
#' by generating a new identifier (a DOI if `use_doi = TRUE`) and updating the Member
#' Node with a public version of the object.  If metadata_file is not missing, it
#' should be an edited version of the metadata to be used to update the original. If
#' parent_resmap_pid is not missing, it indicates the PID of a parent package that
#' should be updated as well, using the parent_metadata_pid, parent_data_pids, and
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
#'   Not optional if a parent package exists.
#' @param parent_metadata_pid (character)  Optional. Identifier for the metadata document of the parent package.
#'   Not optional if a parent package exists.
#' @param parent_data_pids (character)  Optional. Identifier for the data objects of the parent package.
#'   Not optional if the parent package contains data objects.
#' @param parent_child_pids (character) Optional. Resource map identifier(s) of child packages in the parent package.
#'   \code{resource_map_pid} should not be included. Not optional if the parent package contains other child packages.
#' @param child_pids (character) Optional. Child packages resource map PIDs.
#' @param metadata_path (character or eml) Optional. An eml class object or a path to a metadata file to update with.
#'   If this is not set, the existing metadata document will be used.
#' @param public (logical) Optional. Make the update public. If `FALSE`, will set the metadata and resource map to private (but not the data objects).
#'   This applies to the new metadata PID and its resource map and data object.
#'   access policies are not affected.
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as arguments exist on the MN before continuing.
#'   Checks that objects exist and are of the right format type. This speeds up the function, especially when `data_pids` has many elements.
#'
#' @return (character) Named character vector of PIDs in the data package, including PIDs for the metadata, resource map, and data objects.
#'
#' @import dataone
#' @import datapack
#' @import EML
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#'
#' rm_pid <- "resource_map_urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe"
#' meta_pid <- "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe"
#' data_pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#' "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#'
#' meta_path <- "/home/Documents/myMetadata.xml"
#'
#' publish_update(mn, meta_pid, rm_pid, data_pids, meta_path, public = TRUE)
#'}
publish_update <- function(mn,
                           metadata_pid,
                           resource_map_pid,
                           data_pids = NULL,
                           child_pids = NULL,
                           metadata_path = NULL,
                           identifier = NULL,
                           use_doi = FALSE,
                           parent_resmap_pid = NULL,
                           parent_metadata_pid = NULL,
                           parent_data_pids = NULL,
                           parent_child_pids = NULL,
                           public = TRUE,
                           check_first = TRUE) {

  # Don't allow setting a dataset to private when it uses a DOI
  if (use_doi && !public) {
    stop("You cannot use a DOI and set 'public = FALSE' at the same time.")
  }

  # Do a simple sanity check on the PIDs passed in
  stopifnot(is.character(metadata_pid))
  stopifnot(is.character(resource_map_pid))

  if (!is.null(data_pids)) {
    stopifnot(all(is.character(data_pids)))
  }

  if (!is.null(child_pids)) {
    stopifnot(all(is.character(child_pids)))
  }

  if (!is.null(identifier)) {
    stopifnot(is.character(identifier))
  }

  if (!is.null(parent_resmap_pid)) {
    stopifnot(is.character(parent_resmap_pid))
  }

  if (!is.null(parent_metadata_pid)) {
    stopifnot(is.character(parent_metadata_pid))
  }

  if (!is.null(parent_data_pids)) {
    stopifnot(all(is.character(parent_data_pids)))
  }

  if (!is.null(parent_child_pids)) {
    stopifnot(all(is.character(parent_child_pids)))
  }

  # Check to see if the obsoleted package is in the list of parent_child_pids
  # If it is notify the user and remove it from the list
  if (resource_map_pid %in% parent_child_pids) {
    message("Removing the old resource map from the list of child PIDs in the parent package.")
    resource_map_pid_index <- which(resource_map_pid == parent_child_pids)
    parent_child_pids <- parent_child_pids[-resource_map_pid_index]
  }

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

  # Check that we have write permission on the metadata and resource map objects
  if (!all(is_authorized(mn, ids = c(metadata_pid, resource_map_pid), "write"))) {
    stop(paste0("You do not have authorization to 'write' to either the metadata PID (", metadata_pid, ") or the resource map PID (", resource_map_pid, ") so your call to this function will not succeed. Check your permissions and try again."))
  }

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
    # Check for obsoleted metadata_pid
    meta_obsoletedBy <- dataone::getSystemMetadata(mn, metadata_pid)@obsoletedBy
    if (!is.na(meta_obsoletedBy)) {
      stop("The value passed in for the argument 'metadata_pid' of '", metadata_pid, "' is already obsoleted by a newer version with PID '", meta_obsoletedBy, "'. All PID arguments to publish_update should be the latest versions of each object series.")
    }
  }

  # Check for obsoleted resource_map_pid. The resource map and metadata can desassociate without this check.
  rm_obsoletedBy <- dataone::getSystemMetadata(mn, resource_map_pid)@obsoletedBy
  if (!is.na(rm_obsoletedBy)) {
    stop("The value passed in for the argument 'resource_map_pid' of '", resource_map_pid, "' is already obsoleted by a newer version with PID '", rm_obsoletedBy, "'. All PID arguments to publish_update should be the latest versions of each object series.")
  }

  # Prepare the response object
  response <- list()

  # Set up some variables for use later on
  me <- get_token_subject()

  # Get some things from the node
  if (is.null(metadata_path)) {
    # Get the metadata doc
    message("Getting metadata from the MN.")
    doc <- EML::read_eml(rawToChar(dataone::getObject(mn, metadata_pid)), asText = TRUE)

  } else if (class(metadata_path)[1] == "emld") {
      # If an eml object is provided, use it directly after validating
      if (!eml_validate(metadata_path)) {
        stop("The EML object is not valid.")
      }

    doc <- metadata_path

  } else {
    # Alternatively, read an edited metadata file from disk if provided
    if (!file.exists(metadata_path)) {
      stop(paste0("Metadata doesn't exist: ", metadata_path))
    }

    message(paste0("Getting metadata from the path: ", metadata_path, "."))
    doc <- EML::read_eml(metadata_path)
  }

  # get the metadata sysmeta from the node
  metadata_sysmeta <- dataone::getSystemMetadata(mn, metadata_pid)

  message("Downloaded EML and sysmeta...")

  # Generate PIDs for our updated objects
  if (is.null(identifier)) {
    if (use_doi) {
      message("Minting a new DOI")
      metadata_updated_pid <- dataone::generateIdentifier(mn, scheme = "DOI")
      message(paste0("Minted a new DOI of ", metadata_updated_pid, "."))
    } else {
      metadata_updated_pid <- new_uuid()
      message(paste0("Using generated UUID PID of ", metadata_updated_pid, "."))
    }
  } else {
    message(paste0("Using manually-specified identifier of ", identifier, "."))
    metadata_updated_pid <- identifier
  }

  # Generate a resource map PID from the new metadata PID
  resmap_updated_pid <- paste0("resource_map_",  metadata_updated_pid)

  # Update the metadata

  # Replace packageId
  doc$packageId <- metadata_updated_pid

  # Replace system if needed
  if (is.null(doc$system)) {
    doc$system <- "https://search.dataone.org"
  }

  # Replace access if needed
  if (length(doc$access$allow) & (!is.null(metadata_path))) {
    doc$access <- list()
  }

  # Write out the document to disk. We do this in part because
  # set_other_entities takes a path to the doc.
  eml_path <- tempfile()
  EML::write_eml(doc, eml_path)

  # Create System Metadata for the updated EML file
  metadata_updated_sysmeta <- new("SystemMetadata",
                                  identifier = metadata_updated_pid,
                                  formatId = "eml://ecoinformatics.org/eml-2.1.1",
                                  size = file.size(eml_path),
                                  checksum = digest::digest(eml_path, algo = "sha1", serialize = FALSE, file = TRUE),
                                  checksumAlgorithm = "SHA1",
                                  submitter = me,
                                  rightsHolder = metadata_sysmeta@rightsHolder,
                                  obsoletes = metadata_pid,
                                  fileName = metadata_sysmeta@fileName)

  # Set the SID if one existed on old metadata object
  if (!is.na(metadata_sysmeta@seriesId)) {
    metadata_updated_sysmeta@seriesId <- metadata_sysmeta@seriesId
  }

  # Copy access and replication details from object we're updating
  metadata_updated_sysmeta@accessPolicy <- metadata_sysmeta@accessPolicy
  metadata_updated_sysmeta@replicationAllowed <- metadata_sysmeta@replicationAllowed
  metadata_updated_sysmeta@numberReplicas <- metadata_sysmeta@numberReplicas
  metadata_updated_sysmeta@preferredNodes <- metadata_sysmeta@preferredNodes
  metadata_updated_sysmeta@blockedNodes <- metadata_sysmeta@blockedNodes

  # Set the replication information to the defaults
  metadata_updated_sysmeta <- clear_replication_policy(metadata_updated_sysmeta)

  if (public) {
    # Make the metadata public
    metadata_updated_sysmeta <- datapack::addAccessRule(metadata_updated_sysmeta, "public", "read")

    # Make the data objects public
    for (data_pid in data_pids) {
      set_public_read(mn, data_pid)
    }
  } else {
    metadata_updated_sysmeta <- datapack::removeAccessRule(metadata_updated_sysmeta, "public", "read")
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

  message(paste0("Updated metadata document ", metadata_pid, " with ", metadata_updated_pid, "."))

  # Update the resource map
  #########################
  response[["resource_map"]] <- update_resource_map(mn,
                                                    resource_map_pid = resource_map_pid,
                                                    metadata_pid = metadata_updated_pid,
                                                    data_pids = data_pids,
                                                    child_pids = child_pids,
                                                    identifier = resmap_updated_pid,
                                                    public = public,
                                                    check_first = check_first)

  set_rights_holder(mn, response[["resource_map"]], metadata_sysmeta@rightsHolder)

  message("Updated resource map")

  # Update the parent resource map to add the new package
  #######################################################
  if (!is.null(parent_resmap_pid)) {
    if (is.null(parent_metadata_pid)) {
      stop("Missing required parameters to update parent package.")
    }

    message("Updating parent resource map...")

    # Check to see if the just-updated package is in the list of
    # parent_child_pids, notify the user, and add it to the list
    if (!(resmap_updated_pid %in% parent_child_pids)) {
      message("Adding the new resource map to the list of child PIDs in the parent package.")
      parent_child_pids <- c(parent_child_pids, resmap_updated_pid)
    }

    response[["parent_resource_map"]] <- update_resource_map(mn,
                                                             resource_map_pid = parent_resmap_pid,
                                                             metadata_pid = parent_metadata_pid,
                                                             data_pids = parent_data_pids,
                                                             child_pids = parent_child_pids,
                                                             public = public,
                                                             check_first = check_first)

    set_rights_holder(mn, response[["parent_resource_map"]], metadata_sysmeta@rightsHolder)
  }

  response[["data"]] <- data_pids
  response[["child_pids"]] <- child_pids

  return(response)
}


#' Create a resource map object on a Member Node
#'
#' This function first generates a new resource map RDF/XML document locally and
#' then uses the [dataone::createObject()] function to create the object on the
#' specified MN.
#'
#' If you only want to generate resource map RDF/XML, see [generate_resource_map()].
#'
#' @param mn (MNode) The Member Node
#' @param metadata_pid (character) The PID of the metadata object to go in the package.
#' @param data_pids (character) The PID(s) of the data objects to go in the package.
#' @param child_pids (character) The resource map PIDs of the packages to be
#'   nested under the package.
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as
#'   arguments exist on the MN before continuing. This speeds up the function,
#'   especially when `data_pids` has many elements.
#' @param ... Additional arguments that can be passed into [publish_object()].
#'
#' @return (character) The PID of the created resource map.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode('STAGING2')
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#'
#' meta_pid <- 'urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe'
#' dat_pid <- c('urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1',
#' 'urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe')
#'
#' create_resource_map(mn, metadata_pid = meta_pid, data_pids = dat_pid)
#'}
create_resource_map <- function(mn,
                                metadata_pid,
                                data_pids = NULL,
                                child_pids = NULL,
                                check_first = TRUE,
                                ...) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  if (check_first) {
    message("Checking all the object passed in as arguments exist before going on...")

    stopifnot(object_exists(mn, metadata_pid))
    if (!is.null(data_pids))
      stopifnot(all(object_exists(mn, data_pids)))
    if (!is.null(child_pids))
      stopifnot(all(object_exists(mn, child_pids)))
  }

  pid <- paste0("resource_map_urn:uuid:", uuid::UUIDgenerate())
  path <- generate_resource_map(metadata_pid = metadata_pid,
                                data_pids = data_pids,
                                child_pids = child_pids,
                                resource_map_pid = pid)

  stopifnot(file.exists(path))

  actual <- publish_object(mn,
                           path,
                           pid,
                           format_id = "http://www.openarchives.org/ore/terms",
                           ...)

  stopifnot(pid == actual)

  return(pid)
}


#' Update an existing resource map object on a Member Node
#'
#' This function first generates a new resource map RDF/XML document locally and
#' then uses the [dataone::updateObject()] function to update an object on the
#' specified MN.
#'
#' If you only want to generate resource map RDF/XML, see [generate_resource_map()].
#'
#' This function also can be used to add a new child packages to a
#' parent package. For example, if you have:
#'
#' Parent A B
#'
#' and want to add C as a sibling package to A and B, e.g.:
#'
#' Parent A B C
#'
#' then you could use this function.
#'
#' Note: This function currently replaces the rightsHolder on the resource map
#' temporarily to allow updating but sets it back to the rightsHolder that was
#' in place before the update.
#'
#' @param mn (MNode) The Member Node.
#' @param metadata_pid (character) The PID of the metadata object to go in the package.
#' @param data_pids (character) The PID(s) of the data objects to go in the package.
#' @param child_pids (character) The resource map PIDs of the packages to be
#'   nested under the package.
#' @param public (logical) Whether or not to make the new resource map public read.
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as
#'   arguments exist on the MN before continuing. This speeds up the function,
#'   especially when `data_pids` has many elements.
#' @param resource_map_pid (character) The PID of the resource map to be updated.
#' @param other_statements (data.frame) Extra statements to add to the resource map.
#' @param identifier (character) Manually specify the identifier for the new metadata object.
#'
#' @return (character) The PID of the updated resource map.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode('STAGING2')
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#'
#' rm_pid <- "resource_map_urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe"
#' meta_pid <- "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe"
#' data_pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#' "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#'
#' rm_new <- update_resource_map(mn, rm_pid, meta_pid, data_pids)
#'}
update_resource_map <- function(mn,
                                resource_map_pid,
                                metadata_pid,
                                data_pids = NULL,
                                child_pids = NULL,
                                other_statements = NULL,
                                identifier = NULL,
                                public = FALSE,
                                check_first = TRUE) {

  # Check arguments
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(resource_map_pid),
            nchar(resource_map_pid) > 0)
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  if (check_first) {
    message("Checking all the object passed in as arguments exist before going on...")

    stopifnot(object_exists(mn, resource_map_pid))
    stopifnot(object_exists(mn, metadata_pid))
    if (!is.null(data_pids))
      stopifnot(object_exists(mn, data_pids))
    if (!is.null(child_pids))
      stopifnot(object_exists(mn, child_pids))
    stopifnot(is_resource_map(mn, resource_map_pid))
    if (!is.null(child_pids))
    stopifnot(all(is_resource_map(mn, child_pids)))
  }

  # Get the current rightsHolder
  sysmeta <- dataone::getSystemMetadata(mn, resource_map_pid)
  stopifnot(is(sysmeta, "SystemMetadata"))

  previous_rights_holder <- sysmeta@rightsHolder

  # Set the rightsHolder to us temporarily
  me <- get_token_subject()
  set_rights_holder(mn, resource_map_pid, me)

  # Create the replacement resource map
  if (is.null(identifier)) {
    identifier <- paste0("resource_map_", new_uuid())
  }

  new_rm_path <- generate_resource_map(metadata_pid = metadata_pid,
                                       data_pids = data_pids,
                                       child_pids = child_pids,
                                       resource_map_pid = identifier)
  stopifnot(file.exists(new_rm_path))

  rm(sysmeta)

  message(paste0("Getting updated copy of System Metadata for ", resource_map_pid))
  sysmeta <- dataone::getSystemMetadata(mn, resource_map_pid)
  stopifnot(is(sysmeta, "SystemMetadata"))

  new_rm_sysmeta <- sysmeta
  new_rm_sysmeta@identifier <- identifier
  new_rm_sysmeta@size <- file.size(new_rm_path)
  new_rm_sysmeta@checksum <- digest::digest(new_rm_path, algo = "sha1", serialize = FALSE, file = TRUE)
  new_rm_sysmeta@checksumAlgorithm <- "SHA1"
  new_rm_sysmeta@rightsHolder <- previous_rights_holder
  new_rm_sysmeta@obsoletes <- resource_map_pid
  slot(new_rm_sysmeta, "obsoletedBy", check = FALSE) <- NA

  # Set the replication policy back to default
  new_rm_sysmeta <- clear_replication_policy(new_rm_sysmeta)

  new_rm_sysmeta <- add_admin_group_access(new_rm_sysmeta)

  if (public) {
    new_rm_sysmeta <- datapack::addAccessRule(new_rm_sysmeta, "public", "read")
  } else {
    new_rm_sysmeta <- datapack::removeAccessRule(new_rm_sysmeta, "public", "read")
  }

  # Update it
  message(paste0("Updating resource map..."))
  resmap_update_response <- dataone::updateObject(mn,
                                                  pid = resource_map_pid,
                                                  newpid = identifier,
                                                  sysmeta = new_rm_sysmeta,
                                                  file = new_rm_path
  )

  # Set the rightsHolder back
  set_rights_holder(mn, resource_map_pid, previous_rights_holder)

  if (file.exists(new_rm_path)) {
    file.remove(new_rm_path)
  }

  message(paste0("Successfully updated ", resource_map_pid, " with ", identifier, "."))

  return(resmap_update_response)
}


#' Set the file name for an object
#'
#' Set the file name for an object.
#'
#' @param mn (MNode) The Member Node.
#' @param pid (character) The PID of the object to set the file name on.
#' @param name (character) The file name.
#'
#' @return (logical) Whether the update succeeded.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn, "urn:node:mnTestKNB")
#'
#' pid <- "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe"
#' set_file_name(mn, pid, "myfile.csv")
#' }
set_file_name <- function(mn, pid, name) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(pid),
            nchar(pid) > 0)
  stopifnot(is.character(name),
            nchar(name) > 0)

  sysmeta <- dataone::getSystemMetadata(mn, pid)

  if (!is.na(sysmeta@fileName)) {
    if (sysmeta@fileName == name)
      stop(paste0("fileName for object ", pid, "is already set to: ", name))
  }

  sysmeta@fileName <- name
  dataone::updateSystemMetadata(mn, pid, sysmeta)
}


#' Update physical of an updated data object
#'
#' This function updates the EML with the new physical
#' of a data object once it has been updated.
#' This is a helper function for [update_package_object()].
#'
#' @param eml (eml) An EML class object.
#' @param mn (MNode) The Member Node of the data package.
#' @param data_pid (character) The identifier of the data object to be updated.
#' @param new_data_pid (character) The new identifier of the updated data object.
#'
#' @importFrom stringr str_detect
#'
#' @noRd
update_physical <- function(eml, mn, data_pid, new_data_pid) {
  stopifnot(is(eml, "eml"))
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(data_pid), nchar(data_pid) > 0)
  stopifnot(is.character(new_data_pid), nchar(new_data_pid) > 0)

  all_url <- unlist(EML::eml_get(eml, "url"))
  if (sum(stringr::str_detect(all_url, data_pid)) == 0) {
    stop("The obsoleted data PID does not match any physical sections, so the EML will not be updated.")
  }

  dataTable_url <- unlist(EML::eml_get(eml@dataset@dataTable, "url"))

  if (any(stringr::str_detect(dataTable_url, data_pid))) {
    position <- which(stringr::str_detect(dataTable_url, data_pid))
    new_phys <- pid_to_eml_physical(mn, new_data_pid)
    eml@dataset@dataTable[[position]]@physical@.Data <- new_phys
  }

  otherEntity_url <- unlist(EML::eml_get(eml@dataset@otherEntity, "url"))

  if (any(stringr::str_detect(otherEntity_url, data_pid))) {
    position <- which(stringr::str_detect(otherEntity_url, data_pid))
    new_phys <- pid_to_eml_physical(mn, new_data_pid)
    eml@dataset@otherEntity[[position]]@physical@.Data <- new_phys
  }

  spatialVector_url <- unlist(EML::eml_get(eml@dataset@spatialVector, "url"))

  if (any(stringr::str_detect(spatialVector_url, data_pid))) {
    position <- which(stringr::str_detect(spatialVector_url, data_pid))
    new_phys <- pid_to_eml_physical(mn, new_data_pid)
    eml@dataset@spatialVector[[position]]@physical@.Data <- new_phys
  }

  invisible(eml)
}


#' Update a data object and associated resource map and metadata
#'
#' This function updates a data object and then automatically
#' updates the package resource map with the new data PID. If an object
#' already has a `dataTable`, `otherEntity`, or `spatialVector`
#' with a working physical section, the EML will be updated with the new physical.
#' It is a convenience wrapper around [update_object()] and [publish_update()].
#'
#' @param mn (MNode) The Member Node of the data package.
#' @param data_pid (character) PID for data object to update.
#' @param new_data_path (character) Path to new data object.
#' @param resource_map_pid (character) PID for resource map to update.
#' @param format_id (character) Optional. The format ID to set for the object.
#'   When not set, [guess_format_id()] will be used
#'   to guess the format ID. Should be a \href{https://cn.dataone.org/cn/v2/formats}{DataONE format ID}.
#' @param public (logical) Optional. Make the update public. If `FALSE`,
#'   will set the metadata and resource map to private (but not the data objects).
#'   This applies to the new metadata PID and its resource map and data object.
#'   Access policies are not affected.
#' @param use_doi (logical) Optional. If `TRUE`, a new DOI will be minted.
#' @param ... Other arguments to pass into [publish_update()].
#'
#' @return (character) Named character vector of PIDs in the data package, including PIDs
#' for the metadata, resource map, and data objects.
#'
#' @import dataone
#' @import EML
#'
#' @export
#'
#' @seealso [update_object()] [publish_update()]
#'
#' @examples
#' \dontrun{
#' cnTest <- dataone::CNode("STAGING")
#' mnTest <- dataone::getMNode(cnTest,"urn:node:mnTestARCTIC")
#'
#' pkg <- create_dummy_package_full(mnTest, title = "My package")
#'
#' file.create("new_file.csv")
#' update_package_object(mnTest, pkg$data[1], "new_file.csv", pkg$resource_map, format_id = "text/csv")
#' file.remove("new_file.csv")
#' }
update_package_object <- function(mn,
                                  data_pid,
                                  new_data_path,
                                  resource_map_pid,
                                  format_id = NULL,
                                  public = TRUE,
                                  use_doi = FALSE,
                                  ...) {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(data_pid), nchar(data_pid) > 0)
  stopifnot(is.character(new_data_path), nchar(new_data_path) > 0, file.exists(new_data_path))
  stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
  stopifnot(is.logical(public))

  pkg <- get_package(mn, resource_map_pid)
  eml <- EML::read_eml(rawToChar(dataone::getObject(mn, pkg$metadata)))

  new_data_pid <- update_object(mn,
                                pid = data_pid,
                                path = new_data_path,
                                format_id = format_id)

  other_data_pids <- pkg$data[which(pkg$data != data_pid)] # wrapped in which for better NA handling
  new_data_pids <- c(other_data_pids, new_data_pid)

  eml_new <- tryCatch(update_physical(eml = eml,
                                      mn = mn,
                                      data_pid = data_pid,
                                      new_data_pid = new_data_pid),
                      error = function(e) {
                        message("The obsoleted data PID does not match any physical sections, so the EML will not be updated.",
                                "\nCheck if the correct resource map PID was given.")
                        return(eml)
                      })

  eml_path <- tempfile(fileext = ".xml")
  EML::write_eml(eml_new, eml_path)

  pkg_new <- publish_update(mn,
                            metadata_pid = pkg$metadata,
                            resource_map_pid = pkg$resource_map,
                            metadata_path = eml_path,
                            data_pids = new_data_pids,
                            child_pids = pkg$child_packages,
                            public = public,
                            use_doi = use_doi,
                            ...)

  file.remove(eml_path)

  cat("\nThe new data pid is:", new_data_pid)

  return(pkg_new)
}
