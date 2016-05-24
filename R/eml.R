#' eml.R
#'
#' Helpers for creating EML.
#'


pid_to_entity <- function(mn, pid, sysmeta=NULL) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  # Get the System Metadata if needed
  if (is.null(sysmeta)) {
    sysmeta <- getSystemMetadata(mn, pid)
  }

  sysmeta_to_entity(sysmeta)
}

sysmeta_to_entity <- function(sysmeta) {
  stopifnot(class(sysmeta) == "SystemMetadata")

  # otherEntity
  other_entity <- new("otherEntity")
  other_entity@id <- new("xml_attribute", sysmeta@checksum)
  other_entity@scope <- new("xml_attribute", "document")

  if (is.na(sysmeta@fileName)) {
    other_entity@entityName <- new("entityName", "NA")
  }
  else {
    other_entity@entityName <- new("entityName", sysmeta@fileName)
  }

  other_entity@entityType <- "Other"

  # otherEntity/physical
  phys <- new("physical")
  phys@scope <- new("xml_attribute", "document")

  if (is.na(sysmeta@fileName)) {
    phys@objectName <- new("objectName", sysmeta@fileName)
  } else {
    phys@objectName <- new("objectName", "NA")
  }

  phys@size <- new("size", format(sysmeta@size, scientific = FALSE))
  phys@authentication <- new("ListOfauthentication", list(new("authentication", sysmeta@checksum)))
  phys@authentication[[1]]@method <- new("xml_attribute", sysmeta@checksumAlgorithm)

  phys@dataFormat <- new("dataFormat")
  phys@dataFormat@externallyDefinedFormat <- new("externallyDefinedFormat")
  phys@dataFormat@externallyDefinedFormat@formatName <- sysmeta@formatId

  phys@distribution <- new("ListOfdistribution", list(new("distribution")))
  phys@distribution[[1]]@scope  <- new("xml_attribute", "document")
  phys@distribution[[1]]@online <- new("online")
  phys@distribution[[1]]@online@url <- new("url", paste0("ecogrid://knb/", sysmeta@identifier))
  slot(phys@distribution[[1]]@online@url, "function") <- new("xml_attribute", "download")

  other_entity@physical <- new("ListOfphysical", list(phys))

  other_entity
}

#' Convert an object to an EML otherEntity XML tree.
#'
#' @param pid (character) PID of the object to generate entity information for.
#' @param sysmeta (SystemMetadata) Optional. An already in-memory System Metadata object. Avoids having to make a network request.
#'
#' @return The EML otherEntity (otherEntity)
pid_to_entity <- function(mn, pid, sysmeta=NULL) {

}


add_other_entities <- function(mn, path, pids) {
  stopifnot(class(mn) == "MNode")
  stopifnot(file.exists(path))
  stopifnot(all(is.character(pids)),
            all(nchar(pids) > 0))

  log_message(paste0("Adding EML otherEntity elements for ", pids))

  # Get the metadata document from the MN and load it as an EML document
  doc <- EML::read_eml(path)
  stopifnot(class(doc) == "eml")

  # Check for any existing otherEntity elements and quit if any are found
  if (length(doc@dataset@otherEntity) != 0) {
    stop(paste0("Metadata file already contains one or more otherEntity elements. Stopping."))
  }

  # Create the otherEntity elements as a list
  other_entities <- lapply(pids, function(pid) {
    pid_to_entity(mn, pid)
  })

  doc@dataset@otherEntity <- new("ListOfotherEntity", other_entities)
  stopifnot(length(doc@dataset@otherEntity) == length(pids))

  # Write the modified document back to disk and stop if it isn't valid
  write_eml(doc, path)
  stopifnot(EML::eml_validate(path) == TRUE)

  path
}

# add_other_entities <- function(mn,
#                                metadata_pid,
#                                data_pids,
#                                resmap_pid,
#                                child_pids=NULL,
#                                use_doi=FALSE,
#                                parent_resmap_pid=NULL,
#                                parent_metadata_pid=NULL,
#                                parent_data_pids=NULL,
#                                parent_child_pids=NULL) {
#   stopifnot(class(mn) == "MNode")
#   stopifnot(is.character(metadata_pid),
#             nchar(metadata_pid) > 0,
#             object_exists(mn, metadata_pid))
#   stopifnot(all(is.character(data_pids)),
#             all(nchar(data_pids) > 0),
#             all(object_exists(mn, metadata_pid)))
#
#
#   # Get the metadata document from the MN and load it as an EML document
#   metadata_doc <- rawToChar(getObject(mn, metadata_pid))
#   eml_path <- tempfile(fileext = ".xml")
#   writeLines(metadata_doc, con = eml_path)
#   doc <- EML::read_eml(eml_path)
#
#   # Check for any existing otherEntity elements and quit if any are found
#   if (length(doc@dataset@otherEntity) != 0) {
#     stop(paste0("Existing metadata object with PID ", metadata_pid, " already contains one or more otherEntity elements."))
#   }
#
#   # Create the otherEntity elements as a list
#   other_entities <- lapply(data_pids, function(pid) {
#     data_sysmeta <- getSystemMetadata(mn, pid)
#     object_to_entity(data_sysmeta)
#   })
#
#   doc@dataset@otherEntity <- new("ListOfotherEntity", other_entities)
#
#   # Write the modified document back to disk and stop if it isn't valid
#   write_eml(doc, eml_path)
#   stopifnot(EML::eml_validate(eml_path) == TRUE)
#
#   # Publish the update
#   # Note that a number of the arguments here as simply passesd along directly
#   # from the call to this function
#   publish_update(mn,
#                  metadata_old_pid = metadata_pid,
#                  resmap_old_pid = resmap_pid,
#                  data_old_pids = data_pids,
#                  child_pids = child_pids,
#                  metadata_file_path = eml_path,
#                  use_doi = use_doi,
#                  parent_resmap_pid=NULL,
#                  parent_metadata_pid=NULL,
#                  parent_data_pids=NULL,
#                  parent_child_pids=NULL)
#
#
#   # Clean up the temporary EML file
#   file.remove(eml_path)
# }
