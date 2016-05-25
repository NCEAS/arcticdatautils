#' eml.R
#'
#' Helpers for creating EML.


#' Create an EML otherEntity sub-tree for the given PID.
#'
#' Note this is a wrapper around sysmeta_to_entity which handles the task of
#' creating the EML otherEntity sub-ree.
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pid (character) The PID of the object to create the sub-tree for.
#' @param sysmeta (SystemMetadata) Optional. Manually pass in System Metadata. This avoids an extra network request if the calling environment has already loaded the System Metadata.
#'
#' @return (otherEntity) The XML sub-tree.
#' @export
#'
#' @examples
pid_to_entity <- function(mn, pid, sysmeta=NULL) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  # Get the System Metadata if needed
  if (is.null(sysmeta)) {
    sysmeta <- dataone::getSystemMetadata(mn, pid)
  }

  sysmeta_to_entity(sysmeta)
}

#' Create an EML otherEntity sub-tree for the given object.
#'
#' @param sysmeta (SystemMetadata) A SystemMedata instance for the object.
#'
#' @return (otherEntity) The XML sub-tree.
#' @export
#'
#' @examples
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


#' Creates and adds EML otherEntity elements to an existing EML document.
#'
#' @param mn (MNode) The Member Node the objects exist on.
#' @param path (character) The location on disk of the EML file.
#' @param pids (character) One or more PIDs for the objects.
#'
#' @return (character) The path to the updated EML file.
#' @export
#'
#' @examples
add_other_entities <- function(mn, path, pids) {
  stopifnot(class(mn) == "MNode")
  stopifnot(file.exists(path))
  stopifnot(all(is.character(pids)),
            all(nchar(pids) > 0))

  log_message("Adding EML otherEntity elements...")

  # Get the metadata document from the MN and load it as an EML document
  doc <- EML::read_eml(path)
  stopifnot(class(doc) == "eml")

  # Check for any existing otherEntity elements and quit if any are found
  if (length(doc@dataset@otherEntity) != 0) {
    warning("Metadata file already contains one or more otherEntity elements. All existing otherEntity elements will be removed.")
  }

  # Create the otherEntity elements as a list
  other_entities <- lapply(pids, function(pid) {
    pid_to_entity(mn, pid)
  })
  stopifnot(length(other_entities) > 0)

  doc@dataset@otherEntity <- new("ListOfotherEntity", other_entities)
  stopifnot(length(doc@dataset@otherEntity) == length(pids))

  # Write the modified document back to disk and stop if it isn't valid
  EML::write_eml(doc, path)
  stopifnot(EML::eml_validate(path) == TRUE)

  path
}
