#' helpers.R
#'
#' Various helper functions for things like testing the package.


#' Create a test metadata object.
#'
#' @param mn (MNode) The Member Node.
#' @param data_pids (character) Optional. PIDs for data objects the metadata documents.
#' @return pid (character) PID of published metadata document.
#' @export
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")

#' pid <- create_dummy_metadata(mn)
#' }
create_dummy_metadata <- function(mn, data_pids=NULL) {
  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  me <- get_token_subject()

  # Copy the original EML file to a temporary place
  original_file <- file.path(system.file(package = "arcticdatautils"),
                             "example-eml.xml")
  metadata_file <- tempfile()
  file.copy(original_file, metadata_file)

  sysmeta <- new("SystemMetadata",
                 id = pid,
                 formatId = "eml://ecoinformatics.org/eml-2.1.1",
                 size = file.size(metadata_file),
                 checksum = digest::digest(metadata_file, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_science_metadata.xml")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating metadata ", pid))
  pid <- dataone::createObject(mn, pid, metadata_file, sysmeta)

  # Remove the temporary EML File
  file.remove(metadata_file)

  pid
}

#' Create a test object.
#'
#' @param mn (MNode) The Member Node.
#'
#' @return pid (character) The pid of the dummy object.
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#'
#' pid <- create_dummy_object(mn)
#'}
create_dummy_object <- function(mn) {
  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  me <- get_token_subject()
  tmp <- tempfile()

  writeLines(paste0(sample(LETTERS, 26, replace = TRUE), collapse = ""), con = tmp)

  sysmeta <- new("SystemMetadata",
                 id = pid,
                 formatId = "application/octet-stream",
                 size = file.size(tmp),
                 checksum = digest::digest(tmp, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_object")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating object ", pid))
  create_response <- dataone::createObject(mn, pid, tmp, sysmeta)

  file.remove(tmp)

  create_response
}

#' Create a test package.
#'
#' @param mn (MNode) The Member Node.
#' @param size (numeric) The number of files in the package, including the metadata file.
#'
#' @return pids (character) A named character vector of the data pids in the package.
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' #Create dummy package with 5 data objects and 1 metadata object
#' pids <- create_dummy_package(mn, 6)
#' }
create_dummy_package <- function(mn, size = 2) {
  me <- get_token_subject()

  # Data objects
  if (size > 1) {
    data_pids <- sapply(seq_len(size - 1), function(i) {
      create_dummy_object(mn)
    })

    data_pids <- data_pids[!is.na(data_pids)]  # Filter NA pids (failed creates)

  } else {
    data_pids <- NULL
  }

  # Metadata objects
  meta_pid <- create_dummy_metadata(mn, data_pids = data_pids)

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  resmap_path <- generate_resource_map(meta_pid,
                                       data_pids,
                                       resource_map_pid = pid)

  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = "http://www.openarchives.org/ore/terms",
                 size = file.size(resmap_path),
                 checksum = digest::digest(resmap_path, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_resource_map.xml")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating resource map ", pid))
  resource_map_pid <- dataone::createObject(mn, pid, resmap_path, sysmeta)

  list(metadata = meta_pid,
       resource_map = resource_map_pid,
       data = data_pids)
}

#' Create a test parent package.
#'
#' @param mn (MNode) The Member Node.
#' @param children (character) Child package (resource maps) PIDs.
#'
#' @return pid (character) Named character vector of PIDs including parent package and child package pids.
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
# cn <- CNode("STAGING2")
# mn <- getMNode(cn,"urn:node:mnTestKNB")
#
# child_pid <- "urn:uuid:39a59f99-118b-4c81-9747-4b6c43308e00"
#
# create_dummy_parent_package(mn, child_pid)
#'}
create_dummy_parent_package <- function(mn, children) {
  me <- get_token_subject()
  meta_pid <- create_dummy_metadata(mn)

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  resmap_path <- generate_resource_map(meta_pid,
                                       data_pids = c(),
                                       child_pids = children,
                                       resource_map_pid = pid)

  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = "http://www.openarchives.org/ore/terms",
                 size = file.size(resmap_path),
                 checksum = digest::digest(resmap_path, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_resource_map.xml")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating parent package map ", pid))
  create_response <- createObject(mn, pid, resmap_path, sysmeta)

  list(parent = create_response,
       children = children)
}


#' Create dummy attributes data frame
#'
#' @param numberAttributes (integer) Number of attributes to be created in the table
#' @param factors (character) Optional vector of factor names to include.
#'
#' @return dataframe
#' @export
#'
#' @examples
create_dummy_attributes_dataframe <- function(numberAttributes, factors=NULL) {
  names <- sapply(1:numberAttributes, function(x){ paste0("Attribute ", x) })
  
  if(!is.null(factors)) {
    domains <- c(rep("textDomain", numberAttributes - length(factors)),
                 rep("enumeratedDomain", length(factors)))
    names[(numberAttributes - length(factors) + 1):numberAttributes] <- factors
  }
  
  df <- data.frame(attributeName = names,
                   attributeDefinition = names,
                   measurementScale = rep("nominal", numberAttributes),
                   domain = rep("textDomain", numberAttributes),
                   formatString = rep("NA", numberAttributes),
                   definition = names,
                   unit = rep("NA", numberAttributes),
                   numberType = rep("NA", numberAttributes),
                   missingValueCode = rep("NA", numberAttributes),
                   missingValueCodeExplanation = rep("NA", numberAttributes),
                   stringsAsFactors = F)
  
  df
}

#' Create dummy enumeratedDomain data frame
#'
#' @param factors (character) Optional vector of factor names to include.
#'
#' @return
#' @export
#'
#' @examples
create_dummy_enumeratedDomain_dataframe <- function(factors) {
  names <- rep(factors, 4)
  df <- data.frame(attributeName = names,
                   code = paste0(names, 1:length(names)),
                   definition = names)
  
  df
}
