#' package.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Code related to packaging datasets.

library(dataone)
library(datapackage)


#' Manually create a Data Package from files in the Inventory.
#'
#' @param inventory An Inventory (data.frame)
#' @param package The package identifier (character)
#' @param child_packages Resource Map PIDs for child Data Packages (character)
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' insert_package(my_inventory_df, "my_package_id")
insert_package <- function(inventory, package, child_packages=c()) {
  # Debug
  inventory <- "YYY"
  package <- "XXX"
  child_packages <- c()

  # Config
  mn <- MNode("https://dev.nceas.ucsb.edu/knb/d1/mn/v2")
  me <- "CN=Bryce Mecum A27576,O=Google,C=US,DC=cilogon,DC=org"
  files_base_path <- "~/src/arctic-data/packages/"

  # Find the package contents (metadata and data)
  files <- inventory[inventory$package == package,]
  # Remove this once I fix my bug
  files <- files[!is.na(files$package),]
  stopifnot(nrow(files) > 0)

  # SCIENCE METADATA (1)
  # Find the metadata object
  files_metadata <- files[files$is_metadata == TRUE,]
  stopifnot(nrow(files_metadata) == 1)
  files_metadata[,"file"]


  path_on_disk <- paste0(files_base_path, files_metadata[1,"filename"])

  # Generate and save PID
  metadata_pid <- generateIdentifier(mn)
  metadata_sysmeta <- create_sysmeta(metadata_pid, files_metadata[1,], me)

  create(mn,
         metadata_pid,
         filepath = path_on_disk,
         sysmeta = metadata_sysmeta)


  # DATA (MANY)
  files_data <- files[files$is_metadata == FALSE,]
  files_data[,"file"]

  # Create them
  # Mint PIDs, saving them for later
  data_pids <- c()

  for (i in seq_len(nrow(files_data))) {
    path_on_disk <- paste0(files_base_path, files_data[i,"filename"])

    # Generate and save PID
    data_pid <- generateIdentifier(mn)

    data_sysmeta <- create_sysmeta(data_pid, files_data[i,], me)
    cat(paste0("Caling MN.create() on ", files_data[i,"file"], ".\n"))
    create(mn,
           data_pid,
           filepath = path_on_disk,
           sysmeta = data_sysmeta)

    data_pids <- c(data_pids, data_pid)
  }

  # Generate a resource map
  # Create the resource map
  # Create sysmeta for the resource map
  resource_map_filepath <- create_resource_map(metadata_pid, data_pids)
  resource_map_size_bytes <- file.info(resource_map_filepath)$size
  resource_map_checksum <- digest(resource_map_filepath, algo = "sha256")

  # Get the PID from the resource map XML directly
  # Here I use the ore:isAggregatedBy which points to the #aggregation because
  # this seems like the best way to get the resource map PID. There's probably
  # a better way to do this.

  aggregation_uri <- read_xml(resource_map_filepath) %>%
    xml_find_one("//ore:isAggregatedBy", xml_ns(myxml)) %>%
    xml_attr("rdf:resource", xml_ns(myxml))

  stopifnot(is.character(aggregation_uri),
            nchar(aggregation_uri) > 0)

  aggregation_uri_nobaseurl <- gsub("https://cn.dataone.org/cn/v1/resolve/", "", aggregation_uri)
  resource_map_pid <- gsub("#aggregation", "", aggregation_uri_nobaseurl)

  resource_map_values <- data.frame("format_id" = "http://www.openarchives.org/ore/terms",
                                    "size_bytes" = resource_map_size_bytes,
                                    "checksum_sha256" = resource_map_checksum,
                                    "file" = paste0(resource_map_pid, ".xml"),
                                    stringsAsFactors = FALSE)

  resource_map_sysmeta <- create_sysmeta(resource_map_pid, resource_map_values[1,], me)
  create(mn,
         resource_map_pid,
         filepath = resource_map_filepath,
         sysmeta = resource_map_sysmeta)
}

#' Create a SystemMetadata object. This is a convenience wrapper around just
#' calling new("SystemMetadata", ...) directly.
#'
#' @param pid The PID for the Object. (character)
#' @param inventory_file An Inventory
#' @param who DN for the rightsHolder and submitter (character)
#'
#' @return THe SystemMetadata (SystemMetadata)
#' @export
#'
#' @examples
#' create_sysmeta("my_pid", my_inv[1:10,], CN=Me,O=Test,C=US,DC=cilogon,DC=org")
create_sysmeta <- function(pid, inventory_file, who) {
  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = inventory_file[,"format_id"],
                 size = inventory_file[,"size_bytes"],
                 checksum = inventory_file[,"checksum_sha256"],
                 checksumAlgorithm = "SHA256",
                 submitter = who,
                 rightsHolder = who,
                 fileName = inventory_file[,"file"],
                 originMemberNode = "urn:node:arctica",
                 authoritativeMemberNode = "urn:node:artica")


  sysmeta <- addAccessRule(sysmeta, "public", "read")
  sysmeta <- addAccessRule(sysmeta, who, "write")
  sysmeta <- addAccessRule(sysmeta, who, "changePermission")

  sysmeta
}


#' Create a Resource Map. This is a convenience
#'
#' @param metadata_pid PID of the metadata Object (character)
#' @param data_pids PID(s) of the data Objects (character)
#' @param child_pids (Optional) PID(s) of child Resource Maps (character)
#' @param resolve_base (Optional) The resolve service base URL (character)
#'
#' @return Absolute path to the Resource Map on disk (character)
#' @export
#'
#' @examples
create_resource_map <- function(metadata_pid,
                                data_pids,
                                child_pids=c(),
                                resolve_base="https://cn.dataone.org/cn/v1/resolve/") {
  stopifnot(length(metadata_pid) == 1)
  stopifnot(length(data_pids) >= 1)

  relationships <- data.frame()

  for (data_pid in data_pids) {
    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base, metadata_pid),
                                      predicate = "http://purl.org/spar/cito/documents",
                                      object = paste0(resolve_base, data_pid),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))

    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base, data_pid),
                                      predicate = "http://purl.org/spar/cito/isDocumentedBy",
                                      object = paste0(resolve_base, metadata_pid),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))
  }

  resource_map <- createFromTriples(new("ResourceMap"),
                                    relations = relationships,
                                    identifiers = c(metadata_pid, data_pids))
  outfilepath <- tempfile()
  serializeRDF(resource_map, outfilepath)

  outfilepath
}
