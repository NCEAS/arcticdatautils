#' package.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Code related to inserting datasets as Data Packages.


#' Create a single package Data Package from files in the Inventory.
#'
#' @param inventory An Inventory (data.frame)
#' @param package The package identifier (character)
#' @param child_pids Resource Map PIDs for child Data Packages (character)
#' @param env Environment variables (list)
#'
#' @return A list containing PIDs and whether objects were inserted. (list)
#' @export
#'
#' @examples
#' insert_package(my_inventory_df, "my_package_id")
#'
#' TODO
#' Add routine to check for the files elsewhere
#' This should be automated and based upon the inventory file having another
#' column with that information

insert_package <- function(inventory, package, child_pids=c(), env=list()) {
  # Validate the environment parameter
  stopifnot(!is.null(env), length(env) > 0)
  stopifnot(length(setdiff(c("base_directory", "identifier_scheme", "mn", "submitter", "rights_holder"), names(env))) == 0)
  stopifnot(all(unlist(lapply(env, nchar)) > 0))

  # Configuration
  library(dataone)  # TODO Remove this library call once the package is fixed
  mn <- MNode(env$mn)
  submitter <- env$submitter
  rights_holder <- env$rights_holder
  base_path <- env$base_directory

  stopifnot(class(mn) == "MNode",
            nchar(submitter) > 0,
            nchar(rights_holder) > 0,
            file.exists(base_path))


  # Find the package contents (metadata and data)
  files <- inventory[inventory$package == package,]
  files <- files[!is.na(files$package),]   # TODO: Remove this once I fix my bug
  stopifnot(nrow(files) > 0)

  # Seperate files out into metadata and data by capturing their indicies
  # in the files data.frame
  files_idx_metadata <- which(files$is_metadata == TRUE)
  files_idx_data <- which(files$is_metadata == FALSE)
  stopifnot(length(files_idx_metadata) == 1)


  # Process metadata

  # Metadata PID
  files[files_idx_metadata,"pid"] <- get_or_create_pid(files[files_idx_metadata,], mn, scheme = "UUID")

  if (is.na(files[files_idx_metadata,"pid"])) {
    cat(paste0("Metadata PID was NA for package ", package, ".\n"))
    return(files)
  }

  # Metadata SystemMetadata
  metadata_sysmeta <- create_sysmeta(files[files_idx_metadata,],
                                     base_path,
                                     submitter,
                                     rights_holder)

  if (is.null(metadata_sysmeta)) {
    cat(paste0("System Metadata creation failed for metadata object in package ", package, ".\n"))
    return(files)
  }

  # Metadata Object
  files[files_idx_metadata,"created"] <- create_object(files[files_idx_metadata,],
                                                       metadata_sysmeta,
                                                       base_path,
                                                       mn)

  if (files[files_idx_metadata,"created"] == FALSE) {
    cat(paste0("Object creation failed for metadata object in package ", package, ".\n"))
    return(files)
  }

  print(files)

  return(files)

}




# # DATA (MANY)
# # Create them
# # Mint PIDs, saving them for later
# data_pids <- c()
#
# for (i in seq_len(nrow(files_data))) {
#   path_on_disk <- paste0(base_path, files_data[i,"filename"])
#
#   # Generate and save PID
#   if (!is.na(files_metadata[,"PID"])) {
#     data_pid <- files_data[,"PID"]
#   } else {
#     data_pid <- dataone::generateIdentifier(mn)
#   }
#
#   data_sysmeta <- NULL
#   data_sysmeta <- tryCatch(
#     {
#       create_sysmeta(data_pid,
#                      files_data[i,],
#                      submitter,
#                      rights_holder)
#     },
#     warning = function(w) {
#       cat(paste0("Warning generated while creating SystemMetadata for ", files_data[i,"file"]))
#     },
#     error = function(e) {
#       cat(paste0("Error generated while creating SystemMetadata for ", files_data[i,"file"]))
#     })
#
#   if (is.null(data_sysmeta)) {
#     cat(paste0("Failed to create SystemMetadata for a data object. Returning early.\n"))
#     return(list("metadata_pid" = metadata_pid,
#                 "metadata_created" = TRUE))
#   }
#
#   cat(paste0("Caling MN.create() on ", files_data[i,"file"], " for PID '", data_pid, "'.\n"))
#   data_create_result <- tryCatch(
#     {
#       dataone::create(mn,
#                       data_pid,
#                       file = path_on_disk,
#                       sysmeta = data_sysmeta)
#     },
#     warning = function(w) {
#       cat(paste0("Warning generated while calling MNStorage.create() for ", files_data[i,"file"]))
#     },
#     error = function(e) {
#       cat(paste0("Error generated while calling MNStorage.create() for ", files_data[i,"file"]))
#     })
#
#   # TODO check data_create_result
#   data_create_result
#
#   if (is.null(data_create_result)) {
#     cat(paste0("Failed to create data object. Returning early.\n"))
#     return(list("metadata_pid" = metadata_pid,
#                 "metadata_created" = TRUE))
#   }
#
#   data_pids <- c(data_pids, data_pid)
# }
#
# # Generate a resource map
# # Create the resource map
# # Create sysmeta for the resource map
# resource_map_filepath <- create_resource_map(metadata_pid, data_pids, child_pids)
# resource_map_size_bytes <- file.info(resource_map_filepath)$size
# resource_map_checksum <- digest::digest(resource_map_filepath, algo = "sha256")
# resource_map_pid <- paste0("resourceMap_", metadata_pid)
#
# resource_map_values <- data.frame("format_id" = "http://www.openarchives.org/ore/terms",
#                                   "size_bytes" = resource_map_size_bytes,
#                                   "checksum_sha256" = resource_map_checksum,
#                                   "file" = paste0(resource_map_pid, ".xml"),
#                                   stringsAsFactors = FALSE)
#
# resource_map_sysmeta <- create_sysmeta(resource_map_pid,
#                                        resource_map_values[1,],
#                                        me,
#                                        rh)
#
# response <- tryCatch(
#   {
#     dataone::create(mn,
#                     resource_map_pid,
#                     file = resource_map_filepath,
#                     sysmeta = resource_map_sysmeta)
#   },
#   warning = function(w) {
#     cat(paste0("Warning generated while calling MNStorage.create() on Resource Map for package ", package, ".\n"))
#   },
#   error = function(e) {
#     cat(paste0("Error generated while calling MNStorage.create() on Resource Map for package ", package, ".\n"))
#   })
#
# # TODO Log response
#
# # Parse the response
# stopifnot(!is.null(response))
#
# # Return the results of this call to the function
# #
# # We return all successfully created PIDs so we don't mint them again
# # We return whether or not each metadata or data object was created
# # successfully so we can know whether to insert it next time (if we have to
# # run insert_package on the same package multiple times)
#
# list("metadata_pid" = metadata_pid,
#      "metadata_created" = TRUE,
#      "data_pids" = data_pids,
#      "data_created" = rep(TRUE, length(data_pids)))
# }


#' Create a Resource Map. This is a convenience wrapper around the constructor
#' of the `ResourceMap` class from `DataPackage`.
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
                           data.frame(subject = paste0(resolve_base, URLencode(metadata_pid)),
                                      predicate = "http://purl.org/spar/cito/documents",
                                      object = paste0(resolve_base, URLencode(data_pid)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))

    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base, URLencode(data_pid)),
                                      predicate = "http://purl.org/spar/cito/isDocumentedBy",
                                      object = paste0(resolve_base, URLencode(metadata_pid)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))
  }

  resource_map <- datapackage::createFromTriples(new("ResourceMap", id = paste0("resourceMap_", metadata_pid)),
                                                 relations = relationships,
                                                 identifiers = c(metadata_pid, data_pids))
  outfilepath <- tempfile()
  stopifnot(!file.exists(outfilepath))

  datapackage::serializeRDF(resource_map, outfilepath)

  outfilepath
}

get_or_create_pid <- function(file, mn, scheme="UUID") {
  cat(paste0("get_or_create_pid()\n"))

  stopifnot(is.data.frame(file),
            nrow(file) == 1,
            "pid" %in% names(file))

  # Get the value of the PID
  pid <- file[1,"pid"]

  # Check if the existing PID is a valid one
  if (is.character(pid) && nchar(pid) > 0) {
    cat(paste0("Using alreayd-minted PID of ", pid, "\n"))
    return(pid)
  }

  cat(paste0("Minting new PID...\n"))

  # Try to generate a new pid with generateIdentifier()
  pid <- NA

  pid <- tryCatch(
    {
      dataone::generateIdentifier(mn, scheme)
    },
    error = function(e) {
      # Do nothing
    }
  )

  # Return `pid`, whch is either NA or a PID at this point
  pid
}




create_sysmeta <- function(file, base_path, submitter, rights_holder) {
  cat(paste0("create_sysmeta()\n"))

  stopifnot(is.data.frame(file),
            nrow(file) == 1)

  stopifnot(is.character(base_path),
            nchar(base_path) > 0)

  path_on_disk <- paste0(base_path, file[1,"file"])
  stopifnot(file.exists(path_on_disk))

  # Get the PID
  pid <- file[1,"pid"]
  stopifnot(is.character(pid),
            nchar(pid) > 0)

  # Get other parameters for the System Metadata
  format_id <- file[1,"format_id"]
  size <- file[1,"size_bytes"]
  checksum <- file[1,"checksum_sha256"]
  checksum_algorithm <- "SHA256"
  file_name <- file[1,"filename"]


  sysmeta <- NULL
  sysmeta <- tryCatch(
    {
      x <- new("SystemMetadata",
               identifier = pid,
               formatId = format_id,
               size = size,
               checksum = checksum,
               checksumAlgorithm = checksum_algorithm,
               submitter = submitter,
               rightsHolder = rights_holder,
               fileName = file_name)


      x <- datapackage::addAccessRule(x, "public", "read")
      x <- datapackage::addAccessRule(x, submitter, "write")
      x <- datapackage::addAccessRule(x, submitter, "changePermission")

      x
    },
    warning = function(w) {
      cat(paste0("Warning generated during the call to create_sysmeta() for the metadata file ", file[1,"file"], "\n"))
    },
    error = function(e) {
      cat(paste0("Error generated during the call to create_sysmeta() for the metadata file ", file[1,"file"], "\n"))
      cat(as.character(e))
    }
  )

  sysmeta
}





create_object <- function(file, sysmeta, base_path, mn) {
  cat(paste0("create_object()\n"))

  stopifnot(is.data.frame(file),
            nrow(file) == 1,
            "pid" %in% names(file),
            "file" %in% names(file))

  stopifnot(class(sysmeta) == "SystemMetadata")

  stopifnot(is.character(base_path),
            nchar(base_path) > 0)

  stopifnot(class(mn) == "MNode")

  # Set the return value to FALSE by default
  result <- FALSE

  # Get the PID
  pid <- file[1,"pid"]
  path_on_disk <- paste0(base_path, file[1,"file"])

  # Run the create() call
  response <- NULL
  response <- tryCatch(
    {
      dataone::create(mn,
                      pid,
                      file = path_on_disk,
                      sysmeta = sysmeta)
    },
    warning = function(w) {
      cat(paste0("Warning generated during the call to MNStorage.create() for the metadata file ", file[1,"file"], "\n"))
    },
    error = function(e) {
      cat(paste0("Error generated during the call to MNStorage.create() for the metadata file ", file[1,"file"], "\n"))
      cat(as.character(e))
    })

  # Validate the result
  # We use the XML package to convert the response to a list which just returns
  # a string with the PID when we successfully created the object.

  print(response)

  created_pid <- XML::xmlToList(response)

  if (is.character(created_pid) && nchar(created_pid) > 0) {
    result <- TRUE
  } else {
    result <- FALSE
  }

  print(result)
  result


}



