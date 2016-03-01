#' package.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Code related to inserting datasets as Data Packages.



insert_file <- function(inventory, file) {
  stopifnot(is.data.frame(inventory),
            nrow(inventory) > 0,
            all(c("file",
                  "checksum_sha256",
                  "size_bytes",
                  "pid",
                  "filename") %in% names(inventory)))
  stopifnot(is.character(file),
            nchar(file) > 0,
            file %in% inventory$file)

  # Configuration
  library(dataone)  # TODO Remove this library call once the package is fixed
  env <- env_load("etc/environment.yml")

  stopifnot(class(env) == "list",
            length(env) > 0)
  stopifnot(!is.null(env), length(env) > 0)
  stopifnot(all(c("base_directory",
                  "alternate_directory",
                  "metadata_identifier_scheme",
                  "data_identifier_scheme",
                  "mn",
                  "submitter",
                  "rights_holder") %in% names(env)))
  stopifnot(all(unlist(lapply(env, nchar)) > 0))

  mn <- MNode(env$mn)
  base_path <- env$base_directory
  alt_path <- env$alternate_directory
  metadata_identifier_scheme <- env$metadata_identifier_scheme
  data_identifier_scheme <- env$data_identifier_scheme
  submitter <- env$submitter
  rights_holder <- env$rights_holder

  # Don't do anything if we don't have a valid token
  am <- dataone::AuthenticationManager()
  auth_valid <- dataone:::isAuthValid(am, mn)

  if (auth_valid == FALSE) {
    cat(paste0("Authentication was not valid agaisnt member node: ", mn@endpoint, ". Returning early.\n"))
    return(data.frame())
  }

  # Find the file
  inventory_file <- inventory[inventory$file == file,]
  stopifnot(nrow(inventory_file) == 1)

  # Determine the identifier scheme to use
  if (inventory_file$is_metadata == TRUE) {
    identifier_scheme <- metadata_identifier_scheme
  } else {
    identifier_scheme <- data_identifier_scheme
  }

  # PID
  inventory_file[1,"pid"] <- get_or_create_pid(inventory_file[1,],
                                               mn,
                                               scheme = identifier_scheme)

  if (is.na(inventory_file[1,"pid"])) {
    cat(paste0("PID was NA for file ", file, ".\n"))
    return(inventory_file)
  }

  # System Metadat
  sysmeta <- create_sysmeta(inventory_file[1,],
                            base_path,
                            submitter,
                            rights_holder)

  if (is.null(sysmeta)) {
    cat(paste0("System Metadata creation failed for file ", file, ".\n"))
    return(inventory_file)
  }

  if (!("created" %in% names(inventory_file))) {
    inventory_file$created <- FALSE
  }

  if (inventory_file[1,"created"] == FALSE) {
    inventory_file[1,"created"] <- create_object(inventory_file[1,],
                                                 sysmeta,
                                                 base_path,
                                                 mn)
  }

  if (inventory_file[1,"created"] == FALSE) {
    cat(paste0("Object creation failed for file ", file, ".\n"))
    return(inventory_file)
  }

  return(inventory_file)
}


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

insert_package <- function(inventory, package) {
  stopifnot(is.data.frame(inventory),
            nrow(inventory) > 0,
            all(c("file", "checksum_sha256", "size_bytes", "package", "parent_package", "pid", "filename") %in% names(inventory)))

  stopifnot(is.character(package),
            package %in% inventory$package)


  # Configuration
  library(dataone)  # TODO Remove this library call once the package is fixed
  env <- env_load("etc/environment.yml")

  stopifnot(class(env) == "list",
            length(env) > 0)
  stopifnot(!is.null(env), length(env) > 0)
  stopifnot(all(c("base_directory",
                  "alternate_directory",
                  "metadata_identifier_scheme",
                  "data_identifier_scheme",
                  "mn",
                  "submitter",
                  "rights_holder") %in% names(env)))
  stopifnot(all(unlist(lapply(env, nchar)) > 0))

  mn <- MNode(env$mn)
  base_path <- env$base_directory
  alt_path <- env$alternate_directory
  metadata_identifier_scheme <- env$metadata_identifier_scheme
  data_identifier_scheme <- env$data_identifier_scheme
  submitter <- env$submitter
  rights_holder <- env$rights_holder

  stopifnot(class(mn) == "MNode",
            nchar(submitter) > 0,
            nchar(rights_holder) > 0,
            file.exists(base_path))

  # Don't do anything if we don't have a valid token
  am <- dataone::AuthenticationManager()
  auth_valid <- dataone:::isAuthValid(am, mn)

  if (auth_valid == FALSE) {
    cat(paste0("Authentication was not valid agaisnt member node: ", mn@endpoint, ". Returning early.\n"))
    return(data.frame())
  }

  # Check that any packages with this package as a parent package have
  # resource map identifiers
  child_packages <- inventory[inventory$parent_package == package &
                              inventory$is_metadata,]

  if (!all(child_packages$pid != "")) {
    stop("Not all child packages had PIDs already. Add those packages first.")
  }

  # Gather child pids
  if (nrow(child_packages) > 0) {
    child_pids <- vapply(child_packages$pid, generate_resource_map_pid, "")
  } else {
    child_pids <- c()
  }

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
  files[files_idx_metadata,"pid"] <- get_or_create_pid(files[files_idx_metadata,],
                                                       mn,
                                                       scheme = metadata_identifier_scheme)

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

  # We're about to use the 'created' column, initialize it if needed
  if (!("created" %in% names(files))) {
    files$created <- FALSE
  }

  # Metadata Object
  if (files[files_idx_metadata,"created"] == FALSE) {
    files[files_idx_metadata,"created"] <- create_object(files[files_idx_metadata,],
                                                         metadata_sysmeta,
                                                         base_path,
                                                         mn)
  }

  if (files[files_idx_metadata,"created"] == FALSE) {
    cat(paste0("Object creation failed for metadata object in package ", package, ".\n"))
    return(files)
  }


  for (data_idx in files_idx_data) {
    cat(paste0("Processing data index ", data_idx, " in package ", package, "\n"))

    files[data_idx,"pid"] <- get_or_create_pid(files[data_idx,],
                                               mn,
                                               scheme = data_identifier_scheme)

    if (is.na(files[data_idx,"pid"])) {
      cat(paste0("Data PID was NA for file ", files[data_idx,'filename'], " in package ", package, ". Stopping early.\n"))
      return(files)
    }

    # Metadata SystemMetadata
    data_sysmeta <- create_sysmeta(files[data_idx,],
                                   base_path,
                                   submitter,
                                   rights_holder)

    if (is.null(metadata_sysmeta)) {
      cat(paste0("System Metadata creation failed for metadata object in package ", package, ".\n"))
      return(files)
    }

    # Metadata Object
    if (files[data_idx,"created"] == FALSE) {
      files[data_idx,"created"] <- create_object(files[data_idx,],
                                                 data_sysmeta,
                                                 base_path,
                                                 mn)
    }

    if (files[data_idx,"created"] == FALSE) {
      cat(paste0("Object creation failed for metadata object in package ", package, ".\n"))
      return(files)
    }
  }

  # At this point, all of the metadata and data should be created, let's check
  if (!all(is.character(files[,"pid"])) && !all(files[,"created"] == TRUE)) {
    cat(paste0("Not all files in package ", package, " have PIDs and are created. Skipping Resource Map creation.\n"))
    print(files)
    return(files)
  }

  # Generate and create() the Resource Map
  cat(paste0("Generating resource map for package ", package, ".\n"))
  resource_map_pid <- generate_resource_map_pid(files[files_idx_metadata,"pid"])
  resource_map_filepath <- generate_resource_map(files[files_idx_metadata,"pid"],
                                                 files[files_idx_data,"pid"],
                                                 child_pids)

  cat(paste0("Resource map PID should be ", resource_map_pid, ".\n"))

  resource_map_format_id <- "http://www.openarchives.org/ore/terms"
  resource_map_checksum <- digest::digest(resource_map_filepath, algo = "sha256")
  resource_map_size_bytes <- file.info(resource_map_filepath)$size

  cat(paste0("Generating system metadata for resource map for package ", package, ".\n"))
  resource_map_sysmeta <- new("SystemMetadata",
                              identifier = resource_map_pid,
                              formatId = resource_map_format_id,
                              size = resource_map_size_bytes,
                              checksum = resource_map_checksum,
                              checksumAlgorithm = "SHA256",
                              submitter = submitter,
                              rightsHolder = rights_holder,
                              fileName = paste0(resource_map_pid, ".xml"))

  resource_map_sysmeta <- datapackage::addAccessRule(resource_map_sysmeta, "public", "read")
  resource_map_sysmeta <- datapackage::addAccessRule(resource_map_sysmeta, submitter, "write")
  resource_map_sysmeta <- datapackage::addAccessRule(resource_map_sysmeta, submitter, "changePermission")

  cat(paste0("Creating resource map for package ", package, ".\n"))
  create_resource_map_response <- NULL
  create_resource_map_response <- tryCatch(
    {
      dataone::create(mn,
                      resource_map_pid,
                      file = resource_map_filepath,
                      sysmeta = resource_map_sysmeta)
    },
    error = function(e) {
      cat(paste0("Error encountered while calling create() on the Resource Map for package ", package, ".\n"))
      print(e)

      e
    }
  )

  if (inherits(create_resource_map_response, "error")) {
    created_resource_map_pid <- NULL
    files$resmap_created <- FALSE
  } else {
    print(create_resource_map_response)
    created_resource_map_pid <- XML::xmlToList(create_resource_map_response)
    files$resmap_created <- TRUE
  }

  print(files)

  return(files)
}


#' Generate a Resource Map. This is a convenience wrapper around the constructor
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
generate_resource_map <- function(metadata_pid,
                                  data_pids,
                                  child_pids=c(),
                                  resolve_base="https://cn.dataone.org/cn/v2/resolve") {
  stopifnot(length(metadata_pid) == 1)
  stopifnot((length(data_pids) + length(child_pids)) >= 1)

  # Validate the vector of child PIDs
  if (!is.character(child_pids)) {
    child_pids <- c()
  }

  relationships <- data.frame()

  for (data_pid in data_pids) {
    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base,"/", URLencode(metadata_pid, reserved = TRUE)),
                                      predicate = "http://purl.org/spar/cito/documents",
                                      object = paste0(resolve_base, "/", URLencode(data_pid, reserved = TRUE)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))

    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base, "/", URLencode(data_pid, reserved = TRUE)),
                                      predicate = "http://purl.org/spar/cito/isDocumentedBy",
                                      object = paste0(resolve_base, "/", URLencode(metadata_pid, reserved = TRUE)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))
  }

  resource_map_pid <- generate_resource_map_pid(metadata_pid)

  for (child_pid in child_pids) {
    # aggregates <-> isAggregatedBy
    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base,"/", URLencode(resource_map_pid, reserved = TRUE), "#aggregation"),
                                      predicate = "http://www.openarchives.org/ore/terms/aggregates",
                                      object = paste0(resolve_base, "/", URLencode(child_pid, reserved = TRUE)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))

    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base, "/", URLencode(child_pid, reserved = TRUE)),
                                      predicate = "http://www.openarchives.org/ore/terms/isAggregatedBy",
                                      object = paste0(resolve_base, "/", URLencode(resource_map_pid, reserved = TRUE), "#aggregation"),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))

    # documents <-> isDocumentedBy
    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base,"/", URLencode(metadata_pid, reserved = TRUE)),
                                      predicate = "http://purl.org/spar/cito/documents",
                                      object = paste0(resolve_base, "/", URLencode(child_pid, reserved = TRUE)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))

    relationships <- rbind(relationships,
                           data.frame(subject = paste0(resolve_base, "/", URLencode(child_pid, reserved = TRUE)),
                                      predicate = "http://purl.org/spar/cito/isDocumentedBy",
                                      object = paste0(resolve_base, "/", URLencode(metadata_pid, reserved = TRUE)),
                                      subjectType = "uri",
                                      objectType = "uri",
                                      stringsAsFactors = FALSE))
  }

  resource_map <- datapackage::createFromTriples(new("ResourceMap",
                                                     id = generate_resource_map_pid(metadata_pid)),
                                                 relations = relationships,
                                                 identifiers = unlist(c(metadata_pid, data_pids, child_pids)),
                                                 resolveURI = resolve_base)
  outfilepath <- tempfile()
  stopifnot(!file.exists(outfilepath))

  datapackage::serializeRDF(resource_map, outfilepath)

  outfilepath
}


generate_resource_map_pid <- function(metadata_pid) {
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  if (stringi::stri_startswith_fixed(metadata_pid, "resourceMap")) {
    return(metadata_pid)
  }

  # paste0("resourceMap_",
  # stringr::str_replace(metadata_pid, "urn:uuid:", ""))
  paste0("resourceMap_", metadata_pid)
  # uuid::UUIDgenerate()
}

#' Get the already-minted PID from the inventory or mint a new one.
#'
#' @param file A single row from the inventory (data.frame)
#' @param mn The Member Node that will mint the new PID, if needed. (dataone::MNode)
#' @param scheme The identifier scheme to use. (character)
#'
#' @return The identifier (character)
#' @export
#'
#' @examples
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

  cat(paste0("Minting new PID with scheme ", scheme, "\n"))

  if (scheme == "UUID") {
    pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  } else {
    pid <- tryCatch(
      {
        dataone::generateIdentifier(mn, scheme)
      },
      error = function(e) {
        cat(paste0("Error generating identifier for file ", file[1,"file"], ".\n"))
        print(e)

        e
      }
    )
  }

  if (inherits(pid, "error")) {
    pid <- ""
  }

  # Return `pid`, whch is either "" or a PID at this point
  pid
}




#' Create a sysmeta object.
#'
#' This is a wrapper function around the constructor for a
#' dataone::SystemMetadata object.
#'
#' @param file file A single row from the inventory (data.frame)
#' @param base_path The path prefix to use with the contents of `file[1,"filename]` that
#' will be used to locate the file on disk. (character)
#' @param submitter The submitter DN string for the object. (character)
#' @param rights_holder The rights holder DN string for the object. (character)
#'
#' @return The sysmeta object (dataone::SystemMetadata)
#' @export
#'
#' @examples
create_sysmeta <- function(file, base_path, submitter, rights_holder) {
  cat(paste0("create_sysmeta()\n"))

  stopifnot(is.data.frame(file),
            nrow(file) == 1)

  stopifnot(is.character(base_path),
            nchar(base_path) > 0)

  path_on_disk <- paste0(base_path, file[1,"file"])
  stopifnot(file.exists(path_on_disk))

  # Get the PID
  pid <- as.character(file[1,"pid"])
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
    error = function(e) {
      cat(paste0("Error generated during the call to create_sysmeta() for the metadata file ", file[1,"file"], "\n"))
      print(e)

      e
    }
  )

  # Return NULL instead of a dataone::SystemMetadata
  if (inherits(sysmeta, "error")) {
    return(NULL)
  }

  sysmeta
}





#' Create an object from a row of the inventory.
#'
#' @param file A row from the inventory (data.frame)
#' @param sysmeta The file's sysmeta (dataone::SystemMetadata)
#' @param base_path
#' @param mn
#'
#' @return
#' @export
#'
#' @examples
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
    error = function(e) {
      cat(paste0("Error generated during the call to MNStorage.create() for the metadata file ", file[1,"file"], "\n"))
      print(e)

      e
    })

  # Validate the result
  # We use the XML package to convert the response to a list which just returns
  # a string with the PID when we successfully created the object.

  print(response)

  if (inherits(response, "error")) {
    return(FALSE)
  }

  created_pid <- XML::xmlToList(response)

  if (is.character(created_pid) && nchar(created_pid) > 0) {
    result <- TRUE
  } else {
    result <- FALSE
  }

  print(result)

  result
}
