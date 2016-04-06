#' package.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Code related to inserting datasets as Data Packages.



#' Insert a file from a single row of the Inventory.
#'
#' @param inventory An Inventory (data.frame)
#' @param file The fully-qualified relative path to the file. See examples.
#' @param env (Optional) Specify an environment (list)
#'
#' @example insert_file(my_inv, "./acadis-gateway/project/A/iso.xml")
insert_file <- function(inventory, file, env=NULL) {
  validate_inventory(inventory)
  stopifnot(is.character(file), nchar(file) > 0, file %in% inventory$file)

  # Configuration
  if (is.null(env)) {
    env <- env_load("etc/environment.yml")
    library(dataone)
    env$mn <- dataone::MNode(env$mn_base_url)
  }

  validate_environment(env)

  if (is_token_expired()) {
    log_message("Token is expired. Returning un-modified inventory.")
    return(inventory)
  }

  # Find the file
  inventory_file <- inventory[inventory$file == file,]
  stopifnot(nrow(inventory_file) == 1)

  # Determine the identifier scheme to use
  if (inventory_file$is_metadata == TRUE) {
    identifier_scheme <- env$metadata_identifier_scheme
  } else {
    identifier_scheme <- env$data_identifier_scheme
  }

  log_message(paste0("Using identifier scheme ", identifier_scheme, "."))

  # Determine the PID to use
  inventory_file[1,"pid"] <- get_or_create_pid(inventory_file[1,],
                                               env$mn,
                                               scheme = identifier_scheme)

  if (is.na(inventory_file[1,"pid"])) {
    log_message(paste0("PID was NA for file ", file, ".\n"))
    return(inventory_file)
  }

  # System Metadata
  sysmeta <- create_sysmeta(inventory_file[1,],
                            env$base_path,
                            env$submitter,
                            env$rights_holder)

  if (is.null(sysmeta)) {
    log_message(paste0("System Metadata creation failed for file ", file, ".\n"))
    return(inventory_file)
  }

  if (inventory_file[1,"created"] == FALSE) {
    inventory_file[1,"created"] <- create_object(inventory_file[1,],
                                                 sysmeta,
                                                 env$base_path,
                                                 mn)
  }

  if (inventory_file[1,"created"] == FALSE) {
    log_message(paste0("Object creation failed for file ", file, ".\n"))
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

insert_package <- function(inventory, package, env=NULL) {
  validate_inventory(inventory)
  stopifnot(is.character(package), nchar(package) > 0, package %in% inventory$package)

  # Configuration
  if (is.null(env)) {
    env <- env_load("etc/environment.yml")
    library(dataone)
    env$mn <- dataone::MNode(env$mn_base_url)
  }

  validate_environment(env)

  if (is_token_expired()) {
    log_message("Token is expired. Returning un-modified inventory.")
    return(inventory)
  }

  # Check that any packages with this package as a parent package have
  # resource map identifiers
  child_packages <- inventory[inventory$parent_package == package &
                                inventory$is_metadata,]

  if (!all(child_packages$pid != "") && !(all(child_packages$created == TRUE))) {
    stop("Not all child packages have been created. Add those packages first.")
  }


  # Gather child pids
  if (nrow(child_packages) > 0) {
    child_pids <- vapply(child_packages$pid, generate_resource_map_pid, "")
  }
  else {
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


  # Process metadata if it hasn't already been created
  if (files[files_idx_metadata,"created"] == FALSE) {
    # Determine the PID to use for the metadata
    files[files_idx_metadata,"pid"] <- get_or_create_pid(files[files_idx_metadata,],
                                                         env$mn,
                                                         scheme = env$metadata_identifier_scheme)

    if (is.na(files[files_idx_metadata,"pid"])) {
      log_message(paste0("Metadata PID was NA for package ", package, ".\n"))
      return(files)
    }

    # Metadata SystemMetadata
    metadata_sysmeta <- create_sysmeta(files[files_idx_metadata,],
                                       env$base_path,
                                       env$submitter,
                                       env$rights_holder)

    if (is.null(metadata_sysmeta)) {
      log_message(paste0("System Metadata creation failed for metadata object in package ", package, ".\n"))
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
                                                           env$base_path,
                                                           env$mn)
    }

    if (files[files_idx_metadata,"created"] == FALSE) {
      log_message(paste0("Object creation failed for metadata object in package ", package, ".\n"))
      return(files)
    }
  } else {
    log_message("Skipped creating metadata because it was already created.")
  }

  # Insert data files if needed
  if (any(files[files_idx_data,"created"] == FALSE)) {

    for (data_idx in files_idx_data) {
      # Skip if already created
      if (files[data_idx,"created"] == TRUE) {
        log_message(paste0("File ", files[data_idx,"filename"], " in package ", package, " already created. Moving on to the next data object.\n"))
        next
      }

      log_message(paste0("Processing data index ", data_idx, " in package ", package, "\n"))

      # Determine the PID to use for the data
      files[data_idx,"pid"] <- get_or_create_pid(files[data_idx,],
                                                 env$mn,
                                                 scheme = env$data_identifier_scheme)

      if (is.na(files[data_idx,"pid"])) {
        log_message(paste0("Data PID was NA for file ", files[data_idx,'filename'], " in package ", package, ". Stopping early.\n"))
        return(files)
      }

      # Metadata SystemMetadata
      data_sysmeta <- create_sysmeta(files[data_idx,],
                                     env$base_path,
                                     env$submitter,
                                     env$rights_holder)

      if (is.null(metadata_sysmeta)) {
        log_message(paste0("System Metadata creation failed for metadata object in package ", package, ".\n"))
        return(files)
      }

      # Metadata Object
      if (files[data_idx,"created"] == FALSE) {
        files[data_idx,"created"] <- create_object(files[data_idx,],
                                                   data_sysmeta,
                                                   env$base_path,
                                                   env$mn)
      }

      if (files[data_idx,"created"] == FALSE) {
        log_message(paste0("Object creation failed for metadata object in package ", package, ".\n"))
        return(files)
      }
    }
  } else {
    log_message("Skipped creating data files because they were all created.")
  }

  # At this point, all of the metadata and data should be created, let's check
  if (!all(is.character(files[,"pid"])) && !all(files[,"created"] == TRUE)) {
    log_message(paste0("Not all files in package ", package, " have PIDs and are created. Skipping Resource Map creation.\n"))
    return(files)
  }

  # Generate and create() the Resource Map
  log_message(paste0("Generating resource map for package ", package, ".\n"))
  resource_map_pid <- generate_resource_map_pid(files[files_idx_metadata,"pid"])
  resource_map_filepath <- generate_resource_map(files[files_idx_metadata,"pid"],
                                                 files[files_idx_data,"pid"],
                                                 child_pids)

  log_message(paste0("Resource map PID is ", resource_map_pid, " for package with metadata file ", files[files_idx_metadata,"file"], ".\n"))

  resource_map_format_id <- "http://www.openarchives.org/ore/terms"
  resource_map_checksum <- digest::digest(resource_map_filepath, algo = "sha256")
  resource_map_size_bytes <- file.info(resource_map_filepath)$size
  resource_map_file_name <- paste0(stringr::str_replace_all(resource_map_pid, ":", "_"), ".xml")

  log_message(paste0("Generating system metadata for resource map for package ", package, ".\n"))
  resource_map_sysmeta <- new("SystemMetadata",
                              identifier = resource_map_pid,
                              formatId = resource_map_format_id,
                              size = resource_map_size_bytes,
                              checksum = resource_map_checksum,
                              checksumAlgorithm = "SHA256",
                              submitter = env$submitter,
                              rightsHolder = env$rights_holder,
                              fileName = resource_map_file_name)

  resource_map_sysmeta <- add_access_rules(resource_map_sysmeta)

  log_message(paste0("Creating resource map for package ", package, ".\n"))
  create_resource_map_response <- NULL
  create_resource_map_response <- tryCatch({
    dataone::createObject(env$mn,
                          resource_map_pid,
                          file = resource_map_filepath,
                          sysmeta = resource_map_sysmeta)
  },
  error = function(e) {
    log_message(paste0("Error encountered while calling create() on the Resource Map for package ", package, ".\n"))
    log_message(e$message)
    e
  }
  )

  if (inherits(create_resource_map_response, "error")) {
    created_resource_map_pid <- NULL
    files$resmap_created <- FALSE
  }
  else {
    print(create_resource_map_response)
    created_resource_map_pid <- XML::xmlToList(create_resource_map_response)
    files$resmap_created <- TRUE
  }

  log_message(print(files))

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
  stopifnot(length(metadata_pid) + (length(data_pids) + length(child_pids)) >= 1)

  # Validate the vector of child PIDs
  if (!is.character(child_pids)) {
    child_pids <- c()
  }

  relationships <- data.frame()

  # Add special statements to try and get metadata-only PIDs to index
  # Here we add that that the metadata document documents itself which is hacky
  relationships <- rbind(relationships,
                         data.frame(subject = paste0(resolve_base,"/", URLencode(metadata_pid, reserved = TRUE)),
                                    predicate = "http://purl.org/spar/cito/documents",
                                    object = paste0(resolve_base, "/", URLencode(metadata_pid, reserved = TRUE)),
                                    subjectType = "uri",
                                    objectType = "uri",
                                    stringsAsFactors = FALSE))

  relationships <- rbind(relationships,
                         data.frame(subject = paste0(resolve_base,"/", URLencode(metadata_pid, reserved = TRUE)),
                                    predicate = "http://purl.org/spar/cito/isDocumentedBy",
                                    object = paste0(resolve_base, "/", URLencode(metadata_pid, reserved = TRUE)),
                                    subjectType = "uri",
                                    objectType = "uri",
                                    stringsAsFactors = FALSE))

  # Add metadata -> documents -> data statements (and their inverse)
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

  # Add #aggregation aggregates/documenents child resource maps statements
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

  resource_map <- new("ResourceMap",
                      id = generate_resource_map_pid(metadata_pid))

  log_message(paste0("Creating resource map with pids ", paste0(unlist(c(metadata_pid, data_pids, child_pids)), collapse = ", ")))
  resource_map <- datapack::createFromTriples(resource_map,
                                              relations = relationships,
                                              identifiers = unlist(c(metadata_pid, data_pids, child_pids)),
                                              resolveURI = resolve_base)

  # Save the resource map to disk
  outfilepath <- tempfile()
  stopifnot(!file.exists(outfilepath))

  datapack::serializeRDF(resource_map, outfilepath)

  # Clean up after ourselves
  datapack::freeResourceMap(resource_map)
  rm(resource_map)

  # Return the full filepath to the resource map so calling functions can
  # reference it
  outfilepath
}

generate_resource_map_pid <- function(metadata_pid) {
  stopifnot(is.character(metadata_pid),
            nchar(metadata_pid) > 0)

  if (stringi::stri_startswith_fixed(metadata_pid, "resource_map_")) {
    return(metadata_pid)
  }

  paste0("resource_map_", metadata_pid)
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
  stopifnot(is.data.frame(file),
            nrow(file) == 1,
            "pid" %in% names(file))

  # Get the value of the PID
  pid <- file[1,"pid"]

  # Check if the existing PID is a valid one
  if (!is.na(pid) && is.character(pid) && nchar(pid) > 0) {
    log_message(paste0("Using existing PID of ", pid, "\n"))
    return(pid)
  }

  log_message(paste0("Minting new PID with scheme ", scheme, "\n"))

  if (scheme == "UUID") {
    pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  }

  else {
    pid <- tryCatch(
      {
        dataone::generateIdentifier(mn, scheme)
      },
      error = function(e) {
        log_message(paste0("Error generating identifier for file ", file[1,"file"], ".\n"))
        log_message(e$message)
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


      add_access_rules(x)

    },
    error = function(e) {
      log_message(paste0("Error generated during the call to create_sysmeta() for the metadata file ", file[1,"file"], "\n"))
      log_message(e$message)
      e
    }
  )

  # Return NULL instead of a dataone::SystemMetadata
  if (inherits(sysmeta, "error")) {
    return(NULL)
  }

  sysmeta
}


#' Add access rules to the sysmeta object
#'
#' This is a function because I add a set of standard set of access rules to
#' every object and the access rules don't differ across objects.
#'
#' @param sysmeta The SystemMetadata to add rules to (SystemMetadata)
#'
#' @return The modified SystemMetadata object
#' @export
#'
#' @examples
add_access_rules <- function(sysmeta) {
  if (!inherits(sysmeta, "SystemMetadata")) {
    log_message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
    return(sysmeta)
  }

  # Add myself explicitly as changePermission/write so I can update objects
  # in the dev environment
  if (env_get() == "development") {
    sysmeta <- datapack::addAccessRule(sysmeta, env$submitter, "changePermission")
  }

  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
  sysmeta <- datapack::addAccessRule(sysmeta, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")

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
  stopifnot(is.data.frame(file),
            nrow(file) == 1,
            "pid" %in% names(file),
            "file" %in% names(file))

  stopifnot(class(sysmeta) == "SystemMetadata")

  stopifnot(is.character(base_path),
            nchar(base_path) > 0)

  stopifnot(class(env$mn) == "MNode")

  # Set the return value to FALSE by default
  result <- FALSE

  # Get the PID
  pid <- file[1,"pid"]
  path_on_disk <- paste0(base_path, file[1,"file"])

  # Save time and file size so we can determine insert rate
  before_time <- Sys.time()
  file_size_mb <- file[1,"size_bytes"] / 1024 / 1024

  # Run the create() call
  response <- NULL
  response <- tryCatch(
    {
      dataone::createObject(env$mn,
                            pid,
                            file = path_on_disk,
                            sysmeta = sysmeta)
    },
    error = function(e) {
      log_message(paste0("Error generated during the call to MNStorage.create() for the metadata file ", file[1,"file"], "\n"))
      log_message(e$message)
      e
    })

  # Validate the result
  # We use the XML package to convert the response to a list which just returns
  # a string with the PID when we successfully created the object.

  if (inherits(response, "error")) {
    return(FALSE)
  }

  # Print out the insert rate
  time_diff_sec <- round(as.numeric(Sys.time() - before_time, "secs"), 2)
  mb_per_s <- round(file_size_mb / time_diff_sec, 2)
  log_message(paste0("Inserted ", file_size_mb, " MB in ", time_diff_sec, " s (", mb_per_s, " MB/s)\n"))

  created_pid <- XML::xmlToList(response)

  if (is.character(created_pid) && nchar(created_pid) > 0) {
    result <- TRUE
    log_message(paste0("Successfully created object with PID ", created_pid, " for file ", file[1,"file"], ".\n"))
  } else {
    result <- FALSE
    log_message(paste0("Failed to created object with PID ", created_pid, " for file ", file[1,"file"], ".\n"))
  }

  result
}


validate_inventory <- function(inventory) {
  stopifnot(is.data.frame(inventory),
            nrow(inventory) > 0,
            all(c("file",
                  "checksum_sha256",
                  "size_bytes",
                  "package",
                  "parent_package",
                  "pid",
                  "filename",
                  "created",
                  "ready") %in% names(inventory)))
}

validate_environment <- function(env) {
  env_default_components <- c("base_path",
                              "alternate_path",
                              "metadata_identifier_scheme",
                              "data_identifier_scheme",
                              "mn_base_url",
                              "submitter",
                              "rights_holder")
  stopifnot(class(env) == "list",
            length(env) > 0)
  stopifnot(!is.null(env), length(env) > 0)
  stopifnot(all(env_default_components %in% names(env)))
  stopifnot(all(unlist(lapply(env[env_default_components], nchar)) > 0))

  invisible(TRUE)
}


is_token_expired <- function() {
  # Check for presence of the token in options()
  if (is.null(options("authentication_token"))) {
    log_message("Authentication token not set in options().")
    return(FALSE)
  }

  token_info <- try({
    dataone::getTokenInfo(dataone::AuthenticationManager())
  })

  if (inherits(token_info, "try-error") ||
      !is.data.frame(token_info) ||
      !("expired" %in% names(token_info))) {
    log_message("Failed to get token info.")
    return(FALSE)
  }

  if (token_info$expired == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


determine_child_pids <- function(inventory, package) {
  stopifnot(all(c("package", "parent_package", "is_metadata") %in% names(inventory)))

  child_packages <- inventory[inventory$parent_package == package &
                                inventory$is_metadata,]

  # Gather child pids
  if (nrow(child_packages) > 0) {
    child_pids <- vapply(child_packages$pid, generate_resource_map_pid, "")
  } else {
    child_pids <- c()
  }

  names(child_pids) <- NULL
  child_pids
}


#' Update a package with modified metadata.
#'
#' The modified metadata should be set in the `env` variable. For example, if
#' your original metadata is:
#'
#' /home/you/originals/dir/a.xml
#'
#' and your modified metadata is in
#'
#' /home/someone_else/modified/dir/a.xml
#'
#' Then your env should be:
#'
#' env$base_path <- "/home/you/"
#' env$alternate_path <- "/home/someone_else"
#'
#' Note that the data files are not updated either so all that's happening is
#' the metadata object and resource map are being updated.
#'
#' Note that this function checks if the old objects (metadata and resource map)
#' exist on the Member Node before doing their work and will call createObject()
#' instead of updateObject() if the object didn't already exist.
#'
#' @param inventory (data.frame)
#' @param package (character)
#'
#' @return TRUE or FALSE depending on sucess (logical)
#' @export
#'
#' @examples
update_package <- function(inventory,
                           package,
                           env = NULL) {
  validate_inventory(inventory)
  stopifnot("pid_old" %in% names(inventory))
  stopifnot(is.character(package),
            nchar(package) > 0)

  # Grab the env if one wasn't passed in explicitly
  if (is.null(env)) {
    env <- env_load("etc/environment.yml")
    library(dataone)
    env$mn <- MNode(env$mn_base_url)
  }

  stopifnot(!is.null(env))

  # Subset the inventory to just files for this package
  package_files <- inventory[inventory$package == package,]
  stopifnot(nrow(package_files) > 0)

  # Check the token
  if (is_token_expired()) {
    log_message("Token is expired. Returning un-modified inventory.")
    return(package_files)
  }

  # Grab the metadata and data indices for later use
  metadata_file_idx <- which(package_files$is_metadata == TRUE)
  data_file_idx <- which(package_files$is_metadata == FALSE)
  stopifnot(length(metadata_file_idx) == 1)

  log_message(paste0("Updating package ", package, "\n"))

  # Find the converted EML documente
  if (!file.exists(env$alternate_path)) {
    log_message(paste0("Alternate path location of ", env$alternate_path, " does not exist. Returning.\n"))
    return(package_files)
  }

  eml_file_path <- path_join(c(env$alternate_path, package_files[metadata_file_idx,"file"]))

  if (!file.exists(eml_file_path)) {
    log_message(paste0("EML file not found at path ", eml_file_path, ".\n"))
    return(package_files)
  }

  log_message(paste0("Converted document is at ", eml_file_path, "\n"))

  # Get a new PID and replace the packageId
  new_pid <- package_files[metadata_file_idx,"pid"]
  old_pid <- package_files[metadata_file_idx,"pid_old"]

  log_message(paste0("Updating object with old PID ", old_pid, " with new PID ", new_pid, ".\n"))

  stopifnot(!is.na(new_pid),
            is.character(new_pid),
            nchar(new_pid) > 0)

  # Update the 'packageId' attribute on the root element with the new PID
  tmp <- tempfile()
  file.copy(eml_file_path, tmp)
  replace_package_id(tmp, new_pid)
  eml_file_path <- tmp

  # Generate a new filename
  new_metadata_file_name <- "science_metadata.xml"

  # Call MNStorage.update on the metadata object
  # Does this PID even exist? Stop now if it doesn't.
  if (!object_exists(env$mn_base_url, old_pid)) {
    log_message(paste0("Object with PID ", old_pid, " not found. Returning package.\n"))
    return(package_files)
  }

  # Update the object if it doesn't exist on the MN
  log_message(paste0("Checking if metadata object with pid ", new_pid, " already exists.\n"))

  if (!object_exists(env$mn_base_url, new_pid)) {
    sysmeta <- new("SystemMetadata",
                   identifier = new_pid,
                   formatId = "eml://ecoinformatics.org/eml-2.1.1",
                   size = file.size(eml_file_path),
                   checksum = digest::digest(eml_file_path, algo = "sha256"),
                   checksumAlgorithm = "SHA256",
                   submitter = env$submitter,
                   rightsHolder = env$rights_holder,
                   fileName = new_metadata_file_name)

    sysmeta <- add_access_rules(sysmeta)

    update_response <- tryCatch({
      dataone::updateObject(x = env$mn,
                            pid = old_pid,
                            file = eml_file_path,
                            newpid = new_pid,
                            sysmeta = sysmeta)
    },
    error = function(e) {
      log_message(paste0("Error produced during call to updateObject for metadata ", package_files[metadata_file_idx,"file"], " in package ", package, "\n"))
      log_message(e)
      e
    })

    if (inherits(update_response, "error")) {
      package_files$resmap_created <- FALSE
      return(package_files)
    }

    # Set the updated flag to TRUE
    package_files$updated <- TRUE

    log_message(paste0("Inserted updated metadata object for package ", package, "\n"))
  }
  else {
    log_message(paste0("Metadata object already exists. Moving on to the resource map.\n"))
  }



  # Resource Map
  metadata_pid <- package_files[metadata_file_idx,"pid"]
  resource_map_pid <- generate_resource_map_pid(metadata_pid)

  data_pids <- package_files[data_file_idx,"pid"]
  child_pids <- determine_child_pids(inventory, package)

  resource_map_filepath <- generate_resource_map(metadata_pid, data_pids, child_pids)
  resource_map_format_id <- "http://www.openarchives.org/ore/terms"
  resource_map_checksum <- digest::digest(resource_map_filepath, algo = "sha256")
  resource_map_size_bytes <- file.size(resource_map_filepath)
  resource_map_file_name <- paste0(stringr::str_replace_all(resource_map_pid, ":", "_"), ".xml")

  resource_map_sysmeta <- new("SystemMetadata",
                              identifier = resource_map_pid,
                              formatId = resource_map_format_id,
                              size = resource_map_size_bytes,
                              checksum = resource_map_checksum,
                              checksumAlgorithm = "SHA256",
                              submitter = env$submitter,
                              rightsHolder = env$rights_holder,
                              fileName = resource_map_file_name)

  resource_map_sysmeta <- add_access_rules(resource_map_sysmeta)
  old_resmap_pid <- generate_resource_map_pid(old_pid)




  # Cases...
  # 1. Old resource map doesn't exist
  #    then just create the new one if it doesn't exist
  # 2. Old resource map does exist
  #    then update it with the new one if it doesn't exist

  # Check if the new resource map already exists

  if (object_exists(env$mn_base_url, resource_map_pid)) {
    log_message(paste0("The new resource map with PID ", resource_map_pid, " already exists. Finishing up.\n"))
  } else {
    # Now check if the OLD resource map exists

    if (!object_exists(env$mn_base_url, old_resmap_pid)) {
      log_message(paste0("Old resource map with PID ", resource_map_pid, " doesn't exist. Using createObject instead of updateObject.\n"))

      create_response <- tryCatch({
        dataone::createObject(x = env$mn,
                              pid = resource_map_pid,
                              file = resource_map_filepath,
                              sysmeta = resource_map_sysmeta)
      },
      error = function(e) {
        log_message(paste0("Error produced during call to createObject for resource map ", resource_map_pid, " in package ", package, "\n"))
        log_message(e)
        e
      })

      if (inherits(create_response, "error")) {
        log_message("There was an error calling createObject. Returning package files as is.\n")
        return(package_files)
      }

      log_message(create_response)
    } else {
      # Update the old resource map
      update_response <- tryCatch({
        dataone::updateObject(x = env$mn,
                              pid = old_resmap_pid,
                              file = resource_map_filepath,
                              newpid = resource_map_pid,
                              sysmeta = resource_map_sysmeta)
      },
      error = function(e) {
        log_message(paste0("Error produced during call to updateObject for resource map ", package_files[metadata_file_idx,"file"], " in package ", package, "\n"))
        log_message(e)
        e
      })

      if (inherits(update_response, "error")) {
        log_message("There was an error calling updateObject Returning package files as is.\n")
        return(package_files)
      }

      log_message(update_response)
    }
  }

  package_files
}

