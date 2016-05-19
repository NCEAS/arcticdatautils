#' helpers.R
#'
#' Various helper functions for things like testing the package.

create_dummy_metadata <- function(mn) {
  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  me <- get_token_subject()
  the_file <- file.path(system.file("tests/data/example-eml.xml", package = "arcticdatautils"))

  sysmeta <- new("SystemMetadata",
                 id = pid,
                 formatId = "eml://ecoinformatics.org/eml-2.1.1",
                 size = file.size(the_file),
                 checksum = digest::digest(the_file, algo = "sha256"),
                 checksumAlgorithm = "SHA256",
                 submitter = me,
                 rightsHolder = me)
  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  log_message(paste0("Creating metadata ", pid))
  dataone::createObject(mn, pid, the_file, sysmeta)

  pid
}

create_dummy_object <- function(mn) {
  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  me <- get_token_subject()
  tmp <- tempfile()

  writeLines(paste0(sample(LETTERS, 26, replace = TRUE), collapse = ""), con = tmp)

  sysmeta <- new("SystemMetadata",
                 id = pid,
                 formatId = "application/octet-stream",
                 size = file.size(tmp),
                 checksum = digest::digest(tmp, algo = "sha256"),
                 checksumAlgorithm = "SHA256",
                 submitter = me,
                 rightsHolder = me)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  log_message(paste0("Creating object ", pid))
  dataone::createObject(mn, pid, tmp, sysmeta)

  file.remove(tmp)

  pid
}

create_dummy_package <- function(mn, size = 2) {
  me <- get_token_subject()
  meta_pid <- create_dummy_metadata(mn)
  data_pids <- sapply(1:(size - 1), function(i) {
    create_dummy_object(mn)
  })

  # Filter NA pids (failed creates)
  data_pids <- data_pids[!is.na(data_pids)]

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  resmap_path <- generate_resource_map(meta_pid,
                                       data_pids,
                                       resource_map_pid = pid)

  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = "http://www.openarchives.org/ore/terms",
                 size = file.size(resmap_path),
                 checksum = digest::digest(resmap_path, algo = "sha256"),
                 checksumAlgorithm = "SHA256",
                 submitter = me,
                 rightsHolder = me)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  log_message(paste0("Creating resource map ", pid))
  dataone::createObject(mn, pid, resmap_path, sysmeta)

  list(metadata = meta_pid,
       data = data_pids,
       resource_map = pid)
}

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
                 checksum = digest::digest(resmap_path, algo = "sha256"),
                 checksumAlgorithm = "SHA256",
                 submitter = me,
                 rightsHolder = me)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  log_message(paste0("Creating parent package map ", pid))
  createObject(mn, pid, resmap_path, sysmeta)

  list(parent = pid,
       children = children)
}
