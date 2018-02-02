#' inserting.R
#'
#' A set of utilities for inserting packages from files and folders on disk.


#' Create a package from a folder containing an ISO package (legacy)
#'
#' This function  handles the process of inserting the original ISO package
#' and updating it with an EML package.
#'
#' Note: This only works for Gateway packages right now.
#'
#' @param mn (MNode) The Member Node to create the packages on.
#' @param path (character) The path to the folder containing the files.
#' @param data_pids (character) Optional. Manually specify the PIDs of data. This is useful if data were inserted outside this function and you want to re-use those objects.
#'
#' @return (list) All of the PIDs created.
create_from_folder <- function(mn, path, data_pids=NULL) {
  # Validate args
  stopifnot(file.exists(path))

  file_paths <- dir(path, recursive = TRUE, full.names = TRUE)
  metadata_path <- file_paths[grepl("iso19139.xml", file_paths)]
  stopifnot(length(metadata_path) == 1)
  data_paths <- file_paths[!grepl("iso19139.xml", file_paths)]
  stopifnot(!(metadata_path %in% data_paths))

  # Precalculate format IDs
  data_format_ids <- guess_format_id(data_paths)
  names(data_format_ids) <- data_paths

  # Gather alternate identifiers
  alternate_identifiers <- c(extract_local_identifier("gateway", metadata_path),
                             basename(path))

  # Insert the data objects if needed
  if (!is.null(data_pids)) {
    stopifnot(length(data_paths) == length(data_pids))
  } else {
    data_pids <- c()

    for (data_path in data_paths) {
      data_pid <- tryCatch({
        publish_object(mn, data_path, data_format_ids[data_path])
      },
      error = function(e) {
        message(e)
        e
      })

      data_pids[data_path] <- data_pid
    }
  }

  # Insert the original ISO and its package
  iso_pid <- publish_object(mn, metadata_path, format_iso())
  iso_resmap_pid <- create_resource_map(mn, iso_pid, data_pids)

  # Convert the ISO to EML and add alternateIdentifiers
  eml_path <- convert_iso_to_eml(metadata_path)
  eml_path <- add_additional_identifiers(eml_path, alternate_identifiers)
  stopifnot(EML::eml_validate(eml_path))

  eml_package <- publish_update(mn,
                                metadata_old_pid = iso_pid,
                                resmap_old_pid = iso_resmap_pid,
                                data_old_pids = data_pids,
                                metadata_file_path = eml_path)

  list(iso_pid = iso_pid,
       iso_resource_map_pid = iso_resmap_pid,
       eml_pid = eml_package$metadata,
       eml_resource_map_pid = eml_package$resource_map,
       data_pids = data_pids)
}
