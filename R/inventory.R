#' inventory.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions relating to keeping up an inventory of files that exist on the KNB
#' and may or may not be copied to another computer and untarred.
#'


#' Create an empty inventory data.frame. This doesn't need to be a function
#' but I'm making it one in case the initialization routine becomes more
#' complicated.
#'
#' @return An empty data frame
#' @export
#'
#' @examples
#' inv_init()
inv_init <- function() {
  inventory <- data.frame(stringsAsFactors = FALSE)

  inventory
}

#' Load files into the inventory from a text file.
#'
#' Files should be the output of the command:
#'
#'   you@server:/path/to/acadis$ find . -type f
#'
#' @param path Path to a file containing a file listing.
#' @param inventory A \code{data.frame}.
#'
#' @return An inventory (data.frame)
#'
#' @examples
#' inv_load_files("some/file/path.txt", my_inventory)
inv_load_files <- function(path, inventory) {
  stopifnot(file.exists(path))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory))

  # Read the filenames from disk
  files <- read.delim(path,
                      col.names = c("file"),
                      header = FALSE,
                      stringsAsFactors = FALSE)
  stopifnot(is.data.frame(files))

  # Filter out files not under with 'acadis-field-projects' or 'acadis-gateway'
  # subfolders
  size_before <- nrow(files)

  files <- files[stringi::stri_startswith_fixed(files$file, "./acadis-field-projects/") |
                   stringi::stri_startswith_fixed(files$file, "./acadis-gateway/"), "file", drop = FALSE]

  size_diff <- size_before - nrow(files)
  if (size_diff > 0) { cat("Removed ", size_diff, "file(s) that weren't inside acadis-gateway or acadis-field-projects subfolders.\n") }

  # If inventory is empty, just make the inventory the same as filenames
  if (nrow(inventory) == 0) {
    return(files)
  }

  # Only append rows with new filenames
  #
  # Merging algorithm:
  #   Add colums we need to `filenames`
  #   Remove rows from `filenames` that exist in `inventory`
  #   Merge the two

  # Make `filenames` the same shape as `inventory` by appending columns
  for (col_name in names(inventory)) {
    if (col_name %in% names(files)) { next }
    files[,col_name] <- NA
  }

  stopifnot(identical(names(inventory), names(files)))


  # Remove intersections if we had any
  inter_vals <- intersect(inventory$file, files$file)

  if (length(inter_vals) > 0 ) {
    files <- subset(files, match(file, inter_vals, nomatch = 0) == 0)
  }

  inventory <- rbind(inventory,
                     files)

  inventory
}

#' Load file sizes into an inventory from a text file. Removes the column
#' 'size_bytes' from inventory before doing a left join.
#'
#' @param path Path to a file containing sizes.
#' @param inventory A \code{data.frame}.
#'
#' @return An inventory (data.frame)
#'
#' @examples
inv_load_sizes <- function(path, inventory) {
  stopifnot(file.exists(path))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory),
            "file" %in% names(inventory))

  # Read the sizes from disk
  sizes <- read.delim(path,
                      col.names = c("size_bytes", "file"),
                      stringsAsFactors = FALSE,
                      header = FALSE)

  stopifnot(is.data.frame(sizes))

  if (nrow(inventory) != nrow(sizes)) {
    warning(paste("Inventory and incoming 'sizes' data.frame not of the same number of rows.", nrow(inventory), "vs", nrow(sizes)))
  }

  # Join the sizes onto existing filenames in the inventory
  # First drop the existing sizes
  inventory <- inventory[,!(names(inventory) %in% "size_bytes"), drop = FALSE]
  inventory <- dplyr::left_join(inventory, sizes, by = "file")

  # Check the result
  if (any(is.na(inventory$file))) { warning("Some values in the 'filename' column were NA.")}
  if (any(is.na(inventory$size_bytes))) { warning("Some values in the 'size_bytes' column were NA.")}

  inventory
}

#' Load checksums into the inventory file from a text file. This function
#' removes the column 'checksum_sha256' from inventory before doing a
#' left join.
#'
#' @param path Path to a file containing sizes.
#' @param inventory An inventory (data.frame)
#'
#' @return An inventory (data.frame)
#'
#' @examples
inv_load_checksums <- function(path, inventory) {
  stopifnot(file.exists(path))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory),
            "file" %in% names(inventory))

  # Convert the text file to a TSV before reading and joining
  in_file <- readLines(path)
  in_file_withtabs <- gsub("  ", "\t", in_file)
  out_file <- tempfile()
  writeLines(in_file_withtabs, out_file)

  # Read in the file we made above
  checksums <- read.delim(out_file,
                          header = FALSE,
                          col.names = c("checksum_sha256", "file"),
                          sep = "\t",
                          stringsAsFactors = FALSE)

  stopifnot(is.data.frame(checksums))

  if (nrow(inventory) != nrow(checksums)) {
    warning(paste("Inventory and incoming 'checksums' data.frame not of the same number of rows.", nrow(inventory), "vs", nrow(checksums)))
  }

  # Join the checksums onto existing filenames in the inventory
  # First drop the existing checksums
  inventory <- inventory[,!(names(inventory) %in% "checksum_sha256"), drop = FALSE]
  inventory <- dplyr::left_join(inventory, checksums, by = "file")

  # Check the result
  if (any(is.na(inventory$file))) { warning("Some values in the 'file' column were NA.")}
  if (any(is.na(inventory$checksum_sha256))) { warning("Some values in the 'checksum_sha256' column were NA.")}

  inventory
}

#' Load identifiers into the inventory file(s) from a text file. This function
#' removes the column 'identifier' from inventory before doing a
#' left join.
#'
#' @param path Path(s) to files containing identifiers.
#' @param inventory An inventory (data.frame)
#'
#' @return An inventory (data.frame)
#'
#' @examples
inv_load_identifiers <- function(path, inventory) {
  stopifnot(file.exists(path))
  stopifnot(is.data.frame(inventory),
            "file" %in% names(inventory))

  identifiers <- data.frame()

  for (path in paths) {
    # Read the identifiers from disk
    filename_identifiers <- read.csv(path,
                                     col.names = c("file", "identifier"),
                                     header = TRUE,
                                     stringsAsFactors = FALSE)

    # rbind them
    identifiers <- rbind(identifiers,
                         filename_identifiers)
  }

  stopifnot(is.data.frame(identifiers))

  # Join the identifiers onto existing filenames in the inventory
  # First drop the existing identifiers
  inventory <- inventory[,!(names(inventory) %in% "identifier"), drop = FALSE]
  inventory <- dplyr::left_join(inventory, identifiers, by = "file")

  inventory
}

#' Adds a set of extra columsn to the inventory that are useful for working
#' with them.
#'
#' @param inventory An inventory (data.frame)
#'
#' @return An inventory (data.frame)
inv_add_extra_columns <- function(inventory) {
  stopifnot(class(inventory) == "data.frame", "file" %in% names(inventory))

  # Mark metadata files
  cat("Adding 'is_metadata' column.\n")
  inventory$is_metadata <- stringi::stri_endswith_fixed(inventory$file, "ISO.xml") |
    stringi::stri_endswith_fixed(inventory$file, "iso19139.xml")

  # Mark which root subfolder each file is under
  cat("Adding 'subfolder' column.\n")
  inventory$subfolder[stringi::stri_startswith_fixed(inventory$file, "./acadis-field-projects")] <- "FP"
  inventory$subfolder[stringi::stri_startswith_fixed(inventory$file, "./acadis-gateway")] <- "G"

  # Add a column with filename and folder paths
  cat("Adding 'folder' and 'filename' columns.\n")

  inventory$folder <- unlist(
    lapply(
      stringr::str_split(inventory$file, "/"),
      function(x) {
        paste(x[1:(length(x) - 1)], collapse = "/")
      }))

  inventory$filename <- unlist(
    lapply(
      stringr::str_split(inventory$file, "/"),
      function(x) {
        x[length(x)]
      }))

  # Add depth column
  cat("Adding 'depth' column.\n")
  inventory$depth <- unlist(
    lapply(
      stringr::str_split(inventory$file, "/"), length))

  # Add column for whether or not a file is an archive
  cat("Adding 'is_archive' column.\n")
  archive_regex <- ".*\\.(tar|gz|bz2|zip|tgz)$"
  inventory$is_archive <- grepl(archive_regex, inventory$filename)

  # Add a column for the format ID
  cat("Adding 'format_id' column.\n")
  inventory$format_id <- guess_format_id(inventory$filename)

  # Add a column for packages
  cat("Adding 'package' column.\n")
  inventory$package <- NA
  inventory <- as.data.frame(inventory) # Conver to data.frame in case it's a tbl_df

  # Traverse depth-first
  stopifnot("depth" %in% names(inventory))

  for (d in seq(max(inventory$depth), min(inventory$depth))) {
    inv_atdepth_metadata <- which(inventory$depth == d & inventory$is_metadata == TRUE)
    folders <- unique(inventory[inv_atdepth_metadata,"folder"])

    for (folder in folders) {
      # Find all files under this folder's hierarchy that haven't already been
      # packaged
      files_in_package <- stringi::stri_startswith_fixed(inventory$file, paste0(folder, "/")) & is.na(inventory$package)
      inventory[files_in_package,"package"] <- digest::sha1(folder)
    }
  }

  # Calculate statistics related to packages
  inventory <- inventory %>%
    dplyr::group_by(package) %>%
    dplyr::mutate(package_nfiles = length(package),
                  package_size_mb = round(sum(size_bytes) / 1024 / 1024, 1),
                  package_has_archives = any(is_archive))

  as.data.frame(inventory)
}
