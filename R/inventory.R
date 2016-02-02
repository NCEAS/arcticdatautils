#' inventory.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions relating to keeping up an inventory of files that exist on the KNB
#' and may or may not be copied to another computer and untarred.
#'

library(dplyr)
library(stringr)
library(stringi)

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

#' Load filenames into the inventory from a text file.
#'
#' Filenames should be the output of the command:
#'
#'   you@server:/home/jones/acadis$ find . -type f
#'
#' @param filename Filepath to a file containing filenames.
#' @param inventory A \code{data.frame}.
#'
#' @return An inventory (data.frame)
#'
#' @examples
#' inv_load_filenames("some/file/path.txt", my_inventory)
inv_load_filenames <- function(filename, inventory) {
  stopifnot(file.exists(filename))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory))

  # Read the filenames from disk
  filenames <- read.delim(filename,
                          col.names = c("filename"),
                          header = FALSE,
                          stringsAsFactors = FALSE)
  stopifnot(is.data.frame(filenames))

  # Filter files no under with 'acadis-field-projects' or 'acadis-gateway'
  # subfolders
  size_before <- nrow(filenames)
  filenames <- filenames[stri_startswith_fixed(filenames$filename, "./acadis-"),"filename", drop = FALSE]
  size_diff <- size_before - nrow(filenames)
  if (size_diff > 0) { cat("Removed ", size_diff, "filename(s) that weren't inside acadis-gateway or acadis-field-projects subfolders.\n") }

  # If inventory is empty, just make the inventory the same as filenames
  if (nrow(inventory) == 0) {
    return(filenames)
  }

  # Only append rows with new filenames
  #
  # Merging algorithm:
  #   Add colums we need to `filenames`
  #   Remove rows from `filenames` that exist in `inventory`
  #   Merge the two

  # Make `filenames` the same shape as `inventory` by appending columns
  for (col_name in names(inventory)) {
    if (col_name %in% names(filenames)) { next }
    filenames[,col_name] <- NA
  }

  stopifnot(identical(names(inventory), names(filenames)))

  inter_vals <- intersect(inventory$filename, filenames$filename)

  # Remove intersections if we had any
  if (length(inter_vals) > 0 ) {
    filenames <- subset(filenames, match(filename, inter_vals, nomatch = 0) == 0)
  }

  inventory <- rbind(inventory,
                     filenames)

  inventory
}

#' Load file sizes into an inventory from a text file. Removes the column
#' 'size_bytes' from inventory before doing a left join.
#'
#' @param filename Filepath to a file containing sizes.
#' @param inventory A \code{data.frame}.
#'
#' @return An inventory (data.frame)
#'
#' @examples
inv_load_sizes <- function(filename, inventory) {
  stopifnot(file.exists(filename))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory),
            "filename" %in% names(inventory))

  # Read the sizes from disk
  sizes <- read.delim(filename,
                      col.names = c("size_bytes", "filename"),
                      stringsAsFactors = FALSE,
                      header = FALSE)

  stopifnot(is.data.frame(sizes))

  if (nrow(inventory) != nrow(sizes)) {
    warning(paste("Inventory and incoming 'sizes' data.frame not of the same number of rows.", nrow(inventory), "vs", nrow(sizes)))
  }

  # Join the sizes onto existing filenames in the inventory
  # First drop the existing sizes
  inventory <- inventory[,!(names(inventory) %in% "size_bytes"), drop=FALSE]
  inventory <- left_join(inventory, sizes, by="filename")

  # Check the result
  if (any(is.na(inventory$filename))) { warning("Some values in the 'filename' column were NA.")}
  if (any(is.na(inventory$size_bytes))) { warning("Some values in the 'size_bytes' column were NA.")}

  inventory
}

#' Load checksums into the inventory file from a text file. This function
#' removes the column 'checksum_sha256' from inventory before doing a
#' left join.
#'
#' @param filename Filepath to a file containing sizes.
#' @param inventory A \code{data.frame}.
#'
#' @return An inventory (data.frame)
#'
#' @examples
inv_load_checksums <- function(filename, inventory) {
  stopifnot(file.exists(filename))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory),
            "filename" %in% names(inventory))

  checksums <- read.delim(filename,
                          header = FALSE,
                          col.names = c("checksum_sha256", "filename"),
                          sep = "\t",
                          stringsAsFactors = FALSE)

  stopifnot(is.data.frame(checksums))

  if (nrow(inventory) != nrow(checksums)) {
    warning(paste("Inventory and incoming 'checksums' data.frame not of the same number of rows.", nrow(inventory), "vs", nrow(checksums)))
  }

  # Join the checksums onto existing filenames in the inventory
  # First drop the existing checksums
  inventory <- inventory[,!(names(inventory) %in% "checksum_sha256"), drop=FALSE]
  inventory <- left_join(inventory, checksums, by="filename")

  # Check the result
  if (any(is.na(inventory$filename))) { warning("Some values in the 'filename' column were NA.")}
  if (any(is.na(inventory$checksum_sha256))) { warning("Some values in the 'checksum_sha256' column were NA.")}

  inventory
}

#' Load identifiers into the inventory file from a text file. This function
#' removes the column 'identifier' from inventory before doing a
#' left join.
#'
#' @param filename Filepath to a file containing identifiers.
#' @param inventory A \code{data.frame}.
#'
#' @return An inventory (data.frame)
#'
#' @examples
inv_load_identifiers <- function(filename, inventory) {
  stopifnot(file.exists(filename))
  stopifnot("inventory" %in% ls(),
            is.data.frame(inventory),
            "filename" %in% names(inventory))

  # Read the identifiers from disk
  identifiers <- read.table(filename,
                            col.names = c("identifier", "filename"),
                            stringsAsFactors = FALSE)

  stopifnot(is.data.frame(identifiers))

  # Join the identifiers onto existing filenames in the inventory
  # First drop the existing identifiers
  inventory <- inventory[,!(names(inventory) %in% "identifier"), drop=FALSE]
  inventory <- left_join(inventory, identifiers, by="filename")

  inventory
}

#' Adds a set of extra columsn to the inventory that are useful for working
#' with them.
#'
#' @param inventory An inventory (data.frame)
#'
#' @return An inventory (data.frame)
inv_add_extra_columns <- function(inventory) {
  stopifnot(class(inventory) == "data.frame", "filename" %in% names(inventory))

  # Mark metadata files
  inventory$is_metadata <- stri_endswith_fixed(inventory$filename, "ISO.xml") | stri_endswith_fixed(inventory$filename, "iso19139.xml")

  # Mark which root subfolder each file is under
  inventory$subfolder[stri_startswith_fixed(inventory$filename, "./acadis-field-projects")] <- "FP"
  inventory$subfolder[stri_startswith_fixed(inventory$filename, "./acadis-gateway")] <- "G"

  # Add a column with full paths (w/o filename)
  inventory$folder <- unlist(lapply(str_split(inventory$filename, "/"), function(x) { paste(x[1:(length(x) - 1)], collapse="/") } ))

  # Add a column with just filenames
  inventory$file <- unlist(lapply(str_split(inventory$filename, "/"), function(x) { x[length(x)] } ))

  # Add base dir and depth columns
  inventory <- inv_add_base_dir_column(inventory)
  inventory <- inv_add_depth_columns(inventory)

  inventory
}

#' Add a base directory column to the inventory.
#'
#' In this usage, base directory refers to either the folder of a project or
#' dataset, whatever of the two is the highest hierarchical grouping of
#' datasets.
#'
#' For a dataset that is part of a project, its base directory will be the same
#' as the base directory for that project. For a dataset that is not part of
#' a project, its base directory will be its own base directory.
#'
#' @param inventory An inventory (data.frame)
#'
#' @return An inventory (data.frame)
#' @export
#'
#' @examples
inv_add_base_dir_column <- function(inventory) {
  stopifnot(is.data.frame(inventory), nrow(inventory) > 0)

  # Add base_dir column with the base_dir which is either a project root or a dataset root
  fp_regex <- "(\\.\\/acadis-field-projects\\/[\\w-]+\\/[\\d\\w\\\\.]+)"
  fp_beringsea_regex <- "(\\.\\/acadis-field-projects\\/\\w+\\/\\w+\\/[\\w\\.-]+)"
  gateawy_regex <- "(\\.\\/acadis-gateway\\/[a-zA-Z_-]+)"

  inventory$base_dir <- ""
  metadata_indices <- which(sapply(inventory$is_metadata, isTRUE) == TRUE)

  stopifnot(length(metadata_indices) > 0)

  cat(paste0("Marking base directories for metadata files.\n"))

  for (i in metadata_indices) {
    filename <- inventory[i,"filename"]

    match <- NA

    # Field Projects + Bering Sea
    if (stri_detect_fixed(filename, "BeringSea")) {
      match <- str_extract(filename, fp_beringsea_regex)

      if (is.na(match)) {
        print(filename)
        stop("Match was NA. This is bad.", "bs", filename)
      }

      # Field Projets w/o Bering Sea
    } else if (stri_startswith_fixed(filename, "./acadis-field-projects")) {
      match <- str_extract(filename, fp_regex)

      if (is.na(match)) {
        print(filename)
        stop("Match was NA. This is bad.", "fp", filename)
      }
    } else if (stri_startswith_fixed(filename, "./acadis-gateway")) {
      match <- str_extract(filename, gateawy_regex)

      if (is.na(match)) {
        print(filename)
        stop("Match was NA. This is bad.", "g", filename)
      }
    }

    stopifnot(!is.na(match), nchar(match) > 0)
    inventory[i,"base_dir"] <- match
  }

  unique_base_dirs <- unique(inventory$base_dir)
  stopifnot(length(unique_base_dirs) > 0)

  cat(paste0("Marking base_dirs for non-metadata files.\n"))

  for (i in seq_len(length(unique_base_dirs))) {
    base_dir <- unique_base_dirs[i]

    if (nchar(base_dir) <= 0) {
      next
    }

    cat(paste0(i, ":", base_dir, "\n"))
    inventory[stri_startswith_fixed(inventory$filename, base_dir),"base_dir"] <- base_dir
  }

  inventory
}

#' Add a set of columns for marking datasets with various attributes such as
#' whether they have been added, marked, or can be inserted as-is without
#' modification.
#'
#' @param inventory
#'
#' @return An inventory (data.frame)
#' @export
#'
#' @examples
add_marking_columns <- function(inventory) {
  stopifnot(is.data.frame(inventory))

  if ("added" %in% names(inventory)) {
    stop("Column 'added' already exists. Cannot overwrite.")
  }

  inventory$added <- FALSE # Set false flag

  if ("marked" %in% names(inventory)) {
    stop("Column 'marked' already exists. Cannot overwrite.")
  }

  inventory$marked <- FALSE

  if ("as_is" %in% names(inventory)) {
    stop("Column 'as_is' already exists. Cannot overwrite.")
  }

  inventory$as_is <- FALSE

  inventory
}

#' Add columns for the filesystem depth and the range (max-min) of depths
#' within each base directory
#'
#' @param inventory
#'
#' @return
#' @export
#'
#' @examples
inv_add_depth_columns <- function(inventory) {
  stopifnot(is.data.frame(inventory), nrow(inventory) > 0)
  stopifnot(c("filename", "base_dir") %in% names(inventory))

  # Add a column for the depth
  splits <- str_split(inventory$filename, "/")
  inventory$depth <- unlist(lapply(splits, length))

  # Add a column for the depth difference by base_dir
  inventory <- inventory %>% group_by(base_dir) %>% mutate(depth_diff = max(depth) - min(depth))
  inventory <- as.data.frame(inventory) # Uncast from table_df

  inventory
}
