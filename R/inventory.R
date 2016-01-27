#' inventory.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions relating to keeping up an inventory of files that exist on the KNB
#' and may or may not be copied to another computer and untarred.
#'

library(dplyr)
library(stringr)

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
  filenames <- read.delim(filename, col.names = c("filename"), stringsAsFactors = FALSE)

  stopifnot(is.data.frame(filenames))

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
  sizes <- read.delim(filename, col.names = c("size_bytes", "filename"), stringsAsFactors = FALSE)

  stopifnot(is.data.frame(sizes))

  if (nrow(inventory) != nrow(sizes)) {
    warning("Data frames not of the same number of rows.")
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

  # Read the sizes from disk
  # NOTE: This is from munged output
  #   y <- readLines("~/src/arctic-data/planning/file-checksums/file-checksums-sha256.txt")
  #   y2 <- gsub("  ", "\t", y)
  #   writeLines(y2, "~/src/arctic-data/planning/file-checksums/file-checksums-sha256.tsv")
  checksums <- read.delim(filename, header = FALSE, col.names = c("checksum_sha256", "filename"), sep = "\t", stringsAsFactors = FALSE)

  stopifnot(is.data.frame(checksums))

  if (nrow(inventory) != nrow(checksums)) {
    warning("Data frames not of the same number of rows.")
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
  identifiers <- read.table(filename, col.names = c("identifier", "filename"), stringsAsFactors = FALSE)

  stopifnot(is.data.frame(identifiers))

  # Join the identifiers onto existing filenames in the inventory
  # First drop the existing identifiers
  inventory <- inventory[,!(names(inventory) %in% "identifier"), drop=FALSE]
  inventory <- left_join(inventory, identifiers, by="filename")

  inventory
}

#' Adds a column with hierarchy depths
#'
#' @param inventory An inventory (data.frame)
#'
#' @return An inventory (data.frame)
inv_add_depth_column <- function(inventory) {
  stopifnot(class(inventory) == "data.frame", "filename" %in% names(inventory))

  splits <- str_split(inventory$filename, "/")
  stopifnot(length(splits) == nrow(inventory))

  inventory$depth <- unlist(lapply(splits, length))
  stopifnot("depth" %in% names(inventory))

  inventory
}
