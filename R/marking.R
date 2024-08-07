# Functions for marking datasets before adding


#' Divide packages and their files into themes
#'
#' @description
#' Themes divide packages into groups based upon how the actions we will take
#' to insert them. Packages are divided into one of three themes:
#'
#'  "many-files"
#'
#'    The package has more files than we'd like to include in a Resource Map
#'    and we will want to archive its contents before inserting.
#'
#'  "has-versions"
#'
#'    The package has version information embedded into its folder structure.
#'    These packages will be hand-verified and inserted manually when a plan
#'    is developed to insert them.
#'
#'  "ready-to-go"
#'
#'    All other packages not in the above themes.
#'
#' Note: Adds a 'theme' column to 'inventory'.
#' Note: Depends on the following columns:
#'
#'  - filename
#'  - package_nfiles
#'
#'
#' @param inventory (data.frame) An inventory.
#' @param nfiles_cutoff (integer) Number of cutoff files.
#'
#' @return (data.frame) An inventory.
#'
#' @noRd
#' @importFrom rlang .data
theme_packages <- function(inventory, nfiles_cutoff=100) {
  stopifnot(is.data.frame(inventory),
            "package_nfiles" %in% names(inventory))

  if ("theme" %in% names(inventory)) {
    warning("Column 'theme' already exists.
            Overwriting any changes you may have made.")
  }

  # Note:
  #
  # We mark packages/files in a specific order. We need to mark versions last
  # because we want that rule to apply no matter the package size.

  # First mark everything as ready-to-go
  inventory$theme <- "ready-to-go"

  # Then mark packages with too many files
  inventory$theme[inventory$package_nfiles > nfiles_cutoff] <- "many-files"

  # Then mark packages with versions embedded in their folders
  inventory$theme[grep("v_\\d\\.", inventory$file)] <- "has-versions"

  # There should be no un-themed packages once we're done
  theme_stats <- dplyr::group_by(inventory, .data$theme)
  theme_stats <- dplyr::filter(theme_stats, .data$is_metadata == TRUE)
  theme_stats <- dplyr::summarize(theme_stats, npkgs = length(.data$filename))

  cat("Theme summary (by package):\n")

  for (i in seq_len(nrow(theme_stats))) {
    cat(paste0("  ",
               theme_stats[i,"theme"],
               ":\t",
               theme_stats[i,"npkgs"],
               " package(s)\n"))
  }

  inventory
}
