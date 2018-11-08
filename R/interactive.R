# Functions for interactive viewing of the inventory and other objects


#' View packages
#'
#' @param inventory (character) An inventory.
#'
#' @noRd
view_packages <- function(inventory) {
  stopifnot(is.data.frame(inventory),
            nrow(inventory) > 0)
  stopifnot("package" %in% names(inventory))

  packages <- unique(inventory$package)
  viewing <- TRUE
  pkg_index <- 0
  while (viewing == TRUE) {
    pkg_index <- pkg_index + 1
    package <- packages[pkg_index]
    show_package(inventory, package)

    response <- wait_for_key()

    if (response == "s") {
      viewing <- FALSE
    }
  }
}


# Helper function for view_packages()
wait_for_key <- function() {
  response <- readline(prompt = "Press [S]top or [C]ontinue")
  response <- tolower(response)
  response
}


# Helper function for view_packages()
show_package <- function(inventory, package) {
  cat(paste0("Package: ", package, "\n"))

  inv_p <- inventory[inventory$package == package,,drop = FALSE]
  inv_p <- inv_p[!is.na(inv_p$file),]

  metadata_files <- inv_p[inv_p$is_metadata == TRUE,]
  data_files <- inv_p[inv_p$is_metadata == FALSE,]

  cat(paste0("Metadata file: ", metadata_files[1,"filename"], "\n"))

  cat(paste0("File in package: ", nrow(inv_p), "\n"))

  cat(paste0("Data files...\n"))
  print(data_files[,"filename"])
}
