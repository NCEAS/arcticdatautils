#' Test updating an existing metadata object that is part of a data package.
#'
#' The basic workflow is:
#'  1. Update the metadata object
#'    1a. Make any changes to the metadata object that are desired
#'        (for example, convert from one metadata standard to another)
#'  2. Update the metadata object
#'  3. Update the resource map the metadata object is part of
#'
#'  A special note about this script: If the metadata object that's being
#'  updated is part of a set of nested data data packages, you'll want to
#'  insert these depth-first (children-first)
#'
#' This process and its functions use the existing Inventory structure, but
#' with one special modification:
#'
#' The $pid column is the new PID for the updated objects and a new column
#' is introduced $pid_old so that the correct linkages can be made.

library(dataone)
devtools::load_all("~/src/ropensci-datapackage/")
library(dplyr)

# Set up the environment
env <- env_load("etc/environment.yml")
env$mn <- MNode(env$mn_base_url)
env$base_path <- "~/src/arctic-data/arcticdata/"

# Load the inventory
inventory_path <- "inst/inventory_nested/inventory_nested.csv"
inventory <- read.csv(inventory_path,
                      stringsAsFactors = FALSE)


# (Optional) Insert the originals
inventory$pid <- sapply(inventory, digest::digest, algo = "sha256")
inventory$created <- FALSE
inventory$ready <- TRUE

for (d in seq(max(inventory$depth), min(inventory$depth))) {
  for (package in unique(inventory[inventory$depth == d,"package"])) {
    cat(paste0("Inserting package ", package, "\n"))

    last_insert <- insert_package(inventory, package, env)
    inventory <- inv_update(inventory, last_insert)
  }
}

# Bring in a list of new PIDs
# new_pids <- read.csv("...")
# inventory <- left_join(inventory, new_pids, by = "file")
inventory$pid_old <- inventory$pid
inventory$pid <- sapply(1:nrow(inventory),
                        function(x) { paste0("urn:uuid:",
                                             uuid::UUIDgenerate() )})

# Insert
for (d in max(inventory$depth):min(inventory$depth)) {
  print(d)

  packages_at_depth <- unique(inventory[inventory$is_metadata & inventory$depth == d,"package"])

  for (package in packages_at_depth) {
    print(package)

    convert_to_eml_and_update_package(inventory, package, env)
  }
}
