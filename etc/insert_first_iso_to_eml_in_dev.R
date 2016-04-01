devtools::load_all("vendor/rdataone")
devtools::load_all("vendor/datapack")
devtools::load_all("~/src/arctic-data/arcticdata")

setwd("~/src/iso2eml/tests/iso/")
filenames <- dir()
setwd("~/src/arctic-data/arcticdata")

# Set up the environment
env <- env_load("etc/environment.yml")
env$mn <- MNode(env$mn_base_url)
env$base_path <- "~/src/iso2eml/tests/iso/"

# Insert originals
file_full_paths <- paste0(env$base_path, filenames)
inventory <- data.frame(file = filenames,
                        checksum_sha256 = sapply(file_full_paths, digest::digest, algo="sha256"),
                        size_bytes = sapply(file_full_paths, file.size),
                        stringsAsFactors = FALSE)

inventory <- inv_add_extra_columns(inventory)
inventory$package <- LETTERS[1:10]
inventory$is_metadata <- TRUE
inventory$format_id <- "http://www.isotc211.org/2005/gmd"
inventory <- inv_add_parent_package_column(inventory)
inventory$pid <- sapply(1:nrow(inventory), function(x) { paste0("urn:uuid:", uuid::UUIDgenerate())})
inventory$ready <- TRUE
inventory$created <- FALSE


# Temp just submit one
inventory <- inventory[1,]

# endtemp
for (package in inventory$package) {
  last <- insert_package(inventory, package, env)
  inventory <- inv_update(inventory, last)
}



# Update them now
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
