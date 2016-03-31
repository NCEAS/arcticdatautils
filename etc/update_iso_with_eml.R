#' Update all the ISO metadata with EML
#'
#'
devtools::load_all(".")
library(dataone)
devtools::load_all("~/src/ropensci-datapackage/")
library(xslt)
library(xml2)
library(XML)
library(stringr)
library(stringi)

isotoeml <- xslt::read_xslt("iso2eml.xsl")

inventory <- read.csv("~/src/arctic-data/inventory/master_all.csv", stringsAsFactors = FALSE)
nrow(inventory)
nrow(inventory[!is.na(inventory$package),])
inventory <- inventory[!is.na(inventory$package),] #Filter out non-package files
nrow(inventory)

# Filter out large packages
nrow(inventory)
inventory <- inventory[inventory$package_nfiles <= 1000,]
nrow(inventory)

# Setup
Sys.setenv("ARCTICDATA_ENV" = "development")
options(authentication_token = Sys.getenv("D1TOKEN"))
env <- env_load("etc/environment.yml")
library(dataone)
env$mn <- MNode(env$mn_base_url)
env$base_path <- "~/sync/"

######
#' For each package:
#'  - Update the metadata object (new pid is used)
#'  - Update the resource map
#'  - Update all resource maps above it ? No. This will be done later.



# Run it
for (d in max(inventory$depth):min(inventory$depth)) {
  print(d)

  packages_at_depth <- unique(inventory[inventory$is_metadata & inventory$depth == d,"package"])

  for (package in packages_at_depth) {
    convert_to_eml_and_update_package(inventory, package)
  }
}

# Insert a file, temporary code
# inventory[inventory$file == package_files[metadata_file_idx,"file"],"created"] <- FALSE
# insert_file(inventory, package_files[metadata_file_idx,"file"], env)
# inventory[inventory$file == package_files[metadata_file_idx,"file"],"created"] <- TRUE
