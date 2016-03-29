# Insert the resource maps for everything

devtools::load_all(".")
devtools::load_all("~/src/ropensci-datapackage/")


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
Sys.setenv("ARCTICDATA_ENV" = "production")
options(authentication_token = Sys.getenv("D1TOKEN"))
env <- env_load("etc/environment.yml")
library(dataone)
env$mn <- MNode(env$mn_base_url)

# Run it
for (d in max(inventory$depth):min(inventory$depth)) {
  print(d)

  packages_at_depth <- unique(inventory[inventory$is_metadata & inventory$depth == d,"package"])

  for (package in packages_at_depth) {
    print(package)

    insert_package(inventory, package, env)
  }
}


