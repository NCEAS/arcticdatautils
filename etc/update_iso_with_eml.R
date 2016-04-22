#' Update all the ISO metadata with EML

# library(dataone) # Custom reference
# library(datapack) # Custom reference
devtools::load_all(".")

# Load inventory
inventory_file_path <- "inventory/master_updated.csv"
if (!file.exists(inventory_file_path)) {
  log_message("Inventory file not found. Exiting.")
  break
}

log_message("Loading inventory file.")
inventory <- read.csv(inventory_file_path,
                      stringsAsFactors = FALSE)

# Filter out non-package files
inventory <- inventory[which(!is.na(inventory$package)),]

# Setup env
Sys.setenv("ARCTICDATA_ENV" = "production")
env <- env_load("etc/environment.yml")
library(dataone)
env$mn <- MNode(env$mn_base_url)

# Set up token
token <- Sys.getenv("D1TOKEN")

if (nchar(token) == 0) {
  log_message("No token found on env var $D1TOKEN. Set it and restart.\n")
  break
}

log_message("Setting authentication token...")
options(authentication_token = token)


# Run it
for (d in max(inventory$depth):min(inventory$depth)) {
  log_message(paste0("Processing depth ", d, "."))

  packages_at_depth <- unique(inventory[inventory$is_metadata & inventory$depth == d,"package"])

  for (package in packages_at_depth) {
    # Check if 'quit' file exists and quit
    if (file.exists("quit")) {
      log_message("Quit file was present. Finishing up and exiting.")
      break
    }

    log_message(paste0("Inserting package ", package, "."))

    last <- update_package(inventory, package, env)
  }
}

log_message("All done.")

