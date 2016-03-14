devtools::load_all(".")

# Load inventory
log_message("Loading inventory rda")
load("inventory/inv.rda")
stopifnot(is.data.frame(inventory))

# Set environment
log_message("Check to see the env is set.")
env_name = Sys.getenv("ARCTICDATA_ENV")
stopifnot(nchar(env_name) > 0)
log_message(paste0("Loading environment '", env_name, "'.\n"))
env <- env_load("etc/environment.yml")
log_message(paste0("Setting up MNode instance for '", env$mn_base_url, "'.\n"))
env$mn <- dataone::MNode(env$mn_base_url) # Set up MN instance

# Set token
log_message("Setting the d1 token")
token = Sys.getenv("D1TOKEN")
stopifnot(nchar(token) > 0)
options(authentication_token = token)

packages <- unique(inventory$package)

for (package in packages) {
  log_message(paste0("Inserting package ", package, "\n"))

  insert_result <- tryCatch({
    insert_package(inventory, package)
  },
  error = function(e) {
    log_message(paste0("There was an error during the inserting of package ", package, "\n"))
    log_message(e$message)
    e
  })

  if (inherits(insert_result, "error")) {
    next
  }

  log_message("Updating inventory...")
  inventory <- inv_update(inventory, insert_result)

  log_message("Writing inventory to disk...")
  save(inventory, file = "inventory-latest.rda")
}
