devtools::load_all(".")

# Load inventory
log_message("Loading inventory rda")
load("inventory/inv.rda")
inventory <- inv
inventory$created <- FALSE
inventory$ready <- TRUE

# Set environment
log_message("Setting environment to 'test'")
Sys.setenv("ARCTICDATA_ENV" = "test")

# Set token
log_message("Setting the d1 token")
token = Sys.getenv("D1TOKEN")
stopifnot(nchar(token) > 0)
options(authentication_token = token)

# Mint PIDs for all objects
log_message("Giving all objects new UUID pids")
inventory$pid <- sapply(1:nrow(inventory), function(x) { paste0("urn:uuid:", uuid::UUIDgenerate()) })

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
