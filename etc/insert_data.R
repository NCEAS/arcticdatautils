devtools::load_all(".")

# Load data
log_message("Loading data rda")
load("data/data.rda")
inventory <- inv
inventory$created <- FALSE
inventory$ready <- TRUE

# Set environment
log_message("Setting environment to 'production'")
Sys.setenv("ARCTICDATA_ENV" = "production")

# Set token
log_message("Setting the d1 token")
token = Sys.getenv("D1TOKEN")
stopifnot(nchar(token) > 0)
options(authentication_token = token)

# Mint PIDs for all objects
log_message("Giving all objects new UUID pids")
inventory$pid <- sapply(1:nrow(inventory), function(x) { paste0("urn:uuid:", uuid::UUIDgenerate()) })


for (i in seq_len(nrow(inventory))) {
  file_path <- inventory[i,"file"]

  log_message(paste0("Inserting file ", file_path, "\n"))

  insert_result <- tryCatch({
    insert_file(inventory, file_path)
  },
  error = function(e) {
    log_message(paste0("There was an error during the inserting of file ", file_path, "\n"))
    log_message(e$message)
    e
  })

  if (inherits(insert_result, "error")) {
    next
  }

  log_message("Updating inventory...")
  inventory[i,"created"] <- TRUE

  log_message("Writing inventory to disk...")
  save(inventory, file = "inventory-latest.rda")
}
