library(methods) # for new()
devtools::load_all(".")

# Decide which data file and log file to use
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 1) {
  data_file <- args[1]
} else {
  data_file <- "inventory/data.rda"
}

# Validate args
stopifnot(file.exists(data_file))
log_path <- paste0("insert_data-", gsub("/", "_", data_file), ".log")
Sys.setenv("LOG_PATH" = log_path)
if (nchar(log_path) != 0) {
  log_message(paste0("Using alternate log path of '", log_path, "'\n"))
}

# Load data
log_message(paste0("Loading data file from location ", data_file, "\n"))
load(data_file)
inventory$ready <- TRUE

# Do basic sanity checking on the data
stopifnot("inventory" %in% ls(),
          all(c("pid", "created") %in% names(inventory)))
stopifnot(all(is.character(inventory$pid)))

# Set environment
log_message("Check to see the env is set.")
env_name = Sys.getenv("ARCTICDATA_ENV")
stopifnot(nchar(env_name) > 0)
log_message(paste0("Loading environment '", env_name, "'.\n"))
env <- env_load("etc/environment.yml")
env$mn <- dataone::MNode(env$mn_base_url) # Set up MN instance

# Set token
log_message("Setting the d1 token")
token = Sys.getenv("D1TOKEN")
stopifnot(nchar(token) > 0)
options(authentication_token = token)

# Stop if the token is expired
if (is_token_expired()) stop("Token is expired.")

# Insert each file
for (i in seq_len(nrow(inventory))) {
  # Check if the file "quit" exists, and, if so, save the inventory and quit
  if (file.exists("quit")) {
    log_message(paste0("File 'quit' exists. Saving inventory and shutting down."))
    break
  }

  # Insert blank line into logs just to help readability
  log_message(" ")

  # Grab an updated token from disk if the token is expired and there is one
  # on disk at 'd1token
  if (is_token_expired() && file.exists("d1token")) {
    log_message("Setting token from contents of file 'd1token'")
    token <- paste0(readLines(con = "d1token"), collapse = "")

    if (nchar(token) > 0) {
      options(authentication_token = token)
    }
  }

  file_path <- inventory[i,"file"]

  if (inventory[i,"created"] == TRUE) {
    log_message(paste0("Object for file ", file_path, " already created. Moving on.\n"))
    next
  }

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
    log_message("Skipping the rest of inserting due to an error in insert_file()\n")
    next
  }

  log_message("Updating inventory...")
  inventory <- inv_update(inventory, insert_result, env)

  # Save to disk every 100 objects
  if (i %% 100 == 0) {
    log_message(paste0("Saving inventory to disk at ", data_file, "\n"))
    save(inventory, file = data_file)
  }
}

log_message("All done, saving inventory to disk one last time.")
save(inventory, file = data_file)
