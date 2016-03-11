devtools::load_all(".")

# Load data
log_message("Loading data rda")
load("data/data.rda")
inventory$ready <- TRUE

stopifnot("inventory" %in% ls(),
          "created" %in% names(inventory))

# Set environment
log_message("Check to see the env is set.")
env_name = Sys.getenv("ARCTICDATA_ENV")
stopifnot(nchar(env_name) > 0)

env <- env_load("etc/environment.yml")

# Set token
log_message("Setting the d1 token")
token = Sys.getenv("D1TOKEN")
stopifnot(nchar(token) > 0)
options(authentication_token = token)

stopifnot(all(is.character(inventory$pid)))
stopifnot(all(nchar(inventory$pid) > 0))

for (i in seq_len(nrow(inventory))) {
  file_path <- inventory[i,"file"]

  if (inventory[i,"created"] == TRUE) {
    log_message(paste0("Object for file ", file_path, " already created. Moving on.\n"))
    next
  }

  log_message(paste0("Inserting file ", file_path, "\n"))

  # Save time and file size so we can determine insert rate
  before_time <- Sys.time()
  file_size_mb <- inventory[i,"size_bytes"] / 1024 / 1024

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

  # Print out the insert rate
  time_diff_sec <- as.numeric(Sys.time() - before_time, "secs")
  mb_per_s <- file_size_mb / time_diff_sec
  log_message(paste0("Inserted ", file_size_mb, " MB in ", time_diff_sec, " s (", mb_per_s, " MB/s)\n"))

  log_message("Updating inventory...")
  inventory[i,"created"] <- TRUE

  # Save to disk every 10 objects
  if (i %% 10 == 0) {
    log_message("Saving inventory to disk (data/data.rda")
    save(inventory, file = "data/data.rda")
  }
}

save(inventory, file = "data/data.rda")
