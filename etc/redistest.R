#' redistest.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Test using redis as a job store

library(rrqueue)
devtools::load_all('.')

# Wait for connection
connection <- wait_for_redis()

# Load inventory
# Find marked but non-inserted datasets
# Start inserting by adding jobs to insert each one
# Max out job queue at N jobs




# logfile <- tempfile()
# rrqueue::worker_spawn("jobs", logfile)
# readLines(con = logfile)



main_job <- function(connection) {
  # Load inventory
  inventory <- read.csv("../inv.csv", stringsAsFactors = FALSE)
  packages <- unique(inventory$package)
  running <- TRUE

  counter <- 1

  # TODO AM:
  # Write in system for monitoring jobs

  queued_jobs <- rep(NA, )

  while (running == TRUE) {
    cat(paste0(counter))

    if (counter > length(packages)) {
      running <- FALSE
      next
    }

    package <- packages[counter]
    package_files <- inventory[inventory$package == package,]
    package_files <- package_files[!is.na(package_files$file),]
    connection$enqueue_(insert_job(package_files))
    counter <- counter + 1
    Sys.sleep(1)
  }
}

insert_job <- function(package_files) {
  stopifnot(is.data.frame(package_files),
            nrow(package_files) > 0,
            "package" %in% names(package_files))

  print(package_files)

  package <- unique(package_files$package)
  stopifnot(length(package) == 1)

  print(package)

  # Returns what was done, either successfully or unsuccessfully
  package_files
}


################################################################################

wait_for_redis <- function() {
  attempts <- 0
  delay_intervals <- c(1, 3, 10, 15, 30, 60)
  connected <- FALSE

  while (connected == FALSE) {
    cat(paste0("Attempting to connect to Redis.\n"))
    attempts <- attempts + 1

    queue <- tryCatch(
      {
        rrqueue::queue("jobs")
      },
      error = function(e) {
        cat(paste0("Failed to connect to Redis. (Attempt number ", attempts, ")\n"))
      }
    )

    if (exists("queue") && "queue" %in% class(queue)) {
      connected = TRUE
    } else {
      # Get the appropriate amount of sleep time
      sleep_time <- ifelse(attempts > length(delay_intervals),
                           delay_intervals[length(delay_intervals)],
                           delay_intervals[attempts])

      cat(paste0("Will try again to connect to Redis in ", sleep_time, " seconds.\n"))
      Sys.sleep(sleep_time)
    }
  }
  stopifnot(connected == TRUE)
  cat(paste0("Successfully connected to Redis.\n"))

  queue
}
