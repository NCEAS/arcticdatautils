#' redis.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Utility functions for working with rrqueue and storr


#' Primary job to be run from the command line.
main_job <- function() {
  connection <- wait_for_redis()

  # Load inventory
  cat(paste0("Reading in inventory from 'inv.csv'...\n"))
  inventory <- read.csv("../inv.csv", stringsAsFactors = FALSE)
  inventory <- inventory[!is.na(inventory$file),]

  stopifnot(is.data.frame(inventory),
            nrow(inventory) > 0)

  # temp
  inventory$pid <- ""
  inventory$created <- FALSE
  # /temp

  packages <- unique(inventory$package)


  cat(paste0("Read in ", nrow(inventory), " rows into the Inventry.\n"))
  cat(paste0("Inventory contains ", length(packages), " packages.\n"))




  cat(paste0("Creating storr environment store...\n"))
  jobs <- storr::storr_environment()
  running <- TRUE
  counter <- 0

  while (running == TRUE) {
    inventory <- merge_completed_jobs(jobs, inventory)

    cat(paste0(counter, "\n"))
    # Stop running if we get to the end

    if (counter + 1 > length(packages)) {
      cat(paste0("Reached the end of packages."))
      running <- FALSE
      next
    }

    # Get the next package in the list
    counter <- counter + 1
    package <- packages[counter]

    # Only insert if needed
    is_it_created <- all(inventory[inventory$package == package,"created"], na.rm = TRUE)

    if (is_it_created == TRUE) {
      cat(paste0("Package ", package, " already created. Moving on.\n"))
      next
    }

    cat(paste0("Enqueueing insertion of package ", package, "\n"))
    package_files <- inventory[inventory$package == package,]

    jobs$set(package, connection$enqueue_(identity(package_files)))
  }
}


#' Helper function to print the status of the inventory and the store.
print_status <- function(store, inventory) {
  cat(paste0("Inventory CREATED:"))
  print(table(inventory$created))
  cat("\n")

  cat(paste0("Tasks:"))
  print(connection$tasks_overview())
  cat("\n")

  jobs <- store$list()
  merge_jobs <- jobs[grep("-merged", jobs)]
  merge_job_results <- vapply(merge_jobs, function(x) { store$get(x) }, logical(1))

  cat(paste0("Merges:"))
  print(table(merge_job_results))
  cat("\n")
}


#' Merge completed jobs back into the inventory
merge_completed_jobs <- function(store, inventory) {
  # merge packages with status == complete and merged == FALSE

  packages <- unique(inventory$package)

  for (package in packages) {
    if (is_package_merged(store, package)) {
      # cat(paste0("Package ", package, " is already merged. Skipping merge of package.\n"))
      next
    }

    if (!store$exists(package)) {
      # cat(paste0("Job for package ", package, " not queued yet. Skipping merge of package.\n"))
      next
    }

    # Get the job result
    job <- store$get(package)

    # Skip if not complete
    if (job$status() != "COMPLETE") {
      next
    }

    cat(paste0("Merging package ", package, ".\n"))
    job_result <- job$result()
    inventory <- inv_update(inventory, job_result)

    # Set the package as merged
    set_package_merged(store, package)
  }

  inventory
}


#' Test whether the package with the given package identifier has been merged
#' back into the inventory.
is_package_merged <- function(store, package) {
  key <- paste0(package, "-merged")

  if (store$exists(key)) {
    value <- store$get(key)
  } else {
    value <- FALSE
  }

  value
}


#' Set a key in the store to track whether a given package has been merged
#' back into the inventory.
set_package_merged <- function(store, package) {
  key <- paste0(package, "-merged")
  store$set(key, TRUE)
}


#' Test whether a job exists for a particular package identifier.
package_in_queue <- function(store, package) {
  store$exists(package)
}


#' Count the number of pending jobs in the store.
count_pending_jobs <- function(store) {
  job_list <- store$list()
  statuses <- sapply(job_list, function(x) { store$get(x)$status()})

  length(which(statuses == "PENDING"))
}


#' Print jobs in a given storr store.
print_jobs <- function(store, print_result=FALSE) {
  jobs <- store$list()
  cat(paste0("Printing jobs...\n"))

  for (j in jobs) {
    job_value <- store$get(j)
    cat(paste0("Job `", j, "` has status ", job_value$status(), "\n"))

    if (print_result == TRUE) {
      job_result <- job_value$result()
      if (is.data.frame(job_result)) {
        job_result_output <- class(job_result)
      } else {
        job_result_output <- job_result
      }

      cat(paste0(job_result_output, "\n"))
    }

    cat("\n\n")
  }
}


#' Test job I'm using to test the job queue system.
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


#' Repeatedly tries to establish a Redis connection and get the default
#' queue from it.
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
        e
      }
    )

    if (exists("queue") && "queue" %in% class(queue)) {
      connected = TRUE
    } else {
      print(queue)

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
