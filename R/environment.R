#' environment.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions related to loading configuriation based upon the environment
#' the code is being run under.

library(yaml)

# TODO: Implement this function for real
env_get <- function() {
  "development"
}

env_load <- function(file) {
  stopifnot(file.exists(file))

  yaml_content <- yaml.load_file(file)
  stopifnot(length(yaml_content) == 3)

  current_env <- env_get()
  stopifnot(current_env %in% names(yaml_content))

  yaml_content[current_env][[1]]
}
