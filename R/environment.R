#' environment.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions related to loading configuriation based upon the environment
#' the code is being run under.


#' Get the current environment name.
#'
#' @return The environment name (character)
env_get <- function() {
  env <- Sys.getenv("ARCTICDATA_ENV")

  if (env == "") {
    env <- "development"
  }

  env
}

#' Load environmental variables from a YAML-formatted environment file.
#'
#' This file should be formatted in the following way:
#'
#' some_environment:
#'   var_one: some value
#'   var_two: some value
#'   var_three: some value
#'
#' @param name (Optional) The environment name (character)
#' @param path (Optional) Path to an environment file (character)
#'
#' @return A list of name <-> value pairs.
#' @export
#'
#' @examples
#' #' Loading the above file with `env_load()` would return a list with the shape:
#' > Sys.setenv("ARCTICDATA_ENV", "some_environment")
#' > dir()
#' [1] "env.yml"
#' > env <- env_load("env.yml)
#' > env
#' $var_one
#' [1] "some value"
#'
#' $var_two
#' [1] "some value"
#'
#' $var_three
#' [1] "some value
env_load <- function(name=NULL, path=NULL) {
  # Determine the environment to load
  if (is.null(name)) {
    name <- env_get()
  }

  # If ARCTICDATA_ENV is set but a
  # Determine the file to load the environment from
  if (!is.null(path)) {
    stopifnot(file.exists(path))

    file <- path
  } else {
    file <- file.path(system.file(package = "arcticdatautils"), "environment.yml")
  }
  stopifnot(file.exists(file))

  # Pull out the content from the YAML file
  yaml_content <- yaml::yaml.load_file(file)
  stopifnot(length(yaml_content) == 3)
  stopifnot(name %in% names(yaml_content))
  env <- yaml_content[name][[1]]

  # Load up the MN
  env$mn <- dataone::MNode(env$mn_base_url)

  # Return the list of environmental variables
  env
}
