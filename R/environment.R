#' environment.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions related to loading configuriation based upon the environment
#' the code is being run under.


#' Get the current environment name.
#'
#' @return (character) The environment name.
#'
#' @export
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
#' @param name (character) Optional. The environment name.
#' @param path (character) Optional. Path to an environment file.
#' @param skip_mn (logical) Optional. Skip contacting the MNode and filling in the $mn element of the environment.
#'
#' @return (list) A list of name-value pairs.
#'
#'
#'
env_load <- function(name=NULL, path=NULL, skip_mn=FALSE) {
  if (!requireNamespace("yaml")) {
    stop(call. = FALSE, 
         "The package 'yaml' must be installed to run this function. ",
         "Please install it and try again.")
  }

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
  if (!skip_mn) {
    env$mn <- dataone::MNode(env$mn_base_url)
  }

  # Return the list of environmental variables
  env
}
