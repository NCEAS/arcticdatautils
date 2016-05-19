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
#' @param file A YAML-formatted environment file.
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
env_load <- function(file=NA) {
  if (is.na(file)) {
    file <- file.path(system.file("inst", package = "arcticdatautils"), "environment.yml")
  }

  stopifnot(file.exists(file))

  yaml_content <- yaml::yaml.load_file(file)
  stopifnot(length(yaml_content) == 3)

  current_env <- env_get()
  stopifnot(current_env %in% names(yaml_content))

  yaml_content[current_env][[1]]
}
