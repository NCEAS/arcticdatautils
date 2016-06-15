#' zzz.R
#'
#' Runs code on package load.
#'

.onLoad <- function(libname, pkgname){
  warn_current_version()
}
