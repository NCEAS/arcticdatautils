#' dataone_formats.R
#'
#' A set of thin functions which return the DataONE format ID string. These are
#' to aid in filling in function arguments and can't remember or don't want to
#' type in the full format ID. By putting these format ID strings into
#' functions, a user's autocompletion routine in their editor can help them
#' fill in the format ID they want.

format_iso <- function() "http://www.isotc211.org/2005/gmd"
format_eml <- function() "eml://ecoinformatics.org/eml-2.1.1"
