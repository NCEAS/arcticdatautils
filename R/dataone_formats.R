#' dataone_formats.R
#'
#' A set of thin functions which return the DataONE format ID string. These are
#' to aid in filling in function arguments and can't remember or don't want to
#' type in the full format ID. By putting these format ID strings into
#' functions, a user's autocompletion routine in their editor can help them
#' fill in the format ID they want.

#' Helper function to generate the ISO 19139 format ID.w
#'
#' @return (character) The format ID for ISO 19139.
#' @export
#'
#' @examples
#' format_iso()
#' \dontrun{
#' # Upload a local ISO19139 XML file:
#' env <- env_load()
#' publish_object(env$mn, "path_to_some_EML_file", format_iso())
#' }
format_iso <- function() {
  "http://www.isotc211.org/2005/gmd"
}


#' Helper function to generate the EML 2.1.1 format ID.
#'
#' @return (character) The format ID for EML 2.1.1.
#' @export
#'
#' @examples
#' format_eml
#'
#' \dontrun{
#' # Upload a local EML 2.1.1 file:
#' env <- env_load()
#' publish_object(env$mn, "path_to_some_EML_file", format_eml())
#' }
format_eml <- function() {
  "eml://ecoinformatics.org/eml-2.1.1"
}
