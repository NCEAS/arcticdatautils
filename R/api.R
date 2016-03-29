# Utility functions for the DataONE API


get_token <- function() {
  # Get token
  if (is.null(getOption("authentication_token"))) {
    stop("No authentication token is set in options(). Please set one and try again.")
  }

  getOption("authentication_token")
}


get_mn_base_url <- function(mn) {
  # Determine MN URL. Accept either an MNode or a character string
  if (class(mn) == "MNode") {
    mn_base_url <- mn$base_url
  }

  mn_base_url <- mn
}

#' Update an object
#'
#' @param oldpid
#' @param newpid
#' @param sysmeta
#' @param object
#'
#' @return
#' @export
#'
#' @examples
object_update <- function(mn, oldpid, newpid, sysmeta, file) {
  # Validate arguments
  stopifnot(class(mn) == "MNode" || is.character(mn) && nchar(mn) > 0)
  stopifnot(all(sapply(oldpid, newpid, is.character)))
  stopifnot(class(sysmeta) == "SystemMetadata")
  stopifnot(is.character(file) && nchar(file) > 0 && file.exists(file))

  mn_base_url <- get_mn_base_url(mn)
  token <- get_token()

  # Set up the request
  request_url <- paste0(mn_base_url, "/object/", oldpid)
  request <- httr::PUT(request_url,
                       httr::add_headers("Authorization" = paste0("Bearer ", token)),
                       body = list(newPid = newpid,
                                   sysmeta = sysmeta,
                                   object = httr::upload_file(file)))

  request
}

