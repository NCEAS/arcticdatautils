# Utility functions for the DataONE API


#' Test whether a token is set.
#'
#' @return
#' @export
#'
#' @examples
is_token_set <- function() {
  token <- tryCatch(get_token(),
                    error = function(e) FALSE)

  if (token == FALSE) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Gets the currently set authentication token.
#'
#' @return
#' @export
#'
#' @examples
get_token <- function() {
  # Get token
  if (is.null(getOption("dataone_test_token"))) {
    stop("No authentication token is set in options(). Please set one and try again.")
  }

  getOption("dataone_test_token")
}


is_token_expired <- function() {
  # Check for presence of the token in options()
  if (!is_token_set()) {
    log_message("Authentication token not set in options().")
    return(FALSE)
  }

  token_info <- try({
    dataone::getTokenInfo(dataone::AuthenticationManager())
  })

  if (inherits(token_info, "try-error") ||
      !is.data.frame(token_info) ||
      !("expired" %in% names(token_info))) {
    log_message("Failed to get token info.")
    return(FALSE)
  }

  if (token_info$expired == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
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

