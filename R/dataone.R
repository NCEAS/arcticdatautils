#' dataone.R
#'
#' Helpers for the DataONE R package.

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


#' Determine whether the set token is expired.
#'
#' @return
#' @export
#'
#' @examples
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
    stop("Failed to get token info.")
  }
  stopifnot("dataone_test_token" %in% token_info$name)
  expired <- token_info[token_info$name == "dataone_test_token","expired"]

  if (expired == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Get the base URL of the Member Node.
#'
#' @param mn
#'
#' @return
#' @export
#'
#' @examples
get_mn_base_url <- function(mn) {
  # Determine MN URL. Accept either an MNode or a character string
  if (class(mn) == "MNode") {
    mn_base_url <- mn$base_url
  }

  mn_base_url <- mn
}


#' Check if the user has authorization to perform an action on an object.
#'
#' @param mn (MNode) The Member Node to query.
#' @param ids (character) The PID or SID to check.
#' @param action (character) One of read, write, or changePermission.
#'
#' @export
is_authorized <- function(mn, ids, action) {
  stopifnot(class(mn) == "MNode")
  stopifnot(is.character(ids))
  stopifnot(action %in% c("read", "write", "changePermission"))

  token <- get_token()
  base_url <- paste0(mn@baseURL, "/", mn@APIversion)

  sapply(ids, function(id) {
    req <- httr::GET(paste0(base_url, "/isAuthorized/", id),
                     query = list(action = action),
                     httr::add_headers("Authorization" = paste0("Bearer ", token)))

    if (req$status_code == 200) {
      return(TRUE)
    } else if (req$status_code == 401) {
      return(FALSE)
    } else if (req$status_code == 404) {
      stop(paste0("An object with the ID ", id, " was not found."))
    } else {
      stop(paste0("An error occurred while checking authorization on the ID '", id, "': Status code was ", req$status_code, "."))
    }
  })
}
