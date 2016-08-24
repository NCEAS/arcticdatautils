#' dataone.R
#'
#' Helpers for the DataONE R package.

#' Check if the user has authorization to perform an action on an object.
#'
#' @param base_url (character) The DataONE API REST API base URL to query.
#' @param ids (character) The PID or SID to check.
#' @param action (character) One of read, write, or changePermission.
#'
#' @export
is_authorized <- function(base_url, ids, action) {
  stopifnot(is.character(base_url),
            length(base_url) == 1)
  stopifnot(is.character(ids))
  stopifnot(action %in% c("read", "write", "changePermission"))

  token <- get_token()

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
