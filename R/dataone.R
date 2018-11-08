# Helper functions for the DataONE R package


#' Test whether a token is set
#'
#' Test whether a token is set.
#'
#' @param node (MNode/CNode) The Member/Coordinating Node to query.
#'
#' @return (logical)
#'
#' @export
#'
#' @examples
#'\dontrun{
#'cn <- CNode('STAGING2')
#'mn <- getMNode(cn,"urn:node:mnTestKNB")
#'is_token_set(mn)
#'}
is_token_set <- function(node) {
  token <- tryCatch(get_token(node),
                    error = function(e) FALSE)

  if (is.null(token) || token == FALSE) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Get the currently set authentication token
#'
#' Get the currently set authentication token.
#'
#' @param node (MNode/CNode) The Member/Coordinating Node to query.
#'
#' @return (character) The token.
#'
#' @export
#'
#' @examples
#'\dontrun{
#'cn <- CNode('STAGING2')
#'mn <- getMNode(cn,"urn:node:mnTestKNB")
#'get_token(mn)
#'}
get_token <- function(node) {
  if (!(class(node) %in% c("MNode", "CNode"))) {
    stop(paste0("Node must be an MNode or CNode. You passed in a '", class(node), "'."))
  }

  if (node@env == "prod") {
    token <- getOption("dataone_token")
  } else if (node@env == "test") {
    token <- getOption("dataone_test_token")
  }

  if (is.null(token)) {
    stop("No token could be found. Please set one with options(...).")
  }

  token
}


#' Determine whether token is expired
#'
#' Determine whether the set token is expired.
#'
#' @param node (character) The Member Node.
#'
#' @return (logical)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- CNode('STAGING2')
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' is_token_expired(mn)
#' }
is_token_expired <- function(node) {
  token_name <- ifelse(node@env == "prod", "dataone_token", "dataone_test_token")

  # Check for presence of the token in options()
  if (!is_token_set(node)) {
    stop("The appropriate token was not set. You must set a token via options(", token_name, "='...')")
  }

  token_info <- try({
    dataone::getTokenInfo(dataone::AuthenticationManager())
  })

  if (inherits(token_info, "try-error") ||
      !is.data.frame(token_info) ||
      !("expired" %in% names(token_info))) {
    stop("Failed to get token info.")
  }

  if (!(token_name %in% token_info$name)) {
    stop("The appropriate token was not set. You must set a token via options(", token_name, "='...')")
  }

  expired <- token_info[token_info$name == token_name,"expired"]

  if (expired == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Get base URL of a Member Node
#'
#' Get the base URL of a Member Node.
#'
#' @param mn (character) The Member Node.
#'
#' @return (character) The URL.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode('STAGING2')
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
# 'url <- get_mn_base_url(mn)
#'}
get_mn_base_url <- function(mn) {
  # Determine MN URL. Accept either an MNode or a character string
  if (is(mn, "MNode")) {
    mn_base_url <- mn@base_url
  }

  mn_base_url <- mn
}


#' Check if user has authorization to perform an action on an object
#'
#' Check if the user has authorization to perform an action on an object.
#'
#' @param node (MNode/CNode) The Member/Coordinating Node to query.
#' @param ids (character) The PID or SID to check.
#' @param action (character) One of read, write, or changePermission.
#'
#' @return (logical)
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn <- CNode('STAGING2')
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
#' "urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
#' is_authorized(mn, pids, "write")
#'}
is_authorized <- function(node, ids, action) {
  stopifnot(class(node) %in% c("MNode", "CNode"))
  stopifnot(is.character(ids))
  stopifnot(action %in% c("read", "write", "changePermission"))

  base_url <- paste0(node@baseURL, "/", node@APIversion)

  sapply(ids, function(id) {
    req <- httr::GET(paste0(base_url, "/isAuthorized/", id),
                     query = list(action = action),
                     httr::add_headers("Authorization" = paste0("Bearer ", get_token(node))))

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
