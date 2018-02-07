#' Get the list of valid formats from DataONE
#'
#' Note that this function is intended to return even if the request to the CN
#' fails. This is so other functions can call continue even if the request
#' fails.
#'
#' @param url (character) The listFormats endpoint. Defaults to the production
#' CN
#'
#' @return (character)
get_formats <- function(url = "https://cn.dataone.org/cn/v2/formats") {
  req <- httr::GET(url)

  if (httr::status_code(req) != 200) {
    warning(paste0("Failed to load an up-to-date list of format IDs from ", url, " because the request to the CN failed. Checking of format IDs is disabled."))
    return(vector("character"))
  }

  formats_content <- httr::content(req, encoding = "UTF-8")
  format_id_nodes <- xml2::xml_find_all(formats_content, "//formatId")

  if (length(format_id_nodes) == 0) {
    return(vector("character"))
  }

  vapply(format_id_nodes, function(x) {
    xml2::xml_text(x)
  },
  "")
}

#' Check that the given format is valid
#'
#' Validity is determined by the given format being found in the list on
#' \url{https://cn.dataone.org/cn/v2/formats}.
#'
#' @param format (character) The format ID to check.
#'
#' @return (logical) Whether or not the format was valid.
check_format <- function(format) {
  formats <- get_formats()

  if (!(format %in% formats))
    stop(call. = FALSE,
         paste0("The provided format_id of '",
                format,
                "' is not a valid format ID. Check what you entered against ",
                "the list of format IDs on ",
                "https://cn.dataone.org/cn/v2/formats."))

  invisible(TRUE)
}
