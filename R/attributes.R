#' Get a data.frame of attributes from a NetCDF object
#'
#' @param nc (ncdf4 or character) Either a ncdf4 object or a file path
#'
#' @return (data.frame) A data.frame of the attributes
#' @export
#'
#' @examples
#' \dontrun{
#' get_ncdf4_attributes("./path/to/my.nc")
#' }
get_ncdf4_attributes <- function(nc) {
  stopifnot(is(nc, "ncdf4") || file.exists(nc))

  # Read the file in if `nc` is a character vector
  if (is.character(nc)) {
    nc <- ncdf4::nc_open(nc)
  }

  attributes <- names(nc$var)
  result <- data.frame()

  for (i in seq_along(attributes)) {
    attribute <- ncdf4::ncatt_get(nc, attributes[i])
    names <- names(attribute)

    for (name in names) {
      result[i, name] <- attribute[[name]]
    }
  }

  result
}

