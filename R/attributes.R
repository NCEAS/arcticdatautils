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
  dims <- nc$dim


  unitlist <- c()
  for (i in 1:length(dims)){
    unitlist[i] <- dims[[i]]$units
  }
  inds <- which(unitlist != '')
  dims <- dims[inds]


  attributes <- c(names(nc$var), attributes(dims)$names)

  result <- data.frame(attributeName=NA)

  for (i in seq_along(attributes)) {
    result[i,"attributeName"] <- attributes[i]
    attribute <- ncdf4::ncatt_get(nc, attributes[i])
    names <- names(attribute)

    for (name in names) {
      result[i, name] <- attribute[[name]]
    }
  }

  result
}

