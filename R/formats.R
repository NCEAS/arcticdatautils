# Functions related to data object formats


#' Get the list of valid formats from DataONE
#'
#' Note that this function is intended to return even if the request to the CN
#' fails. This is so other functions can call continue even if the request
#' fails.
#'
#' @param url (character) The listFormats endpoint. Defaults to the production CN.
#'
#' @return (character) A vector of formats.
#'
#' @noRd
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
#' Check that the given format is valid. Validity is determined by the given format
#' being found in the list on <https://cn.dataone.org/cn/v2/formats>.
#'
#' @param format (character) The format ID to check.
#'
#' @return (logical) Whether or not the format was valid.
#'
#' @noRd
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


#' Guess format from filename
#'
#' Guess format from filename for a vector of filenames.
#'
#' @param filenames (character) A vector of filenames.
#'
#' @return (character) DataONE format IDs.
#'
#' @export
#'
#' @examples
#' formatid <- guess_format_id("temperature_data.csv")
guess_format_id <- function(filenames) {
  extensions <- tolower(tools::file_ext(filenames))
  filetypes <- vector(mode = "character", length = length(extensions))

  for (i in seq_len(length(extensions))) {
    extension <- extensions[i]

    if (extension %in% names(dataone_format_mappings)) {
      filetypes[i] <- dataone_format_mappings[extension][[1]]
    } else {
      filetypes[i] <- "application/octet-stream"
    }
  }

  filetypes
}


# List of DataONE formats used in guess_format_id()
dataone_format_mappings <- list("avi" = "video/avi",
                                "bmp" = "image/bmp",
                                "bz2" = "application/x-bzip2",
                                "csv" = "text/csv",
                                "doc" = "application/msword",
                                "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                                "fasta" = "application/x-fasta",
                                "gif" = "image/gif",
                                "gz" = "application/x-gzip",
                                "html" = "text/html",
                                "ipynb" = "application/json",
                                "jp2" = "image/jp2",
                                "jpg" = "image/jpeg",
                                "jpeg" = "image/jpeg",
                                "kml" = "application/vnd.google-earth.kml/xml",
                                "kmz" = "application/vnd.google-earth.kmz",
                                "md" = "text/markdown",
                                "mov" = "video/quicktime",
                                "mp3" = "audio/mpeg",
                                "mp4" = "video/mp4",
                                "mpg" = "video/mpeg",
                                "mpeg" = "video/mpeg",
                                "n3" = "text/n3",
                                "nc" = "netCDF-3",
                                "pdf" = "application/pdf",
                                "png" = "image/png",
                                "ppt" = "application/vnd.ms-powerpoint",
                                "pptx" = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
                                "py" = "application/x-python",
                                "qt" = "video/quicktime",
                                "r" = "application/R",
                                "rar" = "application/x-rar-compressed",
                                "rdf" = "application/rdf/xml",
                                "rmd" = "text/x-rmarkdown",
                                "sas" = "application/SAS",
                                "svg" = "image/svg/xml",
                                "tar" = "application/x-tar",
                                "tif" = "image/tiff",
                                "tiff" = "image/tiff",
                                "ttl" = "text/turtle",
                                "tsv" = "text/tsv",
                                "txt" = "text/plain",
                                "wav" = "audio/x-wav",
                                "wma" = "audio/x-ms-wma",
                                "wmv" = "video/x-ms-wmv",
                                "xls" = "application/vnd.ms-excel",
                                "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                "xml" = "application/xml",
                                "zip" = "application/zip")


#' Determine the format ID for a NetCDF file
#'
#' Determine the DataONE format ID for a NetCDF file provided by path.
#'
#' @param path (character) Full or relative path to the file in question.
#'
#' @return (character) The DataONE format ID.
#'
#' @noRd
get_netcdf_format_id <- function(path) {
  stopifnot(is.character(path),
            nchar(path) > 0,
            file.exists(path))

  if (!requireNamespace("ncdf4")) {
    stop(call. = FALSE,
         "The package 'ncdf4' must be installed to run this function. ",
         "Please install it and try again.")
  }

  # Try to open the file, capturing errors
  cdf_file <- try({
    ncdf4::nc_open(path)
  })

  # If we failed to open the file, we can assume it's not a valid NetCDF file
  # and we just return application/octet-stream as the format ID
  if (inherits(cdf_file, "try-error")) {
    return("application/octet-stream")
  }

  # Since we got this far, continue detecting the format
  stopifnot("format" %in% names(cdf_file))
  format_string <- cdf_file$format
  stopifnot(is.character(format_string),
            nchar(format_string) > 0)
  format_id = ""

  if (format_string == "NC_FORMAT_CLASSIC") {
    format_id = "netCDF-3"
  } else if (format_string == "NC_FORMAT_NETCDF4") {
    format_id = "netCDF-4"
  } else {
    stop("Unknown NetCDF format discovered.")
  }

  return(format_id)
}


#' Test whether an object has a particular format ID
#'
#' Test whether an object has a particular format ID.
#'
#' @param node (MNode|CNode) The Coordinating/Member Node to run the query on.
#' @param pids (character) The PID(s) for objects.
#' @param format_id (character) The format IDs.
#'
#' @return (logical)
#'
#' @noRd
is_format_id <- function(node, pids, format_id) {
  stopifnot(class(node) %in% c("MNode", "CNode"))
  stopifnot(all(is.character(pids)),
            all(lengths(pids) > 0))
  stopifnot(is.character(format_id),
            nchar(format_id) > 0)

  result <- vector("logical", length(pids))

  for (i in seq_along(pids)) {
    result[i] <- dataone::getSystemMetadata(node, pids[i])@formatId == format_id
  }

  result
}


# The following are a set of thin functions which return the DataONE format ID string.
# These are to aid in filling in function arguments and can't remember or don't want to
# type in the full format ID. By putting these format ID strings into
# functions, a user's autocompletion routine in their editor can help them
# fill in the format ID they want.


#' Generate the ISO 19139 format ID
#'
#' Returns the ISO 19139 format ID.
#'
#' @return (character) The format ID for ISO 19139.
#'
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


#' Generate the EML 2.1.1 format ID
#'
#' Returns the EML 2.1.1 format ID.
#' @param version The version of EML ('2.1.1' or '2.2.0')
#'
#' @return (character) The format ID for EML 2.1.1.
#'
#' @export
#'
#' @examples
#' format_eml()
#' \dontrun{
#' # Upload a local EML 2.1.1 file:
#' env <- env_load()
#' publish_object(env$mn, "path_to_some_EML_file", format_eml())
#' }
format_eml <- function(version) {
  if (version %in% c("2.1","2.1.1", "1", 1)){
    "eml://ecoinformatics.org/eml-2.1.1"
  }
  else if (version %in% c("2.2","2.2.0", "2", 2)){
    "https://eml.ecoinformatics.org/eml-2.2.0"
  }
  else print("Please specify a recognized version name, either '2.1.1' or '2.2.0'")
}

#' Generate the EML 2.2.0 format ID
#'
#' Returns the EML 2.2.0 format ID.
#'
#' @return (character) The format ID for EML 2.2.0
#'
#' @export
#'
#' @examples
#' format_eml()
#' \dontrun{
#' # Upload a local EML 2.2.0 file:
#' env <- env_load()
#' publish_object(env$mn, "path_to_some_EML_file", format_eml_220())
#' }
format_eml_220 <- function() {
  "https://eml.ecoinformatics.org/eml-2.2.0"
}
