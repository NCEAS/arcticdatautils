.onLoad <- function(libname, pkgname) {
  load_d1_formats_list("https://cn.dataone.org/cn/v2/formats")

  invisible()
}

# Warning: This function produces a side-effect of assigning into the package
# env with <<- to update the value of D1_FORMATS
D1_FORMATS <- NULL
load_d1_formats_list <- function(url) {
  req <- httr::GET(url)

  if (httr::status_code(req) != 200) {
    warning(paste0("Failed to load an up-to-date list of format IDs from ", url, " because the request to the CN failed. Checking of format IDs is disabled."))
    return(invisible())
  }

  formats_content <- httr::content(req, encoding = "UTF-8")
  format_id_nodes <- xml2::xml_find_all(formats_content, "//formatId")

  if (length(format_id_nodes) == 0) {
    warning(paste0("Failed to load an up-to-date list of format IDs from ", url, " because the response returned from the CN was not formatted as expected. Checking of format IDs is disabled."))
    return(invisible())
  }

  D1_FORMATS <<- vapply(format_id_nodes, function(x) { xml2::xml_text(x) }, "")
}
