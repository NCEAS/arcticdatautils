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

warn_if_outdated <- function() {
  installed_version <- utils::packageVersion("arcticdatautils")

  req <- httr::GET("https://api.github.com/repos/nceas/arcticdatautils/releases/latest",
                   httr::add_headers("Accept", "application/vnd.github.v3+json"))

  release <- httr::content(req)

  if (httr::status_code(req) != 200) {
    warning(paste0("The request to check whether the version of arcticdatautils you have installed is the latest, the response from GitHub's API failed. The response was: \n\n", req))
    return(invisible())
  }

  if (!("tag_name" %in% names(release))) {
    warning(paste0("While checking to see if your installed version of arcticdatautils is the latest available, the response from GitHub's API was not as expected so checking was not done."))
    return(invisible())
  }

  if (!stringr::str_detect(release$tag_name, paste0("^v", stringr::str_replace_all(as.character(installed_version), "\\.", "\\\\."), "$"))) {
    warning(paste0("You do not have the most recent version of arcticdatautils installed. This is just a reminder to update next time you get the chance. Visit https://github.com/NCEAS/arcticdatautils/releases to find the latest release."))
  }
}
