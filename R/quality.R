#' Score a metadata document against a MetaDIG Suite
#'
#' @param document (eml or character) Either an EML object or path to a file on disk.
#' @param suite_id (character) Optional. Specificy a suite ID. Should be one of https://quality.nceas.ucsb.edu/quality/suites
#'
#' @return (data.frame) A sorted table of Check results
#' @export
#'
#' @examples
#' # Check an EML document you are authoring
#' library(EML)
#' mdq_run(new("eml"))
#'
#' # Check an EML document that is saved to disk
#' mdq_run(system.file("examples", "example-eml-2.1.1.xml", package = "EML"))
mdq_run <- function(document, suite_id = "arctic.data.center.suite.1") {
  if (is(document, "eml")) {
    metadata_path <- tempfile()
    cat("Writing EML document to disk before uploading to the MDQ service. This can take a while.\n")
    EML::write_eml(document, metadata_path)

    if (!file.exists(metadata_path)) {
      stop(call. = FALSE, "Failed to save document to disk for some unknown reason. Try writing the file to disk yourself and re-running this function with a file path instead.")
    }
  } else {
    metadata_path <- document
  }

  req <- httr::POST(paste0("https://quality.nceas.ucsb.edu/quality/suites/", suite_id, "/run"),
              body = list(document = httr::upload_file(metadata_path, "application/xml")),
              encode = "multipart")

  # Remove our temp file if needed
  if (is(document, "eml") && file.exists(metadata_path)) {
    unlink(metadata_path)
  }

  if (req$status_code != 200) {
    print(req)
    stop(call. = FALSE, paste0("Response from server was unexpected."))
  }

  run_result <- httr::content(req)$result
  results <- data.frame(status = vector("character", length(run_result)),
                        description = vector("character", length(run_result)),
                        stringsAsFactors = FALSE)

  for (i in seq_len(length(run_result))) {
    results[i,"status"] <- run_result[[i]]$status
    results[i,"description"] <- run_result[[i]]$check$description

  }

  print(results[order(results$status),], right = FALSE)
}
