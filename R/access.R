#' access.R
#'
#' Utility functions for getting and setting access rules for DataONE objects.

get_related_pids <- function(mn, pid) {
  stopifnot(is.character(mn),
            is.character(pid),
            nchar(pid) > 0)

  # Escape the PID so we can send it to Solr
  # Here I escape colons only, and with a ? instead of
  pid_esc <- gsub(":", "?", pid)
  # pid_esc <- URLencode(pid, reserved = TRUE)

  response <- solr::solr_search(q = sprintf("id:%s", pid_esc),
                                fl = "identifier,resourceMap,documents",
                                rows = 1000,
                                base = paste0(mn, "/query/solr"))

  if (is.null(response)) {
    warning(paste0("Response was NULL for pid ", pid, "."))
    return(character(length = 0))
  }

  if (!is.data.frame(response)) {
    warning(paste0("Response was not a data.frame but was of class ", class(response), " instead."))
    return(character(length = 0))
  }

  if (nrow(response) != 1) {
    warning(paste0("Response did not have one row, as expected, but had ", nrow(response), " rows."))
    return(character(length = 0))
  }

  cat("\nDebugging response..\n")
  print(response)
  cat("\n\n")

  unlist(c(response$identifier,
           stringr::str_split(response$documents, ","),
           response$resourceMap))
}


#' Set the specified access policy on the given PID.
#'
#' This function takes care of the work of fetching the System Metadata
#' for the given PID, checking if the System Metadata has the specified
#' access policy, adding it to the local copy of the System Metadata if
#' necessary, and updating the SystemMetadata on the specified Member Node.
#'
#' @param mn The Member Node to send the query (MNode)
#' @param pid The PID to set the access rule for (character)
#' @param subject The subject of the rule (character)
#' @param permission The permission for the rule (character)
#'
#' @return Whether the update succeeded or the rule was already set (logical)
#' @export
#'
#' @examples
#' # Set write access for bryce on PID "XYZ"
#' set_access_rule(my_mn, "XYZ", "bryce", "write)
set_access_rule <- function(mn, pid, subject, permission) {
  stopifnot(class(mn) == "MNode",
            is.character(pid),
            nchar(pid) > 0,
            is.character(subject),
            is.character(permission))

  sysmeta <- dataone::getSystemMetadata(mn,
                                        URLencode(pid, reserved = TRUE))

  if (datapack::hasAccessRule(sysmeta, subject, permission)) {
    return(TRUE)
  }

  sysmeta <- datapack::addAccessRule(sysmeta, subject, permission)
  dataone::updateSystemMetadata(mn, pid, sysmeta)
}
