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
#' @param permissions The permissions for the rule (character)
#'
#' @return Whether the update succeeded or the rule was already set (logical)
#' @export
#'
#' @examples
#' # Set write access for bryce on PID "XYZ"
#' set_access_rule(my_mn, "XYZ", "bryce", "write)
set_access_rules <- function(mn, pid, subject, permissions) {
  stopifnot(class(mn) == "MNode",
            is.character(pid),
            nchar(pid) > 0,
            is.character(subject),
            is.character(permissions))

  sysmeta <- dataone::getSystemMetadata(mn,
                                        URLencode(pid, reserved = TRUE))


  # Check if we need to make any changes at all and skip updating the sysmeta
  # if no changes are needed
  if (all(sapply(permissions,
                 function(permission) {
                   datapack::hasAccessRule(sysmeta, subject, permission)
                 }))) {
    cat("All permissions were already set. Skipping update.\n")
    return(TRUE)
  }

  for (permission in permissions) {
    if (datapack::hasAccessRule(sysmeta, subject, permission)) {
      cat(paste0("Skipping the addition of permission '", permission, "'.\n"))
      next
    }

    cat(paste0("Adding permission '", permission, "'\n"))
    sysmeta <- datapack::addAccessRule(sysmeta, subject, permission)
  }


  cat("Updating sysmeta.\n")
  dataone::updateSystemMetadata(mn, pid, sysmeta)
}

#' Set the rightsHolder value for an object.
#'
#' @param mn The Member Node to send the query (MNode)
#' @param pid The PID to set the access rule for (character)
#' @param subject The subject to use as rightsHolder (character)
#'
#' @return Whether the update succeeeded (logical)
#' @export
#'
#' @examples
set_rights_holder <- function(mn, pid, subject) {
  stopifnot(class(mn) == "MNode",
            is.character(pid),
            nchar(pid) > 0,
            is.character(subject))

  sysmeta <- dataone::getSystemMetadata(mn,
                                        URLencode(pid, reserved = TRUE))


  sysmeta@rightsHolder <- subject
  dataone::updateSystemMetadata(mn, pid, sysmeta)
}
