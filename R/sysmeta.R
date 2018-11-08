# Utility functions for modifying System Metadata objects


#' Replace subjects in the accessPolicy section of a System Metadata entries
#'
#' This function was written out to fix capitalization errors in a set of
#' existing System Metadata entries but can be used to replace any subject.
#'
#' @param sysmeta (SystemMetadata) The System Metadata object.
#' @param from (character) The DN string to replace.
#' @param to (character) The DN string to put in place of 'from'.
#'
#' @return (SystemMetadata) The modified System Metadata.
#'
#' @noRd
replace_subject <- function(sysmeta,
                            from = "cn=arctic-data-admins,dc=dataone,dc=org",
                            to = "CN=arctic-data-admins,DC=dataone,DC=org") {
  if (!inherits(sysmeta, "SystemMetadata")) {
    message(paste0("An object of class ", class(sysmeta), " was passed in. Returning unmodified object.\n"))
    return(sysmeta)
  }

  # Get the access policy data.frame
  ap <- sysmeta@accessPolicy

  # Convert subject column from factor to string
  # We do this so we can assign new values to it without dealing with factor
  # nonsense.
  ap$subject <- as.character(ap$subject)

  # Replace the subjects
  ap[which(ap$subject == from),"subject"] <- to
  sysmeta@accessPolicy <- ap

  sysmeta
}


#' Clear the replication policy from a System Metadata object
#'
#' @param sysmeta (SystemMetadata) The System Metadata object to clear the replication policy of.
#'
#' @return (SystemMetadata) The modified System Metadata object.
#'
#' @noRd
clear_replication_policy <- function(sysmeta) {
  if (!(is(sysmeta, "SystemMetadata"))) {
    stop("First argument was not of class SystemMetadata.")
  }

  slot(sysmeta, "replicationAllowed") <- FALSE
  slot(sysmeta, "numberReplicas") <- 0
  slot(sysmeta, "blockedNodes") <- list('urn:node:KNB', 'urn:node:mnUCSB1')

  sysmeta
}


#' Get system metadata for all elements of a data package
#'
#' This function retrieves the system metadata for all elements of a data package and returns them as a list.
#' It is useful for inspecting system metadata for an entire data package and identifying changes where needed.
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID for a resource map.
#' @param nmax (numeric) The maximum number of system metadata objects to return.
#' @param child_packages (logical) If parent package, whether or not to include child packages.
#'
#' @return (list) A list of system metadata objects.
#'
#' @import dataone
#' @importFrom methods is
#' @importFrom methods new
#'
#' @export
#'
#' @examples
#'\dontrun{
#' cn_staging <- CNode("STAGING")
#' adc_test <- getMNode(cn_staging, "urn:node:mnTestARCTIC")
#'
#' rm_pid <- "resource_map_urn:uuid:..."
#'
#' all <- get_all_sysmeta(adc_test, rm_pid)
#'
#' # View in viewer to inspect
#' View(all)
#'
#' # Print specific elements to console
#' all[[1]]@rightsHolder
#'
#' # Create separate object
#' sysmeta_md <- all[[2]]
#' }
get_all_sysmeta <- function(mn, resource_map_pid, nmax = 1000, child_packages = FALSE) {
  stopifnot(methods::is(mn, "MNode"))
  stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0, length(resource_map_pid) == 1)
  stopifnot(is_resource_map(mn, resource_map_pid))
  stopifnot(is.numeric(nmax), length(nmax) == 1 , nmax >= 0)
  stopifnot(is.logical(child_packages), length(child_packages) == 1)

  query_params <- paste("identifier:", resource_map_pid, "+OR+resourceMap:", resource_map_pid, "", sep = "\"")
  response <- dataone::query(mn, list(q = query_params, rows = as.character(nmax)))

  if (length(response) == 0) {
    stop(paste0("No results were found when searching for a package with resource map '", resource_map_pid,
                "'.\nThis could be caused by not having appropriate access to read the resource map."))
  }

  if (length(response) == nmax) {
    warning(paste("Query returned the maximum number of objects. It is possible there are more to retrieve.",
                  "\nSpecify a larger number of objects with the 'nmax' argument."))
  }

  # Check if child package
  if (response[[1]]$formatType == "RESOURCE" && !is.null(response[[1]]$resourceMap)) {
    message("The data package with this resource map is a child package.")
  }
  # Check if parent package
  if (any(unlist(lapply(response[2:length(response)], function(x) ifelse(x$formatType == "RESOURCE", TRUE, FALSE))))) {
    message("The data package with this resource map is a parent package.")
    if (child_packages == TRUE) {
      children <- Filter(function(x) x$formatType == "RESOURCE", response[2:length(response)])
      children2 <- vector("list", length(children))
      for (i in seq_along(children)) {
        child_resource_map_pid <- children[[i]]$identifier
        query_params2 <- paste("identifier:", child_resource_map_pid, "+OR+resourceMap:", child_resource_map_pid, "", sep = "\"")
        children2[[i]] <- dataone::query(mn, list(q = query_params2, rows = as.character(nmax)))
      }
    }
  }

  # Translate fields from Solr query to formal class SystemMetadata
  translate <- function(x) {
    sysmeta <- methods::new("SystemMetadata")

    sysmeta@serialVersion <- sysmeta@serialVersion
    sysmeta@identifier <- if (is.null(x$identifier)) {sysmeta@identifier} else {x$identifier}
    sysmeta@formatId <- if (is.null(x$formatId)) {sysmeta@formatId} else {x$formatId}
    sysmeta@size <- if (is.null(x$size)) {sysmeta@size} else {x$size}
    sysmeta@checksum <- if (is.null(x$checksum)) {sysmeta@checksum} else {x$checksum}
    sysmeta@checksumAlgorithm <- if (is.null(x$checksumAlgorithm)) {sysmeta@checksumAlgorithm} else {x$checksumAlgorithm}
    sysmeta@submitter <- if (is.null(x$submitter)) {sysmeta@submitter} else {x$submitter}
    sysmeta@rightsHolder <- if (is.null(x$rightsHolder)) {sysmeta@rightsHolder} else {x$rightsHolder}
    read <- if (is.null(x$readPermission)) {} else {data.frame(subject = unlist(x$readPermission),
                                                               permission = "read")}
    write <- if (is.null(x$writePermission)) {} else {data.frame(subject = unlist(x$writePermission),
                                                                 permission = "write")}
    change <- if (is.null(x$changePermission)) {} else {data.frame(subject = unlist(x$changePermission),
                                                                   permission = "changePermission")}
    sysmeta@accessPolicy <- rbind(read, write, change)
    sysmeta@replicationAllowed <- if (is.null(x$replicationAllowed)) {sysmeta@replicationAllowed} else {x$replicationAllowed}
    sysmeta@numberReplicas <- if (is.null(x$numberReplicas)) {sysmeta@numberReplicas} else {x$numberReplicas}
    sysmeta@preferredNodes <- if (is.null(x$preferredReplicationMN)) {sysmeta@preferredNodes} else {x$preferredReplicationMN}
    sysmeta@blockedNodes <- if (is.null(x$blockedReplicationMN)) {sysmeta@blockedNodes} else {x$blockedReplicationMN}
    sysmeta@obsoletes <- if (is.null(x$obsoletes)) {sysmeta@obsoletes} else {x$obsoletes}
    sysmeta@obsoletedBy <- if (is.null(x$obsoletedBy)) {sysmeta@obsoletedBy} else {x$obsoletedBy}
    sysmeta@archived <- sysmeta@archived
    sysmeta@dateUploaded <- if (is.null(x$dateUploaded)) {sysmeta@dateUploaded} else {as.character(x$dateUploaded)}
    sysmeta@dateSysMetadataModified <- if (is.null(x$dateModified)) {sysmeta@dateSysMetadataModified} else {as.character(x$dateModified)}
    sysmeta@originMemberNode <- if (is.null(x$datasource)) {sysmeta@originMemberNode} else {x$datasource}
    sysmeta@authoritativeMemberNode <- if (is.null(x$authoritativeMN)) {sysmeta@authoritativeMemberNode} else {x$authoritativeMN}
    sysmeta@seriesId <- if (is.null(x$seriesId)) {sysmeta@seriesId} else {x$seriesId}
    sysmeta@mediaType <- if (is.null(x$mediaType)) {sysmeta@mediaType} else {x$mediaType}
    sysmeta@fileName <- if (is.null(x$fileName)) {sysmeta@fileName} else {x$fileName}
    sysmeta@mediaTypeProperty <- if (is.null(x$mediaTypeProperty)) {sysmeta@mediaTypeProperty} else {x$mediaTypeProperty}

    return(sysmeta)
  }

  if (child_packages) {
    other <- Filter(function(x) x$formatType != "RESOURCE", response[2:length(response)])
    response2 <- c(list(response[[1]]), other)
    parent <- lapply(response2, translate)
    names(parent) <- unlist(lapply(parent, function(x) {x@fileName}))
    for (i in seq_along(parent)) {
      if (is.na(names(parent)[i])) {names(parent)[i] <- paste0("missing_fileName", i)}
    }

    child <- lapply(children2, function(x) {lapply(x, translate)})
    for (i in seq_along(child)) {
      names(child[[i]]) <- unlist(lapply(child[[i]], function(x) {x@fileName}))
      for (j in seq_along(child[[i]])) {
        if (is.na(names(child[[i]])[j])) {names(child[[i]])[j] <- paste0("missing_fileName", j)}
      }
    }
    names(child) <- paste0("child", seq_along(child))

    all <- c(parent, child)
  } else {
    all <- lapply(response, translate)
    names(all) <- unlist(lapply(all, function(x) {x@fileName}))
    for (i in seq_along(all)) {
      if (is.na(names(all)[i])) {names(all)[i] <- paste0("missing_fileName", i)}
    }
  }

  return(all)
}
