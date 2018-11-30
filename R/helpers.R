# Various helper functions for things like testing a package


#' Create a test metadata object
#'
#' Create a test EML metadata object.
#'
#' @param mn (MNode) The Member Node.
#' @param data_pids (character) Optional. PIDs for data objects the metadata documents.
#'
#' @return (character) The PID of the published metadata document.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' pid <- create_dummy_metadata(mn)
#' }
create_dummy_metadata <- function(mn, data_pids = NULL) {

  # Make sure the node is not a production node
  if (mn@env == "prod") {
    stop('Can not create dummy metadata on production node.')
  }

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  me <- get_token_subject()

  # Copy the original EML file to a temporary place
  original_file <- file.path(system.file(package = "arcticdatautils"),
                             "example-eml.xml")
  metadata_file <- tempfile()
  file.copy(original_file, metadata_file)

  sysmeta <- new("SystemMetadata",
                 id = pid,
                 formatId = "eml://ecoinformatics.org/eml-2.1.1",
                 size = file.size(metadata_file),
                 checksum = digest::digest(metadata_file, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_science_metadata.xml")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating metadata ", pid))
  pid <- dataone::createObject(mn, pid, metadata_file, sysmeta)

  # Remove the temporary EML File
  file.remove(metadata_file)

  pid
}


#' Create a test object
#'
#' Create a test data object.
#'
#' @param mn (MNode) The Member Node.
#'
#' @return (character) The PID of the dummy object.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#'
#' pid <- create_dummy_object(mn)
#'}
create_dummy_object <- function(mn) {

  # Make sure the node is not a production node
  if (mn@env == "prod") {
    stop('Can not create dummy object on production node.')
  }

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  me <- get_token_subject()
  tmp <- tempfile()

  writeLines(paste0(sample(LETTERS, 26, replace = TRUE), collapse = ""), con = tmp)

  sysmeta <- new("SystemMetadata",
                 id = pid,
                 formatId = "application/octet-stream",
                 size = file.size(tmp),
                 checksum = digest::digest(tmp, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_object")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating object ", pid))
  create_response <- dataone::createObject(mn, pid, tmp, sysmeta)

  file.remove(tmp)

  create_response
}


#' Create a test package
#'
#' Create a test data package.
#'
#' @param mn (MNode) The Member Node.
#' @param size (numeric) The number of files in the package, including the metadata file.
#'
#' @return (list) The PIDs for all elements in the data package.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn,"urn:node:mnTestKNB")
#' #Create dummy package with 5 data objects and 1 metadata object
#' pids <- create_dummy_package(mn, 6)
#' }
create_dummy_package <- function(mn, size = 2) {

  # Make sure the node is not a production node
  if (mn@env == "prod") {
    stop('Can not create dummy package on production node.')
  }

  me <- get_token_subject()

  # Data objects
  if (size > 1) {
    data_pids <- sapply(seq_len(size - 1), function(i) {
      create_dummy_object(mn)
    })

    data_pids <- data_pids[!is.na(data_pids)]  # Filter NA pids (failed creates)

  } else {
    data_pids <- NULL
  }

  # Metadata objects
  meta_pid <- create_dummy_metadata(mn, data_pids = data_pids)

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  resmap_path <- generate_resource_map(meta_pid,
                                       data_pids,
                                       resource_map_pid = pid)

  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = "http://www.openarchives.org/ore/terms",
                 size = file.size(resmap_path),
                 checksum = digest::digest(resmap_path, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_resource_map.xml")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating resource map ", pid))
  resource_map_pid <- dataone::createObject(mn, pid, resmap_path, sysmeta)

  list(metadata = meta_pid,
       resource_map = resource_map_pid,
       data = data_pids)
}


#' Create a test parent package
#'
#' Create a test parent data package.
#'
#' @param mn (MNode) The Member Node.
#' @param children (character) Child package (resource maps) PIDs.
#'
#' @return (list) The resource map PIDs for both the parent and child packages.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Set environment
# cn <- CNode("STAGING2")
# mn <- getMNode(cn,"urn:node:mnTestKNB")
#
# child_pid <- "urn:uuid:39a59f99-118b-4c81-9747-4b6c43308e00"
#
# create_dummy_parent_package(mn, child_pid)
#'}
create_dummy_parent_package <- function(mn, children) {

  # Make sure the node is not a production node
  if (mn@env == "prod") {
    stop('Can not create dummy parent package on production node.')
  }

  me <- get_token_subject()
  meta_pid <- create_dummy_metadata(mn)

  pid <- paste0("urn:uuid:", uuid::UUIDgenerate())
  resmap_path <- generate_resource_map(meta_pid,
                                       data_pids = c(),
                                       child_pids = children,
                                       resource_map_pid = pid)

  sysmeta <- new("SystemMetadata",
                 identifier = pid,
                 formatId = "http://www.openarchives.org/ore/terms",
                 size = file.size(resmap_path),
                 checksum = digest::digest(resmap_path, algo = "sha1", serialize = FALSE, file = TRUE),
                 checksumAlgorithm = "SHA1",
                 submitter = me,
                 rightsHolder = me,
                 fileName = "dummy_resource_map.xml")

  # Temporarily clear out the replication policy to work around NCEI not being
  # Tier 4 MN
  sysmeta <- clear_replication_policy(sysmeta)

  sysmeta <- add_admin_group_access(sysmeta)
  sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")

  message(paste0("Creating parent package map ", pid))
  create_response <- createObject(mn, pid, resmap_path, sysmeta)

  list(parent = create_response,
       children = children)
}


#' Create test attributes data.frame
#'
#' Create a test data.frame of attributes.
#'
#' @param numberAttributes (integer) Number of attributes to be created in the table.
#' @param factors (character) Optional vector of factor names to include.
#'
#' @return (data.frame) A data.frame of attributes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create dummy attribute dataframe with 6 attributes and 1 factor
#' attributes <- create_dummy_attributes_dataframe(6, c("Factor1", "Factor2"))
#' }
create_dummy_attributes_dataframe <- function(numberAttributes, factors = NULL) {
  names <- vapply(seq_len(numberAttributes), function(x) { paste0("Attribute ", x)}, "")

  if(!is.null(factors)) {
    domains <- c(rep("textDomain", numberAttributes - length(factors)),
                 rep("enumeratedDomain", length(factors)))
    names[seq((numberAttributes - length(factors) + 1), numberAttributes)] <- factors
  }

  attributes <- data.frame(attributeName = names,
                           attributeDefinition = names,
                           measurementScale = rep("nominal", numberAttributes),
                           domain = domains,
                           formatString = rep(NA, numberAttributes),
                           definition = names,
                           unit = rep(NA, numberAttributes),
                           numberType = rep(NA, numberAttributes),
                           missingValueCode = rep(NA, numberAttributes),
                           missingValueCodeExplanation = rep(NA, numberAttributes),
                           stringsAsFactors = FALSE)

  attributes
}


#' Create test enumeratedDomain data.frame
#'
#' Create a test data.frame of enumeratedDomains.
#'
#' @param factors (character) Vector of factor names to include.
#'
#' @return (data.frame) A data.frame of factors.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create dummy dataframe of 2 factors/enumerated domains
#' attributes <- create_dummy_enumeratedDomain_dataframe(c("Factor1", "Factor2"))
#' }
create_dummy_enumeratedDomain_dataframe <- function(factors) {
  names <- rep(factors, 4)
  enumeratedDomains <- data.frame(attributeName = names,
                                  code = paste0(names, seq_along(names)),
                                  definition = names)

  enumeratedDomains
}


#' Create dummy package with fuller metadata
#'
#' Creates a fuller package than [create_dummy_package()]
#' but is otherwise based on the same concept. This dummy
#' package includes multiple data objects, responsible parties,
#' geographic locations, method steps, etc.
#'
#' @param mn (MNode) The Member Node.
#' @param title (character) Optional. Title of package. Defaults to "A Dummy Package".
#'
#' @return (list) The PIDs for all elements in the data package.
#'
#' @import EML
#' @import dataone
#'
#' @export
create_dummy_package_full <- function(mn, title = "A Dummy Package") {
  stopifnot(is(mn, "MNode"))
  stopifnot(is.character(title), nchar(title) > 0)
  if (mn@env == "prod") {
    stop("Cannot create dummy package on production node.")
  }

  # Create objects
  file.create(c("dummy1.csv", "dummy2.csv", "dummy1.jpg", "dummy1.R"))
  # TODO: add actual data to dummy files

  pid_csv1 <- publish_object(mn,
                             path = "dummy1.csv",
                             format_id = "text/csv")

  pid_csv2 <- publish_object(mn,
                             path = "dummy2.csv",
                             format_id = "text/csv")

  pid_jpg1 <- publish_object(mn,
                             path = "dummy1.jpg",
                             format_id = "image/jpeg")

  pid_R1 <- publish_object(mn,
                           path = "dummy1.R",
                           format_id = "application/R")

  data_pids <- c(pid_csv1, pid_csv2, pid_jpg1, pid_R1)

  # Import EML
  eml_path_original <- file.path(system.file(package = "arcticdatautils"), "example-eml-full.xml")
  eml <- EML::read_eml(eml_path_original)

  # Add objects to EML
  eml@dataset@title[[1]]@.Data <- title

  attr <- data.frame(
    attributeName = c("Date", "Location", "Salinity", "Temperature"),
    attributeDefinition = c("Date sample was taken on", "Location code representing location where sample was taken", "Salinity of sample in PSU", "Temperature of sample"),
    measurementScale = c("dateTime", "nominal","ratio", "interval"),
    domain = c("dateTimeDomain", "enumeratedDomain","numericDomain", "numericDomain"),
    formatString = c("MM-DD-YYYY", NA, NA, NA),
    definition = c(NA,NA, NA, NA),
    unit = c(NA, NA, "dimensionless", "celsius"),
    numberType = c(NA, NA, "real", "real"),
    missingValueCode = c(NA, NA, NA, NA),
    missingValueCodeExplanation = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE)

  location <- c(CASC = "Cascade Lake", CHIK = "Chikumunik Lake", HEAR = "Heart Lake", NISH = "Nishlik Lake")
  fact <- data.frame(attributeName = "Location", code = names(location), definition = unname(location))

  attributeList <- EML::set_attributes(attributes = attr, factors = fact)

  dT1 <- pid_to_eml_entity(mn,
                           pid = pid_csv1,
                           entityType = "dataTable")
  dT1@attributeList <- attributeList

  dT2 <- pid_to_eml_entity(mn,
                           pid = pid_csv2,
                           entityType = "dataTable")
  dT2@attributeList <- attributeList

  eml@dataset@dataTable <- c(dT1, dT2)

  oE1 <- pid_to_eml_entity(mn,
                           pid = pid_jpg1,
                           entityType = "otherEntity")

  oE2 <- pid_to_eml_entity(mn,
                           pid = pid_R1,
                           entityType = "otherEntity")

  eml@dataset@otherEntity <- c(oE1, oE2)

  eml_path <- tempfile(fileext = ".xml")
  EML::write_eml(eml, eml_path)

  pid_eml <- publish_object(mn,
                            path = eml_path,
                            format_id = "eml://ecoinformatics.org/eml-2.1.1")

  # Create resource map
  resource_map_pid <- create_resource_map(mn,
                                          metadata_pid = pid_eml,
                                          data_pids = data_pids)

  file.remove(c("dummy1.csv", "dummy2.csv", "dummy1.jpg", "dummy1.R"), eml_path)

  return(list(resource_map = resource_map_pid,
              metadata = pid_eml,
              data = data_pids))
}


#' Retrieve a name from an ORCID URL
#'
#' Retrieve first and last name from an ORCID URL by scraping the page.
#'
#' @param orcid_url (character) A valid ORCID URL address.
#'
#' @return (character) First and last name.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' pi_name <- get_orcid_name('https://orcid.org/0000-0002-2561-5840')
#' }
get_orcid_name <- function(orcid_url) {
  req <- httr::GET(orcid_url)
  if (req$status_code != 200) {
    stop('Failed to read in ', orcid_url)
  }

  name <- httr::content(req, "text") %>%
    stringr::str_extract("<title>.*<") %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    stringr::str_remove("<title>")

  return(paste(name[1], name[2]))
}


#' List recent submissions to a DataOne Member Node
#'
#' List recent submissions to a DataOne Member Node from all submitters not present
#' in the administrator whitelist: https://cn.dataone.org/cn/v2/accounts/CN=arctic-data-admins,DC=dataone,DC=org
#'
#' @param mn (MNode) A DataOne Member Node
#' @param from (character) the date at which the query begins in 'YYYY/MM/DD' format. Defaults to \code{Sys.Date()}
#' @param to (character) the date at which the query ends in 'YYYY/MM/DD' format.  Defaults to \code{Sys.Date()}
#' @param formatType (character) the format of objects to query. Must be one of: RESOURCE, METADATA, DATA, or *.
#' @param whitelist (character) An xml list of admin orcid Identifiers. Defaults to https://cn.dataone.org/cn/v2/accounts/CN=arctic-data-admins,DC=dataone,DC=org
#'
#' @export
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#'
#' View(arcticdatautils::list_submissions(adc, '2018-10-01', '2018-10-07'))
#'
#' # Return all submitted objects in the past month for the 'adc' node:
#' library(lubridate)
#' View(list_submissions(adc, Sys.Date() %m+% months(-1), Sys.Date(), '*'))
#'
#' # Return all submitted objects except for one user
#' library(lubridate)
#' View(list_submissions(adc, Sys.Date() %m+% months(-1), Sys.Date(), '*'),
#'                       whitelist = 'http://orcid.org/0000-0002-2561-5840')
#'
#' }
list_submissions <- function(mn, from = Sys.Date(), to = Sys.Date(), formatType = '*',
                             whitelist = 'https://cn.dataone.org/cn/v2/accounts/CN=arctic-data-admins,DC=dataone,DC=org') {
  if (!requireNamespace('lubridate', 'purrr', 'RCurl')) {
    stop(call. = FALSE,
         'The packages "lubridate", "purrr", and "RCurl" must be installed to run this function. ',
         'Please install them and try again.')
  }
  stopifnot(methods::is(mn, 'MNode'))
  if (!is_token_set(mn)) {
    stop('No token set')
  }
  if (!(lubridate::is.Date(as.Date(from, '%Y/%M/%D')))){
    stop('"from" argument must be in YYYY/MM/DD format')
  }
  if (!(lubridate::is.Date(as.Date(to, '%Y/%M/%D')))){
    stop('"to" argument must be in YYYY/MM/DD format')
  }
  if (!(formatType %in% c('RESOURCE', 'METADATA', 'DATA', '*'))) {
    stop('formatType must be one of: RESOURCE, METADATA, DATA, or *')
  }

  req <- httr::GET(whitelist)
  if(req$status_code != 200) {
    warning('Failed to read in', whitelist, '. Results will include admin submissions / edits.')
  }
  whitelist <- httr::content(req, "text")

  # Construct query and return results
  q = sprintf('dateUploaded:["%sT00:00:00Z" TO "%sT23:59:59Z"] AND formatType:%s', from, to, formatType)
  results <- dataone::query(mn, list(q = q,
                                     fl = "identifier AND submitter AND dateUploaded AND formatType AND fileName",
                                     rows = 10000),
                            as = "data.frame")

  # Filter out rows where the submitter is in the whitelist
  results <- results[-which(stringr::str_detect(whitelist, results$submitter)),]

  # Return full names based on orcid Id
  results$submitter_name <- purrr::map(results$submitter, get_orcid_name) %>% unlist()

  # Arrange by dateUploaded
  results <- dplyr::arrange(results, dateUploaded)

  return(results)
}
