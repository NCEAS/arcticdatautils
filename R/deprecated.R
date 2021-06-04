# Deprecated functions that will be removed with the next release

#' Create an EML creator.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' Please use the constructors in the EML package instead
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (creator) The new creator.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_creator("test", "user", email = "test@@user.com")
#' eml_creator("creator", "Bryce", "Mecum", userId = "https://orcid.org/0000-0002-0381-3766")
#' eml_creator("creator", c("Dominic", "'Dom'"), "Mullen", c("NCEAS", "UCSB"),
#'             c("Data Scientist", "Programmer"))
#'}
eml_creator <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "eml_creator()", "EML::eml$creator()")
  eml_party("creator", ...)
}


#' Create an EML contact. Contact information is passed on to [eml_party()]
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' Please use the constructors in the EML package instead
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (contact) The new contact.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_contact("test", "user", email = "test@@user.com")
#' eml_creator("creator", "Bryce", "Mecum", userId = "https://orcid.org/0000-0002-0381-3766")
#' eml_creator("creator", c("Dominic", "'Dom'"), "Mullen", c("NCEAS", "UCSB"),
#'             c("Data Scientist", "Programmer"))
#'}
eml_contact <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "eml_contact()", "EML::eml$contact()")
  eml_party("contact", ...)
}

#' Create an EML associatedParty
#'
#' See [eml_party()] for details.
#'
#' @param ... Arguments passed on to [eml_party()].
#'
#' @return (associatedParty) The new associatedParty.
#'
#' @export
#'
#' @examples
#' eml_associated_party("test", "user", email = "test@@user.com", role = "Principal Investigator")
eml_associated_party <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "eml_associated_party()", "EML::eml$associatedParty()")
  eml_party("associatedParty", ...)
}
#' Search through EMLs
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' please use eml_get_simple() and which() together instead
#'
#' This function returns indices within an EML list that contain an instance where
#' `test == TRUE`. See examples for more information.
#'
#' @param doc (list) An EML object.
#' @param element (character) Element to evaluate.
#' @param test (function/character) A function to evaluate (see examples). If test is a character,
#'   will evaluate if \code{element == test} (see example 1).
#'
#' @import EML
#'
#' @export
#'
#' @author Mitchell Maier mitchell.maier@@gmail.com
#'
#' @examples
#' \dontrun{
#' # Question: Which creators have a surName "Smith"?
#' n <- which_in_eml(eml$dataset$creator, "surName", "Smith")
#' # Answer: eml$dataset$creator[n]
#'
#' # Question: Which dataTables have an entityName that begins with "2016"
#' n <- which_in_eml(eml$dataset$dataTable, "entityName", function(x) {grepl("^2016", x)})
#' # Answer: eml$dataset$dataTable[n]
#'
#' # Question: Which attributes in dataTable[[1]] have a numberType "natural"?
#' n <- which_in_eml(eml$dataset$dataTable[[1]]$attributeList$attribute, "numberType", "natural")
#' # Answer: eml$dataset$dataTable[[1]]$attributeList$attribute[n]
#'
#' #' # Question: Which dataTables have at least one attribute with a numberType "natural"?
#' n <- which_in_eml(eml$dataset$dataTable, "numberType", function(x) {"natural" %in% x})
#' # Answer: eml$dataset$dataTable[n]
#' }
which_in_eml <- function(doc, element, test) {

  lifecycle::deprecate_warn("1.0.0", "which_in_eml()", "eml_get_simple()")

  stopifnot(methods::is(doc, "list"))
  stopifnot(is.character(element))

  if (is.character(test)) {
    value = test
    test = function(x) {x == value}

  } else {
    stopifnot(is.function(test))
  }

  elements_test <- eml_get(doc, element)

  if (is.null(elements_test)) {
    location <- NULL

  } else {
    result <- test(elements_test)

    if (length(isTRUE(result)) > 1) {
      stop("Test must only return one value.")

    } else if (length(isTRUE(result)) == 1){
      location <- which(result == TRUE)

    } else {
      location <- NULL
    }
  }
  names(location) <- NULL
  return(location)
}

#' Set a reference to an EML object
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' please add references directly instead
#'
#' This function creates a new object with the same class as \code{element_to_replace}
#' using a reference to \code{element_to_reference}.
#'
#' @param element_to_reference (list) An EML element to reference.
#' @param element_to_replace (list) An EML element to replace with a reference.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' doc <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#'
#' # Set the first contact as a reference to the first creator
#' doc$dataset$contact[[1]] <- eml_set_reference(doc$dataset$creator[[1]],
#' doc$dataset$contact[[1]])
#'
#' # This is also useful when we want to set references to a subset of 'dataTable'
#'   or 'otherEntity' objects
#' # Add a few more objects first to illustrate the use:
#' doc$dataset$dataTable[[3]] <- doc$dataset$dataTable[[1]]
#' doc$dataset$dataTable[[4]] <- doc$dataset$dataTable[[1]]
#' # Add references to the second and third elements only (not the 4th):
#' for (i in 2:3) {
#'     doc$dataset$dataTable[[i]]$attributeList <- eml_set_reference(
#'                                                       doc$dataset$dataTable[[1]]$attributeList,
#'                                                       doc$dataset$dataTable[[i]]$attributeList)
#' }
#' # If we print the entire 'dataTable' list we see elements 2 and 3 have
#' references while 4 does not.
#'
#' doc$dataset$dataTable
#' }
eml_set_reference <- function(element_to_reference, element_to_replace) {

  lifecycle::deprecate_warn("1.0.0", "eml_set_reference()")

  if (length(element_to_reference$id) == 0) {
    stop('No id detected at element_to_reference$id. Please add an id in order to use references.')
  }
  id <- element_to_reference$id[1]
  element_to_replace <- list(references = id)
  return(element_to_replace)
}


#' Set shared attribute references
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' please add references directly instead
#'
#' This function sets shared attributes using the attributes of the first \code{type}
#' selected and creates references for all remaining objects of equivalent \code{type}.
#'
#' @param doc (emld) An EML object.
#' @param attributeList (attributeList) Optional. An EML attributeList object. If not provided
#'   then it will default to the attributeList of the first \code{type} element.
#' @param type (character) Optional. Specifies whether to replace 'dataTable' or 'otherEntity'
#'   attributeList objects with references. Defaults to 'dataTable'.
#'
#' @return (doc) The modified EML document.
#'
#' @author Dominic Mullen dmullen17@@gmail.com
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' doc <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
#' atts <- EML::set_attributes(
#'                      EML::get_attributes(eml$dataset$dataTable[[1]]$attributeList)$attributes)
#'
#' eml <- eml_set_shared_attributes(eml, atts, type = 'dataTable')
#' }
eml_set_shared_attributes <- function(doc, attributeList = NULL, type = 'dataTable') {

  lifecycle::deprecate_warn("1.0.0", "eml_set_reference()")

  stopifnot(methods::is(doc, 'emld'))
  stopifnot(type %in% c('dataTable', 'otherEntity'))

  x <- doc$dataset[[type]]
  n <- length(x)
  if (n <= 1) {
    stop('1 or fewer entities') # add message
  }

  # If a new attributeList is provided set it
  if (!is.null(attributeList)) {
    x[[1]]$attributeList <- attributeList
  }
  x[[1]]$attributeList$id <- stringi::stri_rand_strings(1, length = 10) # generate random identifier
  # Apply references to all other elements
  for (i in 2:n) {
    x[[i]]$attributeList <- eml_set_reference(x[[1]]$attributeList, x[[i]]$attributeList)
  }

  doc$dataset[[type]] <- x
  return(doc)
}

