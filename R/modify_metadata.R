


test_has_abstract <- function(doc) {
  stopifnot("XMLInternalDocument" %in% class(doc))

  # TODO

  TRUE
}

test_has_bad_topic <- function(path) {
  stopifnot(file.exists(path))

  doc <- XML::xmlParse(path)
  stopifnot("XMLInternalDocument" %in% class(doc))

  # Try to get gmd:topicCategory nodes
  topic_category_nodes <- try({
    XML::getNodeSet(doc, "/gmd:MD_Metadata/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:topicCategory")
  }, silent = TRUE)

  # Return FALSE if executing the xPath failed
  if (inherits(topic_category_nodes, "try-error")) {
    return(FALSE)
  }

  # Return false if there were on gmd:topicCategory nodes
  if (length(topic_category_nodes) == 0) {
    return(FALSE)
  }

  # If we have gmd:topicCategory nodes, scan each one and return TRUE if
  # any of the nodes has more than one child
  for (i in 1:length(topic_category_nodes)) {
    children <- xmlChildren(topic_category_nodes[[1]])

    if (length(children) > 1) {
      return(TRUE)
    }
  }

  # Return FALSE as a fall-through
  FALSE
}

test_has_bad_enum <- function(path) {
  stopifnot(file.exists(path))

  doc <- XML::xmlParse(path)
  stopifnot("XMLInternalDocument" %in% class(doc))

  # Try to get gmd:topicCategory nodes
  topic_category_nodes <- try({
    XML::getNodeSet(doc, "/gmd:MD_Metadata/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:topicCategory/gmd:MD_TopicCategoryCode")
  }, silent = TRUE)

  # Return FALSE if executing the xPath failed
  if (inherits(topic_category_nodes, "try-error")) {
    return(FALSE)
  }

  # Return false if there were on gmd:topicCategory nodes
  if (length(topic_category_nodes) == 0) {
    return(FALSE)
  }

  # If we have gmd:topicCategory nodes, scan each one and return TRUE if
  # any of the nodes has more than one child
  for (i in 1:length(topic_category_nodes)) {
    children <- XML::xmlChildren(topic_category_nodes[[i]])

    for (i in 1:length(children)) {
      child <- children[[i]]
      child_value <- XML::xmlValue(child)

      if (nchar(child_value) != nchar(stringr::str_trim(child_value))) {
        return(TRUE)
      }
    }
  }

  # Return FALSE as a fall-through
  FALSE
}
