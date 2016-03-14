#' modify_metadata.R
#' Author: Bryce Mecum <mecum@nceas.ucsb.edu>
#'
#' Functions related to fixing invalid ISO metadata.
#'
#' Some functions just test whether a validation issue is present. These are
#' prefixed with the text "test". Exactly what they are testing should be
#' described in the docstrings.
#'
#' Other functons fix the bad metadata in place (modifying the original file)
#' and these functions are prefixed with "fix_". Exactly what they are fixing
#' should be described in the docstrings.
#'
#' Example usage:
#'
#' # Find and fix documents in 'mydir' that have extra whitespace in their
#' # topicCategory element(s)
#'
#' the_files <- dir(mydir)
#' bad_enums <- the_files[which(sapply(the_files, test_has_bad_enum))]
#  sapply(bad_enums, fix_bad_enums)


test_has_abstract <- function(path) {
  stopifnot(file.exists(path))

  doc <- try({
    XML::xmlParse(path)
  })

  if (inherits(doc, "try-error")) {
    warning(paste0("Document at path ", path, " failed to parse. Returning TRUE anyway."))
    return(TRUE)
  }

  stopifnot("XMLInternalDocument" %in% class(doc))
  # TODO

  TRUE
}

test_has_bad_topic <- function(path) {
  stopifnot(file.exists(path))

  doc <- try({
    XML::xmlParse(path)
  })

  if (inherits(doc, "try-error")) {
    warning(paste0("Document at path ", path, " failed to parse. Returning FALSE anyway."))
    return(FALSE)
  }

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
    children <- XML::xmlChildren(topic_category_nodes[[1]])

    if (length(children) > 1) {
      return(TRUE)
    }
  }

  # Return FALSE as a fall-through
  FALSE
}

test_has_bad_enum <- function(path) {
  stopifnot(file.exists(path))

  doc <- try({
    XML::xmlParse(path)
  })

  if (inherits(doc, "try-error")) {
    warning(paste0("Document at path ", path, " failed to parse. Returning FALSE anyway."))
    return(FALSE)
  }

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


#' Fix a metadata record with a bad topicCategory.
#'
#' This is the case where the ISO schema says what's inside a
#' gmd:MD_TopicCategoryCode element should match items from a controlled
#' vocabulary. But in the ISO metadata we have, there are newlines and spaces
#' around that text which causes a check for string equality to fail. i.e.
#'
#' 'oceans' != '\n     oceans     \n'
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
fix_bad_enum <- function(path) {
  stopifnot(file.exists(path))

  doc <- XML::xmlParse(path)
  stopifnot("XMLInternalDocument" %in% class(doc))

  # Try to get gmd:topicCategory nodes
  topic_category_nodes <- try({
    XML::getNodeSet(doc, "/gmd:MD_Metadata/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:topicCategory/gmd:MD_TopicCategoryCode")
  }, silent = TRUE)

  # Return FALSE if executing the xPath failed
  if (inherits(topic_category_nodes, "try-error")) {
    return(doc)
  }

  for (i in 1:length(topic_category_nodes)) {
    topic_category_node <- topic_category_nodes[[i]]

    try({
      XML::xmlValue(topic_category_node[[1]]) <- stringr::str_trim(XML::xmlValue(topic_category_node[[1]]))
    }, silent = TRUE)
  }

  writeLines(XML::saveXML(doc), con = path)
}


#' Fix a metadata record with multiple MD_TopicCategory children elements
#' inside a single topicCategory element.
#'
#' Example:
#'
#' <gmd:topicCategory>
#'   <gmd:MD_TopicCategoryCode>imageryBaseMapsEarthCover</gmd:MD_TopicCategoryCode>
#'   <gmd:MD_TopicCategoryCode>oceans</gmd:MD_TopicCategoryCode>
#' </gmd:topicCategory>

#' @param path
#'
#' @return
#' @export
#'
#' @examples
fix_bad_topic <- function(path) {
  stopifnot(file.exists(path))

  doc <- XML::xmlParse(path)
  stopifnot("XMLInternalDocument" %in% class(doc))

  # Get the MD_DataIdentification element so we can search it for topicCategory
  # nodes
  data_ident_nodes <- try({
    XML::getNodeSet(doc, "/gmd:MD_Metadata/gmd:identificationInfo/gmd:MD_DataIdentification")
  }, silent = TRUE)

  # Return FALSE if executing the xPath failed
  if (inherits(data_ident_nodes, "try-error")) {
    return(-1)
  }

  for (i in 1:length(data_ident_nodes)) {
    data_ident_node <- data_ident_nodes[[i]]
    children <- XML::xmlChildren(data_ident_node)

    topic_category_indices <- which(names(children) %in% "topicCategory")

    if (length(topic_category_indices) == 0) {
      next
    }

    for (topic_category_index in topic_category_indices) {
      topic_category_node <- children[[topic_category_index]]

      # If it has more than one child, fix it
      # Extract its values
      # Remove it from the document
      # Insert a new topicCategory for each topic
      topic_category_node_children <- XML::xmlChildren(topic_category_node)

      # Skip this one if it doesn't have multiple children
      if (length(topic_category_node_children) <= 1) {
        next
      }

      # Accumulate topics
      topics <- unlist(lapply(topic_category_node_children, function(x) {
        stringr::str_trim(XML::xmlValue(x))
      }))

      # Due to the ISO schema, we have to respect the order of where
      # the topicCategory node goes in the DataIdentification node. To do this
      # we need to keep the origin topicCategory node, modifying it to only
      # have one topic category inside it, then add the other topic categories
      # as siblings (the siblings is imporrtant here).
      first_topic <- topics[1]
      print(first_topic)
      topic_nodes <- XML::xmlChildren(topic_category_node)

      XML::removeChildren(topic_category_node, kids = seq(1, length(topic_nodes)))

      XML::newXMLNode("gmd:MD_TopicCategoryCode",
                      first_topic,
                      parent = topic_category_node)

      for (topic in topics[2:length(topics)]) {
        print(topic)
        XML::newXMLNode("gmd:topicCategory", XML::newXMLNode("gmd:MD_TopicCategoryCode", topic), sibling = topic_category_node)
      }
    }
  }

  writeLines(XML::saveXML(doc), con = path)

  # Workaround
  #
  # For some reason the XML package adds a dummy namespace even when I add
  # a new node as a sibling to a node that has an ancestor with the namesspace
  # This is a REALLY quick and dirty way to fix that line of code and move
  # on

  lines <- readLines(path)
  pattern <- " xmlns:gmd=\"<dummy>\""
  matched_lines <- grep(pattern, lines)

  for (i in matched_lines) {
    lines[i] <- gsub(pattern, "", lines[i])
  }

  writeLines(lines, con = path)
}


#' Uses XMLStarlet to pretty-print/beautify an XML document.
#'
#' This command just runs `xmlstarlet path > path`, doing a simple
#' pretty-printing of the file located at `path`.
#'
#' Note that this function is doing an in-place pretty printing instead of
#' returning the pretty-printed text.
#'
#' Note that this command uses a temporary file as an intermediate step in the
#' pretty-printing process. For some reason, when running xmlstarlet from within
#' R, the same file can't be used as the input to `xmlstarlet format` and as the
#' shell redirection file (`> somefile.txt`). If you try to run `xmlstarlet
#' format` on the same file as you redirect to, you get a weird parse error from
#' xmlstarlet.
#'
#'
#' @param path Path to your file you want pretty-printed. (character)
#'
#' @return Returns the result of the `system` command (0 = success)
#' @export
#'
#' @examples
pretty_print <- function(path) {
  stopifnot(file.exists(path),
            file.info(path)$size > 0)

  # Detect xmlstarlet
  starlet <- system("which xmlstarlet",
                    ignore.stdout = TRUE)

  if (starlet != 0) {
    stop("Failed to run `xmlstarlet`. Is it in the $PATH?")
  }

  # Use a tempfile because this system command does weird stuff if you
  # try to format and redirect back to the file you're formatting
  tmp_file <- tempfile()
  copy_result <- file.copy(from = path,
                           to = tmp_file)

  if (copy_result != TRUE) {
    stop(paste0("Copy job failed for some reason between ", path, " and ", tmp_file, "."))
  }

  command <- paste0("xmlstarlet format ", tmp_file, " > ", path)

  system_result <- system(command)

  if (system_result != 0) {
    stop(paste0("Result of the command `", command, "` was not zero."))
  }

  system_result
}
