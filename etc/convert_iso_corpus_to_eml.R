#' Convert the ISO corpus to EML
#'
#' I extracted two tasks out of the update_package pipeline:
#'
#' 1. Use an XSLT to translate the ISO to EML
#' 2. Use the R XML package to clean up <individualName> elements


# temp source util.r
source("R/util.R")
# temp
base_dir_inc <- "~/sync/"
base_dir_out <- "~/sync/eml/"

files <- readLines("../metadata/iso-to-eml/iso-files.txt")
full_paths <- sapply(paste0(base_dir_inc, files), path_join)
names(full_paths) <- NULL

# XSLT
iso_to_eml <- xslt::read_xslt("iso2eml.xsl")

# Load up alternate identifiers table
# alternate_identifiers <- read.csv("../identifiers/master_identifiers_list.csv",
#                                   stringsAsFactors = FALSE)

alternate_identifiers <- read.csv("../identifiers/master_identifiers_list.csv",
                                  stringsAsFactors = FALSE)


# Get a temp file we can reuse
tmp <- tempfile()

for (i in seq_along(full_paths)) {
  print(i)

  full_path <- full_paths[i]
  print(full_path)

  # Read the doc in
  doc <- xml2::read_xml(full_paths[i])

  # Translate it and write it back out
  translated <- xslt::xslt_transform(doc, iso_to_eml)
  xml2::write_xml(translated, file = tmp)

  # Substitute the party names
  substitute_eml_party(tmp)


  identifiers_for_file <- alternate_identifiers[alternate_identifiers$file == files[i],]

  if (length(identifiers_for_file) > 0) {
    add_additional_identifiers(tmp, identifiers_for_file)
  }

  # Create a folder to write it out to
  new_path <- stringr::str_replace(full_paths[[i]], base_dir_inc, base_dir_out)
  new_path_parts <- stringr::str_split(new_path, "/")
  new_path_parts <- new_path_parts[[1]][1:(length(new_path_parts[[1]]) - 1)]
  new_path_folder <- paste(new_path_parts, collapse = "/")
  dir.create(new_path_folder, recursive = TRUE)

  # Use `xml fo` on it to get it pretty
  pretty <- system(paste0("/usr/local/bin/xml fo ", tmp), intern = TRUE)
  writeLines(pretty, con = tmp)

  file.copy(from = tmp,
            to = new_path,
            overwrite = FALSE)
}

