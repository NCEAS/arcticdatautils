# Test inserting resource maps
# TODO: Turn this into a test

library(dataone)
devtools::load_all(".")
devtools::load_all("~/src/ropensci-datapackage/")

Sys.setenv("ARCTICDATA_ENV" = "development")
env <- env_load("etc/environment.yml")
env$mn <- MNode(env$mn_base_url)
env$base_path <- "./"

# Make three data files
# Make a metadata file

files <- paste0("./inst/inventory_simple/", dir("./inst/inventory_simple/"))

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)

inv <- inv_add_extra_columns(inv)
inv$is_metadata <- c(FALSE, FALSE, FALSE, TRUE)
inv[which(inv$is_metadata == TRUE),"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- "A"
inv[inv$file == "./inst/inventory_simple/eml_metadata.xml","is_metadata"] <- TRUE
inv$parent_package <- ""
inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""
insert_package(inv, "A", env)




# Now do a metadata-only one
files <- paste0("./inst/inventory_simple/", dir("./inst/inventory_simple/"))

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)

inv <- inv_add_extra_columns(inv)
inv[4,"is_metadata"] <- TRUE
inv[which(inv$is_metadata == TRUE),"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- "A"
inv[inv$file == "./inst/inventory_simple/eml_metadata.xml","is_metadata"] <- TRUE
inv$parent_package <- ""
inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""
inv <- inv[which(inv$is_metadata == TRUE),]
insert_package(inv, "A", env)


# Now do nested ones
files <- paste0("./inst/inventory_simple/", dir("./inst/inventory_simple/"))

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)
inv <- rbind(inv,
             inv,
             inv)

inv <- inv_add_extra_columns(inv)
inv$is_metadata <- c(FALSE, FALSE, FALSE, TRUE)
inv[which(inv$is_metadata == TRUE),"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- c(rep("A", 4), rep("B", "4"), rep("C", 4))
inv$parent_package <- c(rep("", 4), rep("A", "4"), rep("B", 4))
inv[inv$file == "./inst/inventory_simple/eml_metadata.xml","is_metadata"] <- TRUE
inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""

last <- insert_package(inv, "C", env)
inv[which(inv$package == "C"),"created"] <- last$created
inv[which(inv$package == "C"),"pid"] <- last$pid

last <- insert_package(inv, "B", env)
inv[which(inv$package == "B"),"created"] <- last$created
inv[which(inv$package == "B"),"pid"] <- last$pid

last <- insert_package(inv, "A", env)



# Now do a nested one where the child one is metadata-only
files <- paste0("./inst/inventory_simple/", dir("./inst/inventory_simple/"))

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)
inv <- rbind(inv,
             inv,
             inv)

inv <- inv_add_extra_columns(inv)
inv$is_metadata <- c(FALSE, FALSE, FALSE, TRUE)
inv[which(inv$is_metadata == TRUE),"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- c(rep("A", 4), rep("B", "4"), rep("C", 4))
inv$parent_package <- c(rep("", 4), rep("A", "4"), rep("B", 4))
inv[inv$file == "./inst/inventory_simple/eml_metadata.xml","is_metadata"] <- TRUE
inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""

# Filter out C's data files
inv <- inv[inv$package == "C" & inv$is_metadata == TRUE | inv$package %in% c("A", "B"),]

last <- insert_package(inv, "C", env)
inv[which(inv$package == "C"),"created"] <- last$created
inv[which(inv$package == "C"),"pid"] <- last$pid

last <- insert_package(inv, "B", env)
inv[which(inv$package == "B"),"created"] <- last$created
inv[which(inv$package == "B"),"pid"] <- last$pid

last <- insert_package(inv, "A", env)

# Now do a nested one where the top-level parent is metadata-only
files <- paste0("./inst/inventory_simple/", dir("./inst/inventory_simple/"))

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)
inv <- rbind(inv,
             inv,
             inv)

inv <- inv_add_extra_columns(inv)
inv$is_metadata <- c(FALSE, FALSE, FALSE, TRUE)
inv[which(inv$is_metadata == TRUE),"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- c(rep("A", 4), rep("B", "4"), rep("C", 4))
inv$parent_package <- c(rep("", 4), rep("A", "4"), rep("B", 4))
inv[inv$file == "./inst/inventory_simple/eml_metadata.xml","is_metadata"] <- TRUE
inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""

inv <- inv[inv$package %in% c("A") & inv$is_metadata == TRUE | inv$package %in% c("B", "C"),]

last <- insert_package(inv, "C", env)
inv[which(inv$package == "C"),"created"] <- last$created
inv[which(inv$package == "C"),"pid"] <- last$pid

last <- insert_package(inv, "B", env)
inv[which(inv$package == "B"),"created"] <- last$created
inv[which(inv$package == "B"),"pid"] <- last$pid

last <- insert_package(inv, "A", env)

# Now do a nested one where the middle is metadata-only
files <- paste0("./inst/inventory_simple/", dir("./inst/inventory_simple/"))

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)
inv <- rbind(inv,
             inv,
             inv)

inv <- inv_add_extra_columns(inv)
inv$is_metadata <- rep(c(FALSE, FALSE, FALSE, TRUE), 3)
inv[which(inv$is_metadata == TRUE),"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- c(rep("A", 4), rep("B", "4"), rep("C", 4))
inv$parent_package <- c(rep("", 4), rep("A", "4"), rep("B", 4))
inv[inv$file == "./inst/inventory_simple/eml_metadata.xml","is_metadata"] <- TRUE
inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""

# Remove data from A and B
inv <- inv[inv$package %in% c("B") & inv$is_metadata == TRUE | inv$package %in% c("A", "C"),]

last <- insert_package(inv, "C", env)
inv[which(inv$package == "C"),"created"] <- last$created
inv[which(inv$package == "C"),"pid"] <- last$pid

last <- insert_package(inv, "B", env)
inv[which(inv$package == "B"),"created"] <- last$created
inv[which(inv$package == "B"),"pid"] <- last$pid

last <- insert_package(inv, "A", env)


#' Insert an even more complicated one
#' A
#'   B
#'     E
#'     F
#'   C
#'   D

inv <- data.frame(file = files,
                  size_bytes = file.size(files),
                  checksum_sha256 = sapply(files, function(file) { digest::digest(file, algo="sha256")}),
                  stringsAsFactors = FALSE)
inv <- rbind(inv,
             inv,
             inv,
             inv,
             inv,
             inv)
inv <- inv_add_extra_columns(inv)
inv$is_metadata <- rep(c(FALSE, FALSE, FALSE, TRUE), 6)
inv[inv$is_metadata == TRUE,"format_id"] <- "eml://ecoinformatics.org/eml-2.1.1"
inv$package <- c(rep("A", 4),
                 rep("B", 4),
                 rep("C", 4),
                 rep("D", 4),
                 rep("E", 4),
                 rep("F", 4))

inv$parent_package <- c(rep("", 4),
                        rep("A", 4),
                        rep("A", 4),
                        rep("A", 4),
                        rep("B", 4),
                        rep("B", 4))

inv$created <- FALSE
inv$ready <- TRUE
inv$pid <- ""

last <- insert_package(inv, "E", env)
inv[which(inv$package == "E"),"created"] <- last$created
inv[which(inv$package == "E"),"pid"] <- last$pid

last <- insert_package(inv, "F", env)
inv[which(inv$package == "F"),"created"] <- last$created
inv[which(inv$package == "F"),"pid"] <- last$pid

last <- insert_package(inv, "B", env)
inv[which(inv$package == "B"),"created"] <- last$created
inv[which(inv$package == "B"),"pid"] <- last$pid

last <- insert_package(inv, "C", env)
inv[which(inv$package == "C"),"created"] <- last$created
inv[which(inv$package == "C"),"pid"] <- last$pid

last <- insert_package(inv, "D", env)
inv[which(inv$package == "D"),"created"] <- last$created
inv[which(inv$package == "D"),"pid"] <- last$pid

last <- insert_package(inv, "A", env)
inv[which(inv$package == "A"),"created"] <- last$created
inv[which(inv$package == "A"),"pid"] <- last$pid
