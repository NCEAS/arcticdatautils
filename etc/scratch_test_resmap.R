#' child one 4ac68b8fbe2a0a2fea11d04ab806320e67068716
#' child two cfc98378a65c1b30d88e47214347fcf9a8b92465
#' child three 611c6a92901a4aec20e3b3d010d2e782d109197d
#' parent is 6397c2b708fbda6eeb8b6cb8ff021d93ee36579a

devtools::load_all(".")
devtools::load_all("~/src/ropensci-datapackage/")

###########################

# inv <- read.csv("../inv.csv", stringsAsFactors = FALSE)

###########################

inv <- inv_init()
inv <- inv_load_files(inv, "~/src/arctic-data/planning/files.txt")
inv <- inv_load_sizes(inv, "~/src/arctic-data/planning/sizes.txt")
inv <- inv_load_checksums(inv, "~/src/arctic-data/planning/checksums.txt")
inv <- inv_add_extra_columns(inv)
inv <- inv_add_parent_package_column(inv)
inv <- theme_packages(inv)

inv$pid <- ""
inv$created <- FALSE
inv$ready <- TRUE

write.csv(inv, file = "../inv.csv")
save(inv, file = "../inv.rda")

###########################

# Filter
test <- inv[grep("understanding_the_physical_properties", inv$file),]
test <- test[test$size_bytes < 10 * 1024 * 1024,] # Filter out large files

# Child pkg 1
last <- insert_package(test, "4ac68b8fbe2a0a2fea11d04ab806320e67068716")
test <- inv_update(test, last)

last <- insert_package(test, "cfc98378a65c1b30d88e47214347fcf9a8b92465")
test <- inv_update(test, last)

last <- insert_package(test, "611c6a92901a4aec20e3b3d010d2e782d109197d")
test <- inv_update(test, last)

# Insert the parent finally
last <- insert_package(test, "6397c2b708fbda6eeb8b6cb8ff021d93ee36579a")
test <- inv_update(test, last)

test



# Fixing my resmap bug
last <- insert_package(test, "4ac68b8fbe2a0a2fea11d04ab806320e67068716")


devtools::load_all(".")
devtools::load_all("~/src/ropensci-datapackage/")
readLines(generate_resource_map("urn:uuid:X-X-X-X", c("urn:uuid:Y-Y-Y-Y"), c("urn:uuid:CHILD")))
