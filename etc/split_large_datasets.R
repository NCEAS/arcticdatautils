# Here I split the largest datasets (>1000 files) into nested data packages

devtools::load_all(".")

# Load the entire inventory
inventory <- inv_init()
inventory <- inv_load_files(inventory, "../planning/files.txt")
inventory <- inv_load_sizes(inventory, "../planning/sizes.txt")
inventory <- inv_load_checksums(inventory, "../planning/checksums.txt")
inventory <- inv_add_extra_columns(inventory)
inventory <- inv_add_parent_package_column(inventory)
inventory <- theme_packages(inventory)

large <- inventory[inventory$package_nfiles > 1000,]
length(unique(large$package))
nrow(large)

large_data <- large[large$is_metadata == FALSE,]
nrow(large_data)

splits <- split(1:nrow(large_data), cut(1:nrow(large_data), breaks = 4))

# Give all data files PIDs first
large_data$created <- FALSE
large_data$ready <- TRUE
large_data$pid <- sapply(1:nrow(large_data), function(x) paste0("urn:uuid:", uuid::UUIDgenerate()))

large_data_group1 <- large_data[splits[[1]],]
large_data_group2 <- large_data[splits[[2]],]
large_data_group3 <- large_data[splits[[3]],]
large_data_group4 <- large_data[splits[[4]],]

sum(nrow(large_data_group1), nrow(large_data_group2), nrow(large_data_group3), nrow(large_data_group4)) == nrow(large_data)
any(duplicated(c(large_data_group1$file, large_data_group2$file, large_data_group3$file, large_data_group4$file)))

# Check that none of these files are in the 'data.rda'
load('data/data.rda')
any(duplicated(inventory$file, medium$file))

# Save them out
inventory <- large_data_group1
save(inventory, file = "data/large_data_group1.rda")
inventory <- large_data_group2
save(inventory, file = "data/large_data_group2.rda")
inventory <- large_data_group3
save(inventory, file = "data/large_data_group3.rda")
inventory <- large_data_group4
save(inventory, file = "data/large_data_group4.rda")

