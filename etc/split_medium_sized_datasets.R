# Here I split the datasets into datasets with > 100 but <= 1000 objects in them
# and split that into four separate data frames. Separate R processes will then
# chunk away at inserting those objects
#
# I'm doing this splitting because I haven't found the async queueing systems
# reliable enough to use in this scenario.

devtools::load_all(".")

# Load the entire inventory
inventory <- inv_init()
inventory <- inv_load_files(inventory, "../planning/files.txt")
inventory <- inv_load_sizes(inventory, "../planning/sizes.txt")
inventory <- inv_load_checksums(inventory, "../planning/checksums.txt")
inventory <- inv_add_extra_columns(inventory)
inventory <- inv_add_parent_package_column(inventory)
inventory <- theme_packages(inventory)

# Filter to datasets with nfiles > 100 and nfiles <= 1000
small <- inventory[inventory$package_nfiles <= 100,]
medium <- inventory[inventory$package_nfiles > 100 & inventory$package_nfiles <= 1000,]
large <- inventory[inventory$package_nfiles > 1000,]

# Check that the count is right
sum(nrow(small) + nrow(medium) + nrow(large)) == nrow(inventory)
any(duplicated(c(small$file, medium$file, large$file)))

# Filter to just data
medium_data <- medium[medium$is_metadata == FALSE,]
nrow(medium_data)

# Add a created / ready / pid column
medium_data$created <- FALSE
medium_data$ready <- TRUE
medium_data$pid <- ""

# Split into four equal size (in bytes) chunks, maybe just splitting into equal
# size based upon number of files
total_size <- sum(medium_data$size_bytes) / 1024 / 1024 / 1024
size_cumulative <- cumsum(medium_data$size_bytes) / 1024 / 1024 / 1024

break_one <- which(size_cumulative < total_size / 4)
break_one <- break_one[length(break_one)]

break_two <- which(size_cumulative < total_size / 2)
break_two <- break_two[length(break_two)]

break_three <- which(size_cumulative > 3 * total_size / 4)
break_three <- break_three[1]

c(break_one, break_two, break_three)
medium_data_group1 <- medium_data[1:break_one,]
medium_data_group2 <- medium_data[(break_one+1):break_two,]
medium_data_group3 <- medium_data[(break_two+1):break_three,]
medium_data_group4 <- medium_data[(break_three+1):nrow(medium_data),]

sum(medium_data_group1$size_bytes) / 1024 / 1024 / 1024
sum(medium_data_group2$size_bytes) / 1024 / 1024 / 1024
sum(medium_data_group3$size_bytes) / 1024 / 1024 / 1024
sum(medium_data_group4$size_bytes) / 1024 / 1024 / 1024

# Check that the groups are unique sets of files
any(duplicated(c(medium_data_group1$file, medium_data_group2$file, medium_data_group3$file, medium_data_group4$file)))

# Check that none of these files are in the 'data.rda'
load('data/data.rda')
any(duplicated(inventory$file, medium$file))

# Save them out
inventory <- medium_data_group1
save(inventory, file = "data/medium_data_group1.rda")
inventory <- medium_data_group2
save(inventory, file = "data/medium_data_group2.rda")
inventory <- medium_data_group3
save(inventory, file = "data/medium_data_group3.rda")
inventory <- medium_data_group4
save(inventory, file = "data/medium_data_group4.rda")


