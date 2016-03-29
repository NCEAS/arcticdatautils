# This file is a scratch file for testing stuff

devtools::load_all(".")

# Load up the latest inventory files
inv <- inv_init()
inv <- inv_load_files(inv, "../planning/files.txt")
inv <- inv_load_sizes(inv, "../planning/sizes.txt")
inv <- inv_load_checksums(inv, "../planning/checksums.txt")
inv <- inv_add_extra_columns(inv)
inv <- inv_add_parent_package_column(inv)
inv <- theme_packages(inv, nfiles_cutoff = 1000)
