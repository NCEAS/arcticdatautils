# This file is a scratch file for testing stuff

devtools::load_all(".")

# Load up the latest inventory files
inv <- inv_init()
inv <- inv_load_files("../planning/files.txt", inv)
inv <- inv_load_sizes("../planning/sizes.txt", inv)
inv <- inv_load_checksums("../planning/checksums.txt", inv)
inv <- inv_add_extra_columns(inv)
