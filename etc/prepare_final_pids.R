# Prepare final PIDs for updating what's on production

library(dplyr)


# Load objects master list
inventory <- read.csv("~/src/arctic-data/inventory/master_all.csv",
                      stringsAsFactors = FALSE)
inventory <- inventory[inventory$package_nfiles <= 1000,]

# Load identifier master list
# master <- read.csv("../identifiers/master_identifiers_list.csv",
#                    stringsAsFactors = FALSE)
# table(nchar(master$existing_doi))

# Load specific groups of DOIs
# Gateway
master_gateway <- read.csv("../identifiers/dois_gateway_all36.csv",
                           stringsAsFactors = FALSE)
nrow(master_gateway)

# EOL
master_eol <- read.csv("../identifiers/dois_eol.csv",
                       stringsAsFactors = FALSE)
nrow(master_eol)

# Now join them in
master_both <- rbind(master_gateway,
                     master_eol)
nrow(master_both)

# Load generated DOIs
generated <- readLines("../identifiers/generated-dois.txt")
length(generated)

inventory <- left_join(inventory, master_both[,c("file", "existing_doi")], by = "file")
table(nchar(inventory$existing_doi))

# Find the gateway DATASET indices that don't already have DOIs
gateway_dataset_indices <- grep("acadis-gateway", inventory[inventory$is_metadata == TRUE &
                                                              inventory$depth > 4 &
                                                              is.na(inventory$existing_doi),"file"])


gateway_in_need <- which(inventory$is_metadata & grepl("./acadis-gateway", inventory$file) & inventory$depth > 4 & is.na(inventory$existing_doi))
length(gateway_in_need)
arcss_in_need <- which(inventory$is_metadata & grepl("./acadis-field-projects/ARCSS", inventory$file) & inventory$depth == 5 & is.na(inventory$existing_doi))
length(arcss_in_need)
all(is.na((inventory[gateway_in_need,"existing_doi"])))
all(is.na((inventory[arcss_in_need,"existing_doi"])))

sum(length(gateway_in_need) + length(arcss_in_need)) - length(generated)

# Assign DOIs
inventory[gateway_in_need,"existing_doi"] <- generated[1:length(gateway_in_need)]
inventory[arcss_in_need,"existing_doi"] <- generated[(length(gateway_in_need) + 1):length(generated)]

# Are any we assigned duplciated?
any(duplicated(inventory[!is.na(inventory$existing_doi),"existing_doi"])) # Result: No

# Do all that we identified as needing DOIs have them?
table(nchar(inventory[gateway_in_need,"existing_doi"])) # Result: Yes
table(nchar(inventory[arcss_in_need,"existing_doi"])) # Result: Yes

# Okay, so all files have PIDs
table(nchar(inventory$pid))

# I need to copy those PIDs to pid_old
inventory$pid_old <- inventory$pid

# Then I need to copy in new PIDs
existing_pids_to_bring_in <- which(!is.na(inventory$existing_doi))
inventory[existing_pids_to_bring_in,"pid"] <- inventory[existing_pids_to_bring_in,"existing_doi"]

# And generate new UUID PIDs for the metadata that aren't getting DOIs
metadata_not_dois <- which(inventory$is_metadata == TRUE & !grepl("doi:", inventory$pid))
table(nchar(inventory[metadata_not_dois,"pid"]))

new_uuid_pids <- sapply(1:length(metadata_not_dois), function(x) { paste0("urn:uuid:", uuid::UUIDgenerate())})
inventory[metadata_not_dois,"pid"] <- new_uuid_pids

# Lastly, verify that every data object has the same PID
data_indices <- which(inventory$is_metadata == FALSE)
table(nchar(inventory[data_indices,"pid"])) # Result: All UUIDs
all(inventory[data_indices,"pid"] == inventory[data_indices,"pid_old"])
# result: yes

# and that every metadata object has a new PID
metadata_indices <- which(inventory$is_metadata == TRUE)
all(inventory[metadata_indices,"pid"] != inventory[metadata_indices,"pid_old"])



# Verify that every generated DOI was used
setdiff(generated, inventory[which(inventory$is_metadata),"pid"]) # Result YEP

table(nchar(inventory$pid))
table(nchar(inventory$pid_old))

inventory$updated <- FALSE

write.csv(inventory, file = "../inventory/master_updated.csv")
