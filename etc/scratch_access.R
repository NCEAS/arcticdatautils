#' scratch pad for access.R
#'

library(solr)
library(stringr)
library(dataone)
library(datapack)


Sys.setenv("ARCTICDATA_ENV"="development")
env <- env_load("etc/environment.yml")
env$mn <- MNode(env$mn_base_url)

pid <- gsub(":", "?", "urn:uuid:74af99de-9235-4358-b869-79bec8f3230f")

response <- solr_search(q = sprintf("id:%s", pid),
                        fl = "id,resourceMap,documents",
                        rows = 10,
                        base = paste0(env$mn_base_url, "/query/solr"))

# Testing
me <- "http://orcid.org/0000-0002-0381-3766X"
set_rights_and_access(env$mn, "knb.52.3", me, c("write", "changePermission"))

# Production

pids_to_change <- unique(get_related_pids(env$mn_base_url, "urn:uuid:455247b2-1c50-4cd0-8465-6cb7df463475"))
doi:10.18739/A2KS8D
doi:10.18739/A2MP8P
# me <- "http://orcid.org/0000-0002-0381-3766"
higuera <- "http://orcid.org/0000-0001-5396-9956"

for (pid in pids_to_change) {
  print(pid)
  print(higuera)

  set_access_rules(env$mn, pid, higuera, c("write", "changePermission"))
  set_rights_holder(env$mn, pid, higuera)
}




updated <- read.csv("~/src/arctic-data/inventory/initial_sync/master_updated.csv",
                    stringsAsFactors = FALSE)
target_files <- updated[grep("Integrating", updated$file),]

for (i in seq_len(nrow(target_files))) {
  cat("Setting rules on PID ", target_files[i,"pid"], "\n")

  set_access_rules(env$mn, pid, higuera, c("write", "changePermission"))
  set_rights_holder(env$mn, pid, higuera)
}
