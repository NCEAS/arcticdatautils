#' scratch pad for access.R
#'

library(solr)
library(stringr)
library(dataone)
library(datapack)


Sys.setenv("ARCTICDATA_ENV"="production")
env <- env_load("etc/environment.yml")
pid <- gsub(":", "?", "urn:uuid:78578d5e-f29a-4087-a708-ff31d3e5fad3")

response <- solr_search(q = sprintf("id:%s", pid),
                        fl = "id,resourceMap,documents",
                        rows = 10,
                        base = paste0(env$mn_base_url, "/query/solr"))

str(response)
str_split(response$documents, ",")


pids_to_change <- get_related_pids(env$mn_base_url, "urn:uuid:74af99de-9235-4358-b869-79bec8f3230f")

env$mn <- MNode(env$mn_base_url)
set_access_rule(env$mn, pids_to_change[1], "justaguy", "write")
pids_to_change[1]
