# Get the number of objects in Solr and put it to disk

library(xml2)
library(dplyr)

num_found <- read_xml("https://arcticdata.io/metacat/d1/mn/v2/query/solr/?q=id:*&rows=0") %>% xml_find_one("//result") %>% xml_attr("numFound")
now <- as.POSIXlt(Sys.time(), "GMT")
write(paste(now, num_found, sep = ","), file = "status.txt", append = TRUE)
