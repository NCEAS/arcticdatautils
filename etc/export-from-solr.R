# Export content from Solr to verify my inserts
# I can't insert resource maps until I have confirmed every file that should
# have been inserted was and that I have the correct PID for it.

library(stringr)

# Create a set of URLs to download
base_url <- "https://arcticdata.io/metacat/d1/mn/v2/query/solr/?q=id:*&fl=identifier,checksum,fileName&wt=csv&rows=1000&start="
start_values <- seq(0, 488652, by = 1000) # 488652 objects in solr right now
urls <- paste0(base_url, format(start_values, scientific = FALSE, trim = TRUE)) # Set sci and trim!

# Download all the files to disk
sapply(1:length(urls), function(x) { download.file(urls[x], destfile = paste0("solr-page-", x, ".csv")) } )

# Grab the csv files from disk
csvs <- dir(pattern = "solr-page-\\d+\\.csv")
length(csvs)
length(urls)

missed_pages <- setdiff(1:length(start_values), as.numeric(str_extract(csvs, "\\d+")))
missed_pages

# Download the missed pages
sapply(missed_pages, function(x) { download.file(urls[x], destfile = paste0("solr-page-", x, ".csv")) } )

# Read and merge
x <- do.call("rbind", lapply(csvs, read.csv, stringsAsFactors = FALSE))
