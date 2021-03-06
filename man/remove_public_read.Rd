% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access.R
\name{remove_public_read}
\alias{remove_public_read}
\title{Remove public read access for an object}
\usage{
remove_public_read(mn, pids)
}
\arguments{
\item{mn}{(MNode) The Member Node.}

\item{pids}{(character) The PIDs of the objects to remove public read access for.}
}
\description{
Remove public read access for an object.
}
\examples{
\dontrun{
cn <- CNode("STAGING2")
mn <- getMNode(cn,"urn:node:mnTestKNB")
pids <- c("urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1",
"urn:uuid:23c7cae4-0fc8-4241-96bb-aa8ed94d71fe")
remove_public_read(mn, pids)
}
}
