% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eml.R
\name{eml_nsf_to_project}
\alias{eml_nsf_to_project}
\title{Create an EML project section from a list of NSF award numbers}
\usage{
eml_nsf_to_project(awards, eml_version = "2.2")
}
\arguments{
\item{awards}{(list) A list of NSF award numbers as characters}

\item{eml_version}{(char) EML version to use (2.1.1 or 2.2.0)}
}
\value{
project (emld) An EML project section
}
\description{
This function takes a list of NSF award numbers and uses it to
query the NSF API to get the award title, PIs, and coPIs. The
return value is an EML project section. The function supports 1
or more award numbers
}
\examples{
awards <- c("1203146", "1203473", "1603116")

proj <- eml_nsf_to_project(awards, eml_version = "2.1.1")

me <- list(individualName = list(givenName = "Jeanette", surName = "Clark"))

doc <- list(packageId = "id", system = "system",
           dataset = list(title = "A Mimimal Valid EML Dataset",
                          creator = me,
                          contact = me))

doc$dataset$project <- proj

EML::eml_validate(doc)
}
