% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ontology.R
\name{get_ontology_concepts}
\alias{get_ontology_concepts}
\title{Gets all the concepts}
\usage{
get_ontology_concepts(ontology)
}
\arguments{
\item{ontology}{(list) the list form of a OWL file}
}
\value{
dataframe
}
\description{
Takes an ontology and returns a dataframe with all the URIs and labels.
This is mainly used for MOSAiC because the ontology is modeled differently
}
\examples{

mosaic <- read_ontology("mosaic")
get_ontology_concepts(mosaic)
}
