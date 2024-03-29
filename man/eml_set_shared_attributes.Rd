% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{eml_set_shared_attributes}
\alias{eml_set_shared_attributes}
\title{Set shared attribute references}
\usage{
eml_set_shared_attributes(doc, attributeList = NULL, type = "dataTable")
}
\arguments{
\item{doc}{(emld) An EML object.}

\item{attributeList}{(attributeList) Optional. An EML attributeList object. If not provided
then it will default to the attributeList of the first \code{type} element.}

\item{type}{(character) Optional. Specifies whether to replace 'dataTable' or 'otherEntity'
attributeList objects with references. Defaults to 'dataTable'.}
}
\value{
(doc) The modified EML document.
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
}
\details{
please add references directly instead

This function sets shared attributes using the attributes of the first \code{type}
selected and creates references for all remaining objects of equivalent \code{type}.
}
\examples{
\dontrun{
cn <- dataone::CNode('PROD')
adc <- dataone::getMNode(cn,'urn:node:ARCTIC')
doc <- EML::read_eml(dataone::getObject(adc, 'doi:10.18739/A2S17SS1M'))
atts <- EML::set_attributes(
                     EML::get_attributes(eml$dataset$dataTable[[1]]$attributeList)$attributes)

eml <- eml_set_shared_attributes(eml, atts, type = 'dataTable')
}
}
\author{
Dominic Mullen dmullen17@gmail.com
}
