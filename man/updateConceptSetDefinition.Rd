% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{updateConceptSetDefinition}
\alias{updateConceptSetDefinition}
\title{Update a ConceptSet definition.}
\usage{
updateConceptSetDefinition(conceptSetDefinition, baseUrl)
}
\arguments{
\item{conceptSetDefinition}{An R list object containing the expression for the specification. Must
include id, name and expression. This will be converted to JSON
expression by function and posted into the WebApi. The definition will
be checked against the WebApi instance for errors}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\description{
Update a ConceptSet definition.
}
\details{
Update a ConceptSet definition.
}
\examples{
\dontrun{
definition <- getConceptSetDefinition(id = 13242,
                                      baseUrl = "http://server.org:80/WebAPI",
                                      category = conceptSet)
definition$name <- "My new name for this"
updateConceptSet(conceptSetDefinition, baseUrl, category = "cohort")
}
}
