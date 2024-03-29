% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{detectConceptSetsByName}
\alias{detectConceptSetsByName}
\title{Detect the presence of string matched ConceptSet definitions.}
\usage{
detectConceptSetsByName(pattern, negate = FALSE, baseUrl)
}
\arguments{
\item{pattern}{A pattern to look for. See \link[stringr]{str_detect} for details.}

\item{negate}{If TRUE, return non-matching elements. See \link[stringr]{str_detect} for details.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
FALSE if no matches. If matched - output from \link[ROhdsiWebApi]{getConceptSetDefinitionsMetaData}
}
\description{
Detect the presence of string matched ConceptSet definitions.
}
\details{
Detect string matched ConceptSet definition names from the WebApi, and retrieve metadata
definitions.
}
\examples{
\dontrun{
detectConceptSets(pattern = "this text string to search in pattern",
                  baseUrl = "http://server.org:80/WebAPI")
}
}
