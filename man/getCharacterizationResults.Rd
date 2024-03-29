% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{getCharacterizationResults}
\alias{getCharacterizationResults}
\title{Get results for a Characterization Id.}
\usage{
getCharacterizationResults(characterizationId, baseUrl)
}
\arguments{
\item{characterizationId}{An integer id representing the id that uniquely identifies a 
characterization analysis definition in a WebApi instance.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
An R object with results.
}
\description{
Get results for a Characterization Id.
}
\details{
Get the results for Characterization id.
}
\examples{
\dontrun{
getCharacterizationResults(characterizationId = 342, baseUrl = "http://server.org:80/WebAPI")
}
}
