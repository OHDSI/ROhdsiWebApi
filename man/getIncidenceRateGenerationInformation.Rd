% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{getIncidenceRateGenerationInformation}
\alias{getIncidenceRateGenerationInformation}
\title{Get generation information for IncidenceRate id.}
\usage{
getIncidenceRateGenerationInformation(incidenceRateId, baseUrl)
}
\arguments{
\item{incidenceRateId}{An integer id representing the id that uniquely identifies a IncidenceRate
definition in a WebApi instance.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
An R object representing the IncidenceRate definition
}
\description{
Get generation information for IncidenceRate id.
}
\details{
Get generation (execution) information about IncidenceRate for a incidenceRateId.
}
\examples{
\dontrun{
getIncidenceRateGenerationInformation(incidenceRateId = 13242,
                                      baseUrl = "http://server.org:80/WebAPI")
}
}
