% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{deleteIncidenceRateDefinition}
\alias{deleteIncidenceRateDefinition}
\title{Delete IncidenceRate id definition.}
\usage{
deleteIncidenceRateDefinition(incidenceRateId, baseUrl)
}
\arguments{
\item{incidenceRateId}{An integer id representing the id that uniquely identifies a IncidenceRate
definition in a WebApi instance.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
None, unless error.
}
\description{
Delete IncidenceRate id definition.
}
\details{
Delete the IncidenceRate definition from WebAPI for a given IncidenceRate id
}
\examples{
\dontrun{
deleteIncidenceRateDefinition(incidenceRateId = 13242, baseUrl = "http://server.org:80/WebAPI")
}
}
