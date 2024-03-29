% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{cancelPathwayGeneration}
\alias{cancelPathwayGeneration}
\title{Cancel generation of Pathway id.}
\usage{
cancelPathwayGeneration(pathwayId, baseUrl, sourceKey)
}
\arguments{
\item{pathwayId}{An integer id representing the id that uniquely identifies a Pathway definition
in a WebApi instance.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}

\item{sourceKey}{The source key for a CDM instance in WebAPI, as defined in the
Configuration page}
}
\value{
A tibble with job status information.
}
\description{
Cancel generation of Pathway id.
}
\details{
Cancel the generation of Pathway id in the WebApi.
}
\examples{
\dontrun{
cancelPathwayGeneration(pathwayId = 13242,
                        baseUrl = "http://server.org:80/WebAPI",
                        sourceKey = "HCUP")
}
}
