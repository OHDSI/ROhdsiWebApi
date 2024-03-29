% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{getCohortGenerationInformation}
\alias{getCohortGenerationInformation}
\title{Get generation information for Cohort id.}
\usage{
getCohortGenerationInformation(cohortId, baseUrl)
}
\arguments{
\item{cohortId}{An integer id representing the id that uniquely identifies a Cohort definition in
a WebApi instance.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
An R object representing the Cohort definition
}
\description{
Get generation information for Cohort id.
}
\details{
Get generation (execution) information about Cohort for a cohortId.
}
\examples{
\dontrun{
getCohortGenerationInformation(cohortId = 13242, baseUrl = "http://server.org:80/WebAPI")
}
}
