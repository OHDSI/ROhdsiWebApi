% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{existsCohortName}
\alias{existsCohortName}
\title{Check if Cohort definition name exists.}
\usage{
existsCohortName(cohortName, baseUrl)
}
\arguments{
\item{cohortName}{A string name for the Cohort to be checked.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
If found, the function will return a tibble with details of the specification. If not found, FALSE
will be returned.
}
\description{
Check if Cohort definition name exists.
}
\details{
Check if a string name already exists in the WebApi as a Cohort definition name.
}
\examples{
\dontrun{
existsCohortName(cohortName = "this text string needs to be checked",
                 baseUrl = "http://server.org:80/WebAPI")
}
}
