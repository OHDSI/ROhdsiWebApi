% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{isValidCharacterizationId}
\alias{isValidCharacterizationId}
\title{is Characterization id a valid definition in the WebApi.}
\usage{
isValidCharacterizationId(characterizationIds, baseUrl)
}
\arguments{
\item{characterizationIds}{A list of integer id(s) of the Characterization to be tested for
validity.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
A logical vector indicating if an ID is valid.
}
\description{
is Characterization id a valid definition in the WebApi.
}
\details{
Checks if a set of id for a Characterization is valid. The following checks are performed. 1)
checks if all the ids exists in the WebApi i.e. valid.
}
\examples{
\dontrun{
isValidCharacterizationId(characterizationIds = c(13242, 3423, 34),
                          baseUrl = "http://server.org:80/WebAPI")
}
}
