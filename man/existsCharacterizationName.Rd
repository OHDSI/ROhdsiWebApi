% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoGeneratedDefinitions.R
\name{existsCharacterizationName}
\alias{existsCharacterizationName}
\title{Check if Characterization definition name exists.}
\usage{
existsCharacterizationName(characterizationName, baseUrl)
}
\arguments{
\item{characterizationName}{A string name for the Characterization to be checked.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}
}
\value{
If found, the function will return a tibble with details of the specification. If not found, FALSE
will be returned.
}
\description{
Check if Characterization definition name exists.
}
\details{
Check if a string name already exists in the WebApi as a Characterization definition name.
}
\examples{
\dontrun{
existsCharacterizationName(characterizationName = "this text string needs to be checked",
                           baseUrl = "http://server.org:80/WebAPI")
}
}
