% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetResults.R
\name{getResults}
\alias{getResults}
\title{Get generation results}
\usage{
getResults(id, baseUrl, category)
}
\arguments{
\item{id}{An integer id representing the id that uniquely identifies a 
definition for the category in a WebApi instance.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}

\item{category}{These are the categories in WebApi. The valid string options are 'conceptSet', 
'cohort', 'characterization', 'pathway, 'incidenceRate', 'estimation', 'prediction'.}
}
\value{
Returns the result objects for a given id and category from the WebApi.
}
\description{
Get generation results
}
\details{
Get the results objects from WebApi for a definition of a certain category in WebApi.
}
\examples{
\dontrun{
getResults(id = 282, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
}
}
