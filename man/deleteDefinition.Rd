% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetPostDeleteDefinition.R
\name{deleteDefinition}
\alias{deleteDefinition}
\title{Delete a definition id of a chosen category.}
\usage{
deleteDefinition(id, baseUrl, category)
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
None, unless error.
}
\description{
Delete a definition id of a chosen category.
}
\details{
Delete the definition for an id of chosen category in WebApi.
}
\examples{
\dontrun{
deleteDefinition(id = 13242, baseUrl = "http://server.org:80/WebAPI", category = "cohort")
}
}
