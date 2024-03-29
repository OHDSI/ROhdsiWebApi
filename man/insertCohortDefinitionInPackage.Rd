% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StudyPackage.R
\name{insertCohortDefinitionInPackage}
\alias{insertCohortDefinitionInPackage}
\title{Load a cohort definition and insert it into this package}
\usage{
insertCohortDefinitionInPackage(
  cohortId,
  name = NULL,
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  baseUrl,
  generateStats = FALSE
)
}
\arguments{
\item{cohortId}{An integer id representing the id that uniquely identifies a cohort definition
in a WebApi instance.}

\item{name}{The name that will be used for the JSON and SQL files. If not provided, the
name in cohort will be used, but this may not lead to valid file names.}

\item{jsonFolder}{Path to the folder where the JSON representation will be saved.}

\item{sqlFolder}{Path to the folder where the SQL representation will be saved.}

\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}

\item{generateStats}{Should the SQL include the code for generating inclusion rule statistics?
Note that if TRUE, several additional tables are expected to exists as
described in the details.}
}
\description{
Load a cohort definition and insert it into this package
}
\details{
Load a cohort definition from a WebApi instance and insert it into this package. This will fetch
the JSON object and store it in a folder (defaults to 'the inst/cohorts' folder), and fetch the
template SQL and store it in another folder (defaults to the 'inst/sql/sql_server' folder). Both
folders will be created if they don't exist. When using generateStats = TRUE, the following tables
are required to exist when executing the SQL: cohort_inclusion, cohort_inclusion_result,
cohort_inclusion_stats, and cohort_summary_stats. Also note that the cohort_inclusion table should
be populated with the names of the rules prior to executing the cohort definition SQL. Note:
generate inclusion statistics are created for all by default.
}
\examples{
\dontrun{
# This will create 'inst/cohorts/Angioedema.json' and 'inst/sql/sql_server/Angioedema.sql':

insertCohortDefinitionInPackage(cohortId = 282,
                                name = "Angioedema",
                                baseUrl = "http://server.org:80/WebAPI")
}

}
