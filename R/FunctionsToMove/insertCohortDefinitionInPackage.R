
#' Load a cohort definition and insert it into this package \lifecycle{maturing}
#' @details
#' Load a cohort definition from a WebApi instance and insert it into this package. This will fetch
#' the JSON object and store it in a folder (defaults to 'the inst/cohorts' folder), and fetch the
#' template SQL and store it in another folder (defaults to the 'inst/sql/sql_server' folder). Both
#' folders will be created if they don't exist. When using generateStats = TRUE, the following tables
#' are required to exist when executing the SQL: cohort_inclusion, cohort_inclusion_result,
#' cohort_inclusion_stats, and cohort_summary_stats. Also note that the cohort_inclusion table should
#' be populated with the names of the rules prior to executing the cohort definition SQL. Note:
#' generate inclusion statistics are created for all by default.
#'
#' @template BaseUrl
#' @template CohortId
#' @param name            The name that will be used for the JSON and SQL files. If not provided, the
#'                        name in cohort will be used, but this may not lead to valid file names.
#' @param jsonFolder      Path to the folder where the JSON representation will be saved.
#' @param sqlFolder       Path to the folder where the SQL representation will be saved.
#' @param generateStats   Should the SQL include the code for generating inclusion rule statistics?
#'                        Note that if TRUE, several additional tables are expected to exists as
#'                        described in the details.
#'
#' @examples
#' \dontrun{
#' # This will create 'inst/cohorts/Angioedema.json' and 'inst/sql/sql_server/Angioedema.sql':
#'
#' insertCohortDefinitionInPackage(cohortId = 282,
#'                                 name = "Angioedema",
#'                                 baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
insertCohortDefinitionInPackage <- function(cohortId,
                                            name = NULL,
                                            jsonFolder = "inst/cohorts",
                                            sqlFolder = "inst/sql/sql_server",
                                            baseUrl,
                                            generateStats = FALSE) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertLogical(generateStats, add = errorMessage)
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  object <- getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
  if (is.null(name)) {
    name <- object$name %>% as.character() %>% trimws()
  }
  if (!file.exists(jsonFolder)) {
    dir.create(jsonFolder, recursive = TRUE)
  }
  jsonFileName <- file.path(jsonFolder, paste(name, "json", sep = "."))
  json <- .toJSON(object$expression, pretty = TRUE)
  SqlRender::writeSql(sql = json, targetFile = jsonFileName)
  
  writeLines(paste("- Created JSON file:", jsonFileName))
  
  # Fetch SQL
  sql <- getCohortSql(baseUrl = baseUrl, cohortDefinition = object, generateStats = generateStats)
  if (!file.exists(sqlFolder)) {
    dir.create(sqlFolder, recursive = TRUE)
  }
  sqlFileName <- file.path(sqlFolder, paste(name, "sql", sep = "."))
  SqlRender::writeSql(sql = sql, targetFile = sqlFileName)
  writeLines(paste("- Created SQL file:", sqlFileName))
}