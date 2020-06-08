# @file CohortDefinition
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of ROhdsiWebApi
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Load a cohort definition and insert it into this package \lifecycle{maturing}
#' @details
#' Load a cohort definition from a WebApi instance and insert it into this package. This will fetch
#' the json object and store it in a folder (defaults to 'the inst/cohorts' folder), and fetch the
#' template SQL and store it in another folder (defaults to the 'inst/sql/sql_server' folder). Both
#' folders will be created if they don't exist. When using generateStats = TRUE, the following tables
#' are required to exist when executing the SQL: cohort_inclusion, cohort_inclusion_result,
#' cohort_inclusion_stats, and cohort_summary_stats. Also note that the cohort_inclusion table should
#' be populated with the names of the rules prior to executing the cohort definition SQL. Note:
#' generate inclusion statistics are created for all by default.
#'
#' @template BaseUrl
#' @template CohortId
#' @param name         The name that will be used for the json and SQL files. If not provided, the name
#'                     in cohort will be used, but this may not lead to valid file names.
#' @param jsonFolder   Path to the folder where the JSON representation will be saved.
#' @param sqlFolder    Path to the folder where the SQL representation will be saved.
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
                                            baseUrl) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  object <- getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
  if (is.null(name)) {
    name <- object$name
  }
  if (!file.exists(jsonFolder)) {
    dir.create(jsonFolder, recursive = TRUE)
  }
  jsonFileName <- file.path(jsonFolder, paste(name, "json", sep = "."))
  json <- RJSONIO::toJSON(object$expression, pretty = TRUE)
  SqlRender::writeSql(sql = json, targetFile = jsonFileName)

  writeLines(paste("- Created JSON file:", jsonFileName))

  # Fetch SQL
  sql <- getCohortSql(baseUrl = baseUrl, cohortDefinition = object)
  if (!file.exists(sqlFolder)) {
    dir.create(sqlFolder, recursive = TRUE)
  }
  sqlFileName <- file.path(sqlFolder, paste(name, "sql", sep = "."))
  SqlRender::writeSql(sql = sql, targetFile = sqlFileName)
  writeLines(paste("- Created SQL file:", sqlFileName))
}

#' Insert a set of cohort definitions into package \lifecycle{maturing}
#' @param fileName                Name of a CSV file specifying the cohorts to insert. See details for
#'                                the expected file format.
#' @template BaseUrl
#' @param jsonFolder              Path to the folder where the JSON representations will be saved.
#' @param sqlFolder               Path to the folder where the SQL representations will be saved.
#' @param rFileName               Name of R file to generate when \code{insertCohortCreationR = TRUE}.
#' @param insertTableSql          Should the SQL for creating the cohort table be inserted into the
#'                                package as well? This file will be called CreateCohortTable.sql.
#' @param insertCohortCreationR   Insert R code that will create the cohort table and instantiate the
#'                                cohorts? This will create a file called R/CreateCohorts.R containing
#'                                a function called \code{.createCohorts}.
#' @param packageName             The name of the package (only needed when inserting the R code as
#'                                well).
#'
#' @details
#' The CSV file should have at least the following fields: \describe{ \item{atlasId}{The cohort ID in
#' ATLAS.} \item{cohortId}{The cohort ID that will be used when instantiating the cohort (can be
#' different from atlasId).} \item{name}{The name to be used for the cohort. This name will be used to
#' generate file names, so please use letters and numbers only (no spaces).} }
#'
#' @export
insertCohortDefinitionSetInPackage <- function(fileName = "inst/settings/CohortsToCreate.csv",
                                               baseUrl,
                                               jsonFolder = "inst/cohorts",
                                               sqlFolder = "inst/sql/sql_server",
                                               rFileName = "R/CreateCohorts.R",
                                               insertTableSql = TRUE,
                                               insertCohortCreationR = TRUE,
                                               packageName) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertLogical(insertTableSql, add = errorMessage)
  checkmate::assertLogical(insertCohortCreationR, add = errorMessage)
  checkmate::assertScalar(packageName, add = errorMessage)
  checkmate::assertCharacter(packageName, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  if (insertCohortCreationR && !insertTableSql)
    stop("Need to insert table SQL in order to generate R code")
  if (insertCohortCreationR && jsonFolder != "inst/cohorts")
    stop("When generating R code and generating stats, the jsonFolder must be 'inst/cohorts'")
  if (insertCohortCreationR && sqlFolder != "inst/sql/sql_server")
    stop("When generating R code, the sqlFolder must be 'inst/sql/sql_server'")
  if (insertCohortCreationR && !grepl("inst", fileName))
    stop("When generating R code, the input CSV file must be in the inst folder.")

  cohortsToCreate <- readr::read_csv(fileName, col_types = readr::cols())

  # Inserting cohort JSON and SQL
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Inserting cohort:", cohortsToCreate$name[i]))
    insertCohortDefinitionInPackage(cohortId = cohortsToCreate$atlasId[i],
                                    name = cohortsToCreate$name[i],
                                    baseUrl = baseUrl,
                                    jsonFolder = jsonFolder,
                                    sqlFolder = sqlFolder)
  }

  # Insert SQL to create empty cohort table
  if (insertTableSql) {
    writeLines("Creating SQL to create empty cohort table")
    .insertSqlForCohortTableInPackage(sqlFolder = sqlFolder)
  }

  # Store information on inclusion rules
  writeLines("Storing information on inclusion rules")
  rules <- .getCohortInclusionRules(jsonFolder)
  rules <- merge(rules,
                 data.frame(cohortId = cohortsToCreate$cohortId, cohortName = cohortsToCreate$name))
  csvFileName <- file.path(jsonFolder, "InclusionRules.csv")
  write.csv(rules, csvFileName, row.names = FALSE)
  writeLines(paste("- Created CSV file:", csvFileName))


  # Generate R code to create cohorts
  if (insertCohortCreationR) {
    writeLines("Generating R code to create cohorts")
    templateFileName <- system.file("CreateCohorts.R", package = "ROhdsiWebApi")
    rCode <- readChar(templateFileName, file.info(templateFileName)$size)
    rCode <- gsub("#CopyrightYear#", format(Sys.Date(), "%Y"), rCode)
    rCode <- gsub("#packageName#", packageName, rCode)
    libPath <- gsub(".*inst[/\\]", "", fileName)
    libPath <- gsub("/|\\\\", "\", \"", libPath)
    rCode <- gsub("#fileName#", libPath, rCode)

    rCode <- gsub("#stats_start#", "", rCode)
    rCode <- gsub("#stats_end#", "", rCode)

    fileConn <- file(rFileName)
    writeChar(rCode, fileConn, eos = NULL)
    close(fileConn)
    writeLines(paste("- Created R file:", rFileName))
  }
}

.getCohortInclusionRules <- function(jsonFolder) {
  rules <- data.frame()
  for (file in list.files(path = jsonFolder, pattern = ".*\\.json")) {
    writeLines(paste("Parsing", file, "for inclusion rules"))
    definition <- jsonlite::read_json(file.path(jsonFolder, file))
    if (!is.null(definition$InclusionRules)) {
      nrOfRules <- length(definition$InclusionRules)
      if (nrOfRules > 0) {
        cohortName <- sub(".json", "", file)
        for (i in 1:nrOfRules) {
          rules <- rbind(rules, data.frame(cohortName = cohortName,
                                           ruleSequence = i - 1,
                                           ruleName = definition$InclusionRules[[i]]$name))
        }
      }
    }
  }
  rules
}

.insertSqlForCohortTableInPackage <- function(statsTables = FALSE, sqlFolder) {
  fileName <- system.file("CohortTable.sql", package = "ROhdsiWebApi")
  sql <- readChar(fileName, file.info(fileName)$size)
  if (statsTables) {
    fileName <- system.file("InclusionStatsTables.sql", package = "ROhdsiWebApi")
    sql <- paste(sql, readChar(fileName, file.info(fileName)$size), sep = "\n")
  }
  if (!file.exists(sqlFolder)) {
    dir.create(sqlFolder, recursive = TRUE)
  }
  sqlFileName <- file.path(sqlFolder, "CreateCohortTable.sql")
  fileConn <- file(sqlFileName)
  writeChar(sql, fileConn, eos = NULL)
  close(fileConn)
  writeLines(paste("- Created SQL file:", sqlFileName))
  invisible(sql)
}
