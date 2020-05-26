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

#' Get a cohort definition
#'
#' @details
#' Obtain the cohort definition from WebAPI for a given cohort id
#'
#' @template BaseUrl
#' @template CohortId
#' 
#' @return
#' An R object representing the cohort definition
#'
#' @examples
#' \dontrun{
#' getCohortDefinition(cohortId = 282, baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getCohortDefinition <- function(cohortId, baseUrl) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste(baseUrl, "cohortdefinition", cohortId, sep = "/")
  json <- httr::GET(url)
  data <- httr::content(json)
  if (!is.null(data$payload$message)) {
    stop(data$payload$message)
  }
  data$expression <- RJSONIO::fromJSON(data$expression)
  return(data)
}


#' Load a cohort definition and insert it into this package
#'
#' @details
#' Load a cohort definition from a WebApi instance and insert it into this package. This will fetch
#' the json object and store it in a folder (defaults to 'the inst/cohorts' folder), and fetch the
#' template SQL and store it in another folder (defaults to the 'inst/sql/sql_server' folder). Both
#' folders will be created if they don't exist.
#' When using generateStats = TRUE, the following tables are required to exist when executing the SQL:
#' cohort_inclusion, cohort_inclusion_result, cohort_inclusion_stats, and cohort_summary_stats. Also
#' note that the cohort_inclusion table should be populated with the names of the rules prior to
#' executing the cohort definition SQL.
#'
#' @template BaseUrl
#' @template CohortId
#' @param name            The name that will be used for the json and SQL files. If not provided, the
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
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::assertLogical(generateStats, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  object <- getCohortDefinition(cohortId = cohortId, 
                                baseUrl = baseUrl)
  if (is.null(name)) {
    name <- object$name
  }
  if (!file.exists(jsonFolder)) {
    dir.create(jsonFolder, recursive = TRUE)
  }
  jsonFileName <- file.path(jsonFolder, paste(name, "json", sep = "."))
  jsonlite::write_json(object$expression, jsonFileName, pretty = TRUE)
  writeLines(paste("- Created JSON file:", jsonFileName))

  # Fetch SQL
  sql <- getCohortDefinitionSql(baseUrl = baseUrl,
                                cohortId = cohortId,
                                generateStats = generateStats)
  if (!file.exists(sqlFolder)) {
    dir.create(sqlFolder, recursive = TRUE)
  }
  sqlFileName <- file.path(sqlFolder, paste(name, "sql", sep = "."))
  SqlRender::writeSql(sql = sql, targetFile = sqlFileName)
  writeLines(paste("- Created SQL file:", sqlFileName))
}

#' Insert a set of cohort definitions into package
#'
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
#' @param generateStats           Should cohort inclusion rule statistics be created?
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
                                               generateStats = FALSE,
                                               packageName) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertLogical(insertTableSql, add = errorMessage)
  checkmate::assertLogical(insertCohortCreationR, add = errorMessage)
  checkmate::assertLogical(generateStats, add = errorMessage)
  checkmate::assertScalar(packageName, add = errorMessage)
  checkmate::assertCharacter(packageName, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  if (insertCohortCreationR && !insertTableSql)
    stop("Need to insert table SQL in order to generate R code")
  if (insertCohortCreationR && generateStats && jsonFolder != "inst/cohorts")
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
                                    sqlFolder = sqlFolder,
                                    generateStats = generateStats)
  }

  # Insert SQL to create empty cohort table
  if (insertTableSql) {
    writeLines("Creating SQL to create empty cohort table")
    .insertSqlForCohortTableInPackage(statsTables = generateStats, sqlFolder = sqlFolder)
  }

  # Store information on inclusion rules
  if (generateStats) {
    writeLines("Storing information on inclusion rules")
    rules <- .getCohortInclusionRules(jsonFolder)
    rules <- merge(rules, data.frame(cohortId = cohortsToCreate$cohortId,
                                     cohortName = cohortsToCreate$name))
    csvFileName <- file.path(jsonFolder, "InclusionRules.csv")
    write.csv(rules, csvFileName, row.names = FALSE)
    writeLines(paste("- Created CSV file:", csvFileName))
  }

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
    if (generateStats) {
      rCode <- gsub("#stats_start#", "", rCode)
      rCode <- gsub("#stats_end#", "", rCode)
    } else {
      rCode <- gsub("#stats_start#.*?#stats_end#", "", rCode)
    }
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

#' Get a cohort definition's SQL from WebAPI
#'
#' @details
#' Obtains the template SQL of a cohort.
#' When using generateStats = TRUE, the following tables are required to exist when executing the SQL:
#' cohort_inclusion, cohort_inclusion_result, cohort_inclusion_stats, and cohort_summary_stats. Also
#' note that the cohort_inclusion table should be populated with the names of the rules prior to
#' executing the cohort definition SQL.
#'
#' @template BaseUrl
#' @template CohortId
#' @param generateStats   Should the SQL include the code for generating inclusion rule statistics?
#'                        Note that if TRUE, several additional tables are expected to exists as
#'                        described in the details. By default this is TRUE.
#'
#' @return
#' The templated SQL to generate the cohort
#'
#' @export
getCohortDefinitionSql <- function(baseUrl, cohortId, generateStats = TRUE) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertLogical(generateStats, add = errorMessage)
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  url <- sprintf("%1s/cohortdefinition/sql", baseUrl)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")

  cohortDefinitionExpression <- ROhdsiWebApi::getCohortDefinition(baseUrl = baseUrl, 
                                                                  cohortId = cohortId)
  validJsonExpression <- RJSONIO::toJSON(cohortDefinitionExpression$expression)

  webApiVersion <- getWebApiVersion(baseUrl = baseUrl)
  if (compareVersion(a = "2.7.2", b = webApiVersion) == 0) {
    body <- RJSONIO::toJSON(list(expression = validJsonExpression,
                                 options = list(generateStats = generateStats)), digits = 23)

  } else {
    body <- RJSONIO::toJSON(list(expression = RJSONIO::fromJSON(validJsonExpression),
                                 options = list(generateStats = generateStats)), digits = 23)
  }

  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  sql <- (httr::content(req))$templateSql
  return(sql)
}

#' Get cohort generation information
#'
#' @details
#' Obtains cohort generation statuses for a collection of cohort definition Ids and CDM sources.
#' Useful if running multiple cohort generation jobs that are long-running.
#'
#' @template BaseUrl
#' @param cohortIds       A list of cohortIds
#' @param sourceKeys      (OPTIONAL) A list of CDM source keys. These can be found in Atlas ->
#'                        Configure. Otherwise, all CDM source keys will be used.
#'
#' @return
#' A data frame of cohort generation statuses, start times, and execution durations per definition id
#' and source key.
#'
#' @export
getCohortGenerationInformation <- function(cohortIds, baseUrl, sourceKeys = NULL) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(cohortIds, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  checkSourceKeys <- function(baseUrl, sourceKeys) {
    sourceIds <- lapply(X = sourceKeys, .getSourceIdFromKey, baseUrl = baseUrl)
    return(!(-1 %in% sourceIds))
  }

  if (missing(sourceKeys)) {
    sourceKeys <- (getCdmSources(baseUrl = baseUrl))$sourceKey
  }

  if (!checkSourceKeys(baseUrl = baseUrl, sourceKeys = sourceKeys)) {
    stop("One or more source keys is invalid, please check Atlas -> Configure page.")
  }

  cohortIds <- as.integer(cohortIds)
  tuples <- list(cohortIds, sourceKeys)
  df <- expand.grid(tuples, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("cohortId", "sourceKey")
  
  getCohortName <- function(cohortId) {
    return(tibble::tibble(cohortId = cohortId,
                          name = getCohortDefinition(cohortId = cohortId, 
                                                     baseUrl = baseUrl)$name))
  }
  cohortNames <- do.call("rbind", (lapply(cohortIds, getCohortName)))
  df <- merge(df, cohortNames)
  
  getGenerationInformation <- function(row) {
    result <- .getCohortGenerationStatus(baseUrl = baseUrl,
                                         cohortId = row$cohortId,
                                         sourceKey = row$sourceKey)

    return(tibble::tibble(sourceKey = row$sourceKey,
                          cohortId = row$cohortId,
                          definitionName = row$name,
                          status = result$status,
                          startTime = result$startTime,
                          executionDuration = result$executionDuration,
                          personCount = result$personCount))
  }
  results <- lapply(split(df, 1:nrow(df)), getGenerationInformation)
  results <- do.call("rbind", results)
  return(results)
}

.getCohortGenerationStatus <- function(baseUrl, cohortId, sourceKey) {

  .checkBaseUrl(baseUrl)

  sourceId <- .getSourceIdFromKey(baseUrl = baseUrl, sourceKey = sourceKey)

  url <- sprintf("%1s/cohortdefinition/%2s/info", baseUrl, cohortId)

  response <- httr::GET(url)
  response <- httr::content(response)

  if (length(response) == 0) {
    return(list(status = "NA", startTime = "NA", executionDuration = "NA", personCount = "NA"))
  }

  json <- response[sapply(response, function(j) j$id$sourceId == sourceId)]
  if (length(json) == 0) {
    return(list(status = "NA", startTime = "NA", executionDuration = "NA", personCount = "NA"))
  }

  return(list(status = json[[1]]$status,
              startTime = .millisecondsToDate(milliseconds = json[[1]]$startTime),
              executionDuration = ifelse(is.null(json[[1]]$executionDuration),
                                         "NA",
                                         json[[1]]$executionDuration),
              personCount = ifelse(is.null(json[[1]]$personCount), "NA", json[[1]]$personCount)))
}



.invokeCohortGeneration <- function(baseUrl, sourceKey, cohortId) {
  result <- .getCohortGenerationStatus(baseUrl = baseUrl,
                                       sourceKey = sourceKey,
                                       cohortId = cohortId)
  if (result$status %in% c("STARTING", "STARTED", "RUNNING")) {
    result$status
  } else {
    url <- sprintf("%1s/cohortdefinition/%2s/generate/%3s", baseUrl, cohortId, sourceKey)
    json <- httr::GET(url)
    json <- httr::content(json)
    json$status
  }
}

#' Invoke the generation of a set of cohort definitions
#'
#' @details
#' Invokes the generation of a set of cohort definitions across a set of CDMs set up in WebAPI. Use
#' \code{getCohortGenerationStatuses} to check the progress of the set.
#'
#' @template BaseUrl
#' @param cohortIds           A list of cohortIds
#' @param sourceKeys          A list of sourceKeys
#'
#' @export
invokeCohortSetGeneration <- function(baseUrl, sourceKeys, cohortIds) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(cohortIds, add = errorMessage)
  checkmate::assertCharacter(sourceKeys, min.len = 1, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  checkSourceKeys <- function(baseUrl, sourceKeys) {
    sourceIds <- lapply(X = sourceKeys, .getSourceIdFromKey, baseUrl = baseUrl)
    return(!(-1 %in% sourceIds))
  }

  if (!checkSourceKeys(baseUrl = baseUrl, sourceKeys = sourceKeys)) {
    stop("One or more source keys is invalid, please check Atlas -> Configure page.")
  }

  tuples <- list(cohortIds, sourceKeys)
  df <- expand.grid(tuples, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("cohortId", "sourceKey")

  statuses <- apply(X = df, MARGIN = 1, function(row) {
    list(sourceKey = row["sourceKey"],
         cohortId = as.integer(row["cohortId"]),
         definitionName = getCohortDefinition(baseUrl = baseUrl,
                                              cohortId = as.integer(row["cohortId"])$name),
         result = .invokeCohortGeneration(baseUrl = baseUrl,
                                          sourceKey = row["sourceKey"],
                                          cohortId = as.integer(row["cohortId"])))
  })

  df <- do.call(rbind, lapply(statuses, data.frame, stringsAsFactors = FALSE))
  rownames(df) <- c()
  df
}

#' Delete a cohort definition
#'
#' @details
#' Deletes cohort definition from WebAPI for a given cohort id
#'
#' @template BaseUrl
#' @template CohortId
#' @param silent      [OPTIONAL, Default = FALSE] If TRUE, function will work silently without showing any warning or error message.
#' @param stopOnError [OPTIONAL, Default = FALSE] If silent silent = TRUE, then this will be ignored.
#' 
#' @return
#' NA. A status message will be shown.
#'
#' @examples
#' \dontrun{
#' deleteCohortDefinition(cohortId = 282, baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
deleteCohortDefinition <- function(cohortId, baseUrl, silent = FALSE, stopOnError = FALSE) {
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::assertScalar(sourceKey, add = errorMessage)
  checkmate::assertCharacter(sourceKey, add = errorMessage)
  checkmate::assertLogical(silent, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  cohortDefinition <- tryCatch(ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl),
                               error=function(e) e, 
                               warning=function(w) w
  )
  thereIsAWarning <- stringr::str_detect(string = tolower(paste0("",cohortDefinition$message)), pattern = as.character(cohortId))
  
  if (!silent) {
    if (thereIsAWarning) {
      warning(paste0("", cohortDefinition$message))
    } else {
      url <- paste(baseUrl, "cohortdefinition", cohortId, sep = "/")
      response <- httr::DELETE(url)
      response <- httr::http_status(response)
      if (!stringr::str_detect(string = tolower(response$category), pattern = 'success')) {
        if (stopOnError) {
          stop("Deleting cohort definition id:", cohortId, " failed.")
        } else {
          warning("Deleting cohort definition id:", cohortId, " failed.")
        }
      }
    }
  }
  return(NA)
}



#' Get cohort generation results
#'
#' @details
#' Obtains a list with data frame containing details of output for cohort generation
#' 
#' @template BaseUrl
#' @template CohortId
#' @template SourceKey
#' @param mode        Mode is used to differentiate between inclusion rules and count by events (mode = 0, default) 
#'                    or persons (mode = 1). Default value = 0. 
#' @return            A list of data frames containing cohort generation report
#' @examples
#' \dontrun{
#' getCohortGeneratioInformation(cohortId = 282, baseUrl = "http://server.org:80/WebAPI", sourceKey = "HCUP", mode = 1)
#' }
#' @export
getCohortResults <- function(cohortId, baseUrl , sourceKey, mode = 0) {
  
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId)
  checkmate::assertScalar(sourceKey)
  checkmate::assertCharacter(sourceKey)
  checkmate::assertInt(mode, lower = 0, upper = 1)
  checkmate::reportAssertions(errorMessage)
  
  url <- sprintf("%s/cohortdefinition/%d/report/%s?mode=", baseUrl, cohortId, sourceKey, mode)
  json <- httr::GET(url)
  json <- httr::content(json)
  
  results <- list()
  results$summary <- json$summary %>% tidyr::as_tibble()
  if (length(json$inclusionRuleStats) == 0) {
    results$inclusionRuleStats <- NULL
  } else {
    inclusionRuleStats <- lapply(json$inclusionRuleStats, tibble::as_tibble)
    results$inclusionRuleStats <- do.call("rbind", inclusionRuleStats)
  }
  if (length(json$treemapData) == 0) {
    results$treemapData <- NULL
  } else {
    treemapData <- jsonlite::fromJSON(json$treemapData, simplifyDataFrame = FALSE)
    
    treeMapResult <- list(name = c(), size = c())
    treeMapResult <- .flattenTree(node = treemapData, accumulated = treeMapResult)
    results$treemapData <- dplyr::tibble(bits = treeMapResult$name, size = treeMapResult$size)
    results$treemapData$SatisfiedNumber = stringr::str_count(string = results$treemapData$bits, pattern = '1')
  }
  return(results)
}
