# @file CohortDefinition
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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



#' Get a cohort definition expression
#' 
#' @details 
#' Obtain the JSON expression from WebAPI for a given cohort id
#' 
#' @param definitionId    The number indicating which cohort definition to fetch.
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @return 
#' A JSON list object representing the cohort definition
#' 
#' @examples
#' \dontrun{
#' # This will obtain a cohort definition's JSON expression:
#'
#' getCohortDefinitionExpression(definitionId = 282, 
#'                               baseUrl = "http://server.org:80/WebAPI")
#' }
#'                        
#' @export
getCohortDefinitionExpression <- function(definitionId, 
                                          baseUrl) {
  .checkBaseUrl(baseUrl)
  
  url <- paste(baseUrl, "cohortdefinition", definitionId, sep = "/")
  json <- httr::GET(url)
  httr::content(json)
}



#' Load a cohort definition and insert it into this package
#'
#' @details
#' Load a cohort definition from a WebApi instance and insert it into this package. This will fetch
#' the json object and store it in the 'inst/cohorts' folder, and fetch the template SQL and store it
#' in the 'inst/sql/sql_server' folder. Both folders will be created if they don't exist. When using
#' generateStats = TRUE, the following tables are required to exist when executing the SQL:
#' cohort_inclusion, cohort_inclusion_result, cohort_inclusion_stats, and cohort_summary_stats. Also
#' note that the cohort_inclusion table should be populated with the names of the rules prior to
#' executing the cohort definition SQL.
#'
#' @param definitionId    The number indicating which cohort definition to fetch.
#' @param name            The name that will be used for the json and SQL files. If not provided, the
#'                        name in cohort will be used, but this may not lead to valid file names.
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#'
#' @param generateStats   Should the SQL include the code for generating inclusion rule statistics?
#'                        Note that if TRUE, several additional tables are expected to exists as
#'                        described in the details.
#'
#' @examples
#' \dontrun{
#' # This will create 'inst/cohorts/Angioedema.json' and 'inst/sql/sql_server/Angioedema.sql':
#'
#' insertCohortDefinitionInPackage(definitionId = 282, 
#'                                 name = "Angioedema",
#'                                 baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
insertCohortDefinitionInPackage <- function(definitionId,
                                            name = NULL,
                                            baseUrl,
                                            generateStats = FALSE) {
  .checkBaseUrl(baseUrl)

  ### Fetch JSON object ###
  json <- getCohortDefinitionExpression(definitionId = definitionId, baseUrl = baseUrl)
  
  # expression <- RJSONIO::fromJSON(json$expression)
  if (is.null(name)) {
    name <- json$name
  }
  if (!file.exists("inst/cohorts")) {
    dir.create("inst/cohorts", recursive = TRUE)
  }
  fileConn <- file(file.path("inst/cohorts", paste(name, "json", sep = ".")))
  writeLines(json$expression, fileConn)
  close(fileConn)

  ### Fetch SQL by posting JSON object ###
  parsedExpression <- RJSONIO::fromJSON(json$expression)
  if (generateStats) {
    jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression,
                                     options = list(generateStats = TRUE)), digits = 23)
  } else {
    jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression), digits = 23)
  }
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  url <- paste(baseUrl, "cohortdefinition", "sql", sep = "/")
  cohortSqlJson <- httr::POST(url, body = jsonBody, config = httr::add_headers(httpheader))
  cohortSqlJson <- httr::content(cohortSqlJson)
  sql <- cohortSqlJson$templateSql
  if (!file.exists("inst/sql/sql_server")) {
    dir.create("inst/sql/sql_server", recursive = TRUE)
  }

  fileConn <- file(file.path("inst/sql/sql_server", paste(name, "sql", sep = ".")))
  writeLines(sql, fileConn)
  close(fileConn)
}


#' Insert a set of cohort definitions into package
#'
#' @param fileName                Name of a CSV file in the inst/settings folder of the package
#'                                specifying the cohorts to insert. See details for the expected file
#'                                format.
#' @param baseUrl                 The base URL for the WebApi instance, for example:
#'                                "http://server.org:80/WebAPI".
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
insertCohortDefinitionSetInPackage <- function(fileName,
                                               baseUrl,
                                               insertTableSql = TRUE,
                                               insertCohortCreationR = TRUE,
                                               generateStats = FALSE,
                                               packageName) {
  .checkBaseUrl(baseUrl)
  
  if (insertCohortCreationR && !insertTableSql)
    stop("Need to insert table SQL in order to generate R code")
  cohortsToCreate <- read.csv(file.path("inst/settings", fileName))
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Inserting cohort:", cohortsToCreate$name[i]))
    insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i],
                                                 name = cohortsToCreate$name[i],
                                                 baseUrl = baseUrl,
                                                 generateStats = generateStats)
  }
  if (insertTableSql) {
    .insertSqlForCohortTableInPackage(statsTables = generateStats)
  }
  if (generateStats) {
    rules <- .getCohortInclusionRules()
    rules <- merge(rules, data.frame(cohortId = cohortsToCreate$cohortId,
                                     cohortName = cohortsToCreate$name))
    write.csv(rules, "inst/cohorts/InclusionRules.csv", row.names = FALSE)
  }
  if (insertCohortCreationR) {
    templateFileName <- system.file("CreateCohorts.R", package = "ROhdsiWebApi")
    rCode <- readChar(templateFileName, file.info(templateFileName)$size)
    rCode <- gsub("#CopyrightYear#", format(Sys.Date(), "%Y"), rCode)
    rCode <- gsub("#packageName#", packageName, rCode)
    rCode <- gsub("#fileName#", fileName, rCode)
    if (generateStats) {
      rCode <- gsub("#stats_start#", "", rCode)
      rCode <- gsub("#stats_end#", "", rCode)
    } else {
      rCode <- gsub("#stats_start#.*?#stats_end#", "", rCode)
    }
    fileConn <- file("R/CreateCohorts.R")
    writeChar(rCode, fileConn, eos = NULL)
    close(fileConn)
  }
}

.getCohortInclusionRules <- function() {
  rules <- data.frame()
  for (file in list.files(path = "inst/cohorts", pattern = ".*\\.json")) {
    writeLines(paste("Parsing", file, "for inclusion rules"))
    definition <- RJSONIO::fromJSON(file.path("inst/cohorts", file))
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

.insertSqlForCohortTableInPackage <- function(statsTables = FALSE) {
  fileName <- system.file("CohortTable.sql", package = "ROhdsiWebApi")
  sql <- readChar(fileName, file.info(fileName)$size)
  if (statsTables) {
    fileName <- system.file("InclusionStatsTables.sql", package = "ROhdsiWebApi")
    sql <- paste(sql, readChar(fileName, file.info(fileName)$size), sep = "\n")
  }
  if (!file.exists("inst/sql/sql_server")) {
    dir.create("inst/sql/sql_server", recursive = TRUE)
  }
  fileConn <- file("inst/sql/sql_server/CreateCohortTable.sql")
  writeChar(sql, fileConn, eos = NULL)
  close(fileConn)
  invisible(sql)
}


#' Get a cohort definition's name from WebAPI
#'
#' @details
#' Obtains the name of a cohort.
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param definitionId   The cohort definition id in Atlas.
#' @param formatName     Should the name be formatted to remove prefixes and underscores?
#'
#' @return
#' The name of the cohort.
#'
#' @export
getCohortDefinitionName <- function(baseUrl, definitionId, formatName = FALSE) {
  .checkBaseUrl(baseUrl)
  
  json <- getCohortDefinitionExpression(definitionId = definitionId, baseUrl = baseUrl)

  if (formatName) {
    .formatName(json$name)
  } else {
    json$name
  }
}


#' Get a cohort definition's SQL from WebAPI
#'
#' @details
#' Obtains the template SQL of a cohort.
#'
#' @param baseUrl                 The base URL for the WebApi instance, for example:
#'                                "http://server.org:80/WebAPI".
#' @param definitionId            The cohort definition id in Atlas.
#'
#' @return
#' The templated SQL to generate the cohort
#'
#' @export
getCohortDefinitionSql <- function(baseUrl, 
                                   definitionId) {
  .checkBaseUrl(baseUrl)
  
  url <- sprintf("%1s/cohortdefinition/sql", baseUrl)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  
  json <- getCohortDefinitionExpression(definitionId = definitionId, baseUrl = baseUrl)
  
  webApiVersion <- getWebApiVersion(baseUrl = baseUrl)
  if (compareVersion(a = "2.7.2", b = webApiVersion) == 1) {
    body <- RJSONIO::toJSON(list(expression = RJSONIO::fromJSON(json$expression), 
                                 options = list(generateStats = TRUE)), digits = 23)  
  } else {
    body <- RJSONIO::toJSON(list(expression = json$expression, 
                                 options = list(generateStats = TRUE)), digits = 23)  }
  
  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  (httr::content(req))$templateSql
}



#' Get Priority Vocab Source Key
#'
#' @details
#' Obtains the source key of the default OMOP Vocab in Atlas.
#'
#' @param baseUrl   The base URL for the WebApi instance, for example:
#'                  "http://server.org:80/WebAPI".
#'
#' @return
#' A string with the source key of the default OMOP Vocab in Atlas.
#'
#' @export
getPriorityVocabKey <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  url <- gsub("@baseUrl", baseUrl, "@baseUrl/source/priorityVocabulary")
  json <- httr::GET(url)
  json <- httr::content(json)
  json$sourceKey
}






#' Get a list of concept sets and concepts from a cohort definition
#' 
#' @details 
#' For a given cohort definition id, get all concept sets and resolve all concepts from each
#' 
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param definitionId    The cohort id to fetch concept sets and concepts from
#' 
#' @return 
#' A list of concept sets, set names, and concepts
#' 
#' @examples
#' \dontrun{
#' # This will obtain a list of concept sets and concepts from a cohort id:
#'
#' getConceptsFromCohortId(baseUrl = "http://server.org:80/WebAPI",
#'                         definitionId = 123)
#' }
#' 
#' @export
getConceptSetsAndConceptsFromCohort <- function(baseUrl,
                                                definitionId, 
                                                vocabSourceKey = NULL) {
  
  .checkBaseUrl(baseUrl)
  
  if (missing(vocabSourceKey) || is.null(vocabSourceKey)) {
    vocabSourceKey <- getPriorityVocabKey(baseUrl = baseUrl)
  }
  
  json <- getCohortDefinitionExpression(definitionId = definitionId, baseUrl = baseUrl)
  
  webApiVersion <- getWebApiVersion(baseUrl = baseUrl)
  
  if (compareVersion(a = "2.7.2", webApiVersion) == 1) {
    json <- RJSONIO::fromJSON(json$expression)  
  } else 
    json <- json$expression
  
  url <- sprintf("%1s/vocabulary/%2s/resolveConceptSetExpression", baseUrl, vocabSourceKey)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  
  lapply(json$ConceptSets, function(j) {
    body <- RJSONIO::toJSON(j$expression, digits = 23)
    req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
    req <- httr::content(req)
    concepts <- unlist(req)  
    list(
      id = j$id,
      name = j$name,
      concepts = concepts
    )
  })
}



#' Get Cohort Generation Statuses
#'
#' @details
#' Obtains cohort generation statuses for a collection of cohort definition Ids and CDM sources.
#' Useful if running multiple cohort generation jobs that are long-running.
#'
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param definitionIds   A list of cohort definition Ids
#' @param sourceKeys      A list of CDM source keys. These can be found in Atlas -> Configure.
#'
#' @return
#' A data frame of cohort generation statuses, start times, and execution durations per definition id
#' and source key.
#'
#' @export
getCohortGenerationStatuses <- function(baseUrl, definitionIds, sourceKeys) {
  .checkBaseUrl(baseUrl)
  
  checkSourceKeys <- function(baseUrl, sourceKeys) {
    sourceIds <- lapply(X = sourceKeys, .getSourceIdFromKey, baseUrl = baseUrl)
    return(!(-1 %in% sourceIds))
  }

  if (!checkSourceKeys(baseUrl = baseUrl, sourceKeys = sourceKeys)) {
    stop("One or more source keys is invalid, please check Atlas -> Configure page.")
  }

  tuples <- list(definitionIds, sourceKeys)
  df <- expand.grid(tuples, KEEP.OUT.ATTRS = FALSE)
  colnames(df) <- c("definitionId", "sourceKey")

  statuses <- apply(X = df, MARGIN = 1, function(row) {
    result <- .getCohortGenerationStatus(baseUrl = baseUrl,
                                         definitionId = row["definitionId"],
                                         sourceKey = row["sourceKey"])

    status <- list(sourceKey = row["sourceKey"],
                   definitionId = row["definitionId"],
                   definitionName = getCohortDefinitionName(baseUrl = baseUrl,
                                                            definitionId = row["definitionId"],
                                                            formatName = FALSE),
                   status = result$status,
                   startTime = result$startTime,
                   executionDuration = result$executionDuration,
                   personCount = result$personCount)
  })

  df <- do.call(rbind, lapply(statuses, data.frame, stringsAsFactors = FALSE))
  rownames(df) <- c()
  df
}



.getCohortGenerationStatus <- function(baseUrl, definitionId, sourceKey) {
  millisecondsToDate <- function(milliseconds) {
    sec <- milliseconds/1000
    as.character(as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone()))
  }

  .checkBaseUrl(baseUrl)

  sourceId <- .getSourceIdFromKey(baseUrl = baseUrl, sourceKey = sourceKey)

  url <- sprintf("%1s/cohortdefinition/%2s/info", baseUrl, definitionId)

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
              startTime = millisecondsToDate(milliseconds = json[[1]]$startTime),
              executionDuration = ifelse(is.null(json[[1]]$executionDuration),
                                         "NA",
                                         json[[1]]$executionDuration),
              personCount = ifelse(is.null(json[[1]]$personCount), "NA", json[[1]]$personCount)))
}

.invokeCohortGeneration <- function(baseUrl, sourceKey, definitionId) {
  result <- .getCohortGenerationStatus(baseUrl = baseUrl,
                                       sourceKey = sourceKey,
                                       definitionId = definitionId)
  if (result$status %in% c("STARTING", "STARTED", "RUNNING")) {
    result$status
  } else {
    url <- sprintf("%1s/cohortdefinition/%2s/generate/%3s", baseUrl, definitionId, sourceKey)
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
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param definitionIds   A list of cohort definition Ids
#' @param sourceKeys      A list of CDM source keys. These can be found in Atlas -> Configure.
#'
#' @export
invokeCohortSetGeneration <- function(baseUrl, sourceKeys, definitionIds) {
  .checkBaseUrl(baseUrl)
  
  checkSourceKeys <- function(baseUrl, sourceKeys) {
    sourceIds <- lapply(X = sourceKeys, .getSourceIdFromKey, baseUrl = baseUrl)
    return(!(-1 %in% sourceIds))
  }

  if (!checkSourceKeys(baseUrl = baseUrl, sourceKeys = sourceKeys)) {
    stop("One or more source keys is invalid, please check Atlas -> Configure page.")
  }

  tuples <- list(definitionIds, sourceKeys)
  df <- expand.grid(tuples, KEEP.OUT.ATTRS = FALSE)
  colnames(df) <- c("definitionId", "sourceKey")

  statuses <- apply(X = df, MARGIN = 1, function(row) {
    list(sourceKey = row["sourceKey"],
         definitionId = row["definitionId"],
         definitionName = getCohortDefinitionName(baseUrl = baseUrl,
                                                  definitionId = row["definitionId"],
                                                  formatName = FALSE),
         result = .invokeCohortGeneration(baseUrl = baseUrl,
                                          sourceKey = row["sourceKey"],
                                          definitionId = row["definitionId"]))
  })

  df <- do.call(rbind, lapply(statuses, data.frame, stringsAsFactors = FALSE))
  rownames(df) <- c()
  df
}

#' Get cohort inclusion rules and person counts
#'
#' @details
#' Obtains the inclusion rules from a cohort definition and summarizes the person counts per rule
#'
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param cohortId        The Atlas cohort definition id for the cohort
#' @param sourceKey       The source key for a CDM instance in WebAPI, as defined in the Configuration page
#' 
#' @export
getCohortInclusionRulesAndCounts <- function(baseUrl, 
                                             cohortId, 
                                             sourceKey) {
  url <- sprintf("%s/cohortdefinition/%d/report/%s?mode=0",
                 baseUrl, cohortId, sourceKey)
  json <- httr::GET(url)
  json <- httr::content(json) 
  
  results <- lapply(json$inclusionRuleStats, function(j) {
    list(ruleId = j$id,
         description = j$name,
         indexPersonCount = json$summary$baseCount,
         rulePersonCount = j$countSatisfying,
         rulePercentSatisfied = j$percentSatisfying,
         rulePercentToGain = j$percentExcluded,
         matchRate = json$summary$percentMatched)  
  })
  do.call(rbind.data.frame, results)
}