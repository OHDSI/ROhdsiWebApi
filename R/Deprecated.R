# @file Deprecated
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


#' (Deprecated) Get a cohort definition expression
#'
#' @details
#' (Deprecated) Obtain the JSON expression from WebAPI for a given cohort id
#'
#' @template BaseUrl
#' @template CohortId
#'
#' @return
#' A JSON list object representing the cohort definition This function has been deprecated. As an
#' alternative please use the following steps as in the example below: 1) cohortDefinition <-
#' getCohortDefinition(baseUrl = baseUrl, cohortId = 15873) 2) validJsonExpression <-
#' RJSONIO::toJSON(cohortDefinition$expression) 3) save validJsonExpression object as .txt"
#'
#' @examples
#' \dontrun{
#' # This will obtain a cohort definition's JSON expression:
#'
#' getCohortDefinitionExpression(cohortId = 282, baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getCohortDefinitionExpression <- function(cohortId, baseUrl) {
  .checkBaseUrl(baseUrl)
  .Deprecated(new = "getCohortDefinition",
              package = "ROhdsiWebApi",
              msg = "This function has been deprecated. As an alternative please use the following
  steps as in the example below:
  1) validJsonExpression <- getCohortDefinition(baseUrl = baseUrl, cohortId = 15873)
  2) validJsonExpression <- RJSONIO::toJSON(cohortDefinition$expression)
  3) save validJsonExpression object as .txt", old = as.character(s.call(s.parent()))[1L])
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  url <- paste(baseUrl, "cohortdefinition", cohortId, sep = "/")
  json <- httr::GET(url)
  httr::content(json)
}



#' (Deprecated) Get a cohort definition's name from WebAPI
#'
#' @details
#' (Deprecated) Obtains the name of a cohort. This function has been deprecated. As an alternative
#' please use getCohortDefinition
#'
#' @template BaseUrl
#' @template CohortId
#' @param formatName   Should the name be formatted to remove prefixes and underscores?
#'
#' @return
#' The name of the cohort.
#'
#' @export
getCohortDefinitionName <- function(baseUrl, cohortId, formatName = FALSE) {
  .checkBaseUrl(baseUrl)
  .Deprecated(new = "getCohortDefinition",
              package = "ROhdsiWebApi",
              msg = "This function has been deprecated. As an alternative please use getCohortDefinition",
              old = as.character(sys.call(sys.parent()))[1L])
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertLogical(formatName, add = errorMessage)
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  json <- getCohortDefinitionExpression(cohortId = cohortId, baseUrl = baseUrl)

  if (formatName) {
    .formatName(json$name)
  } else {
    json$name
  }
}

#' Get cohort inclusion rules and person counts
#'
#' @details
#' Obtains the inclusion rules from a cohort definition and summarizes the person counts per rule
#'
#' @template BaseUrl
#' @template CohortId
#' @template SourceKey
#'
#' @export
getCohortInclusionRulesAndCounts <- function(baseUrl, cohortId, sourceKey) {
  .Deprecated(new = "getCohortGenerationReport",
              package = "ROhdsiWebApi",
              msg = "This function has been deprecated. As an alternative please use getCohortResults",
              old = as.character(sys.call(sys.parent()))[1L])

  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::assertScalar(sourceKey, add = errorMessage)
  checkmate::assertCharacter(sourceKey, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  url <- sprintf("%s/cohortdefinition/%d/report/%s?mode=0", baseUrl, cohortId, sourceKey)
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




#' Get a cohort definition's SQL from WebAPI
#'
#' @details
#' Obtains the template SQL of a cohort. When using generateStats = TRUE, the following tables are
#' required to exist when executing the SQL: cohort_inclusion, cohort_inclusion_result,
#' cohort_inclusion_stats, and cohort_summary_stats. Also note that the cohort_inclusion table should
#' be populated with the names of the rules prior to executing the cohort definition SQL.
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
getCohortDefinitionSql <- function(cohortId, baseUrl, generateStats = TRUE) {
  .Deprecated(new = "getCohortDefinitionSql",
              package = "ROhdsiWebApi",
              msg = "This function has been deprecated. As an alternative please use getCohortSql",
              old = as.character(sys.call(sys.parent()))[1L])
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertLogical(generateStats, add = errorMessage)
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  url <- sprintf("%1s/cohortdefinition/sql", baseUrl)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")

  cohortDefinition <- ROhdsiWebApi::getCohortDefinition(baseUrl = baseUrl, cohortId = cohortId)
  validJsonExpression <- RJSONIO::toJSON(cohortDefinition$expression)

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


.formatName <- function(name) {
  gsub("_", " ", gsub("\\[(.*?)\\]_", "", gsub(" ", "_", name)))
}
