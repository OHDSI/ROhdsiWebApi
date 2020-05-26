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
#' A JSON list object representing the cohort definition
#' This function has been deprecated. As an alternative please use the following
#' steps as in the example below:
#'   1) cohortDefinition <- getCohortDefinition(baseUrl = baseUrl, cohortId = 15873)
#'   2) validJsonExpression <- RJSONIO::toJSON(cohortDefinition$expression)
#'   3) save validJsonExpression object as .txt"
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
              package="ROhdsiWebApi", 
              msg = "This function has been deprecated. As an alternative please use the following
              steps as in the example below:
              1) validJsonExpression <- getCohortDefinition(baseUrl = baseUrl, cohortId = 15873)
              2) validJsonExpression <- RJSONIO::toJSON(cohortDefinition$expression)
              3) save validJsonExpression object as .txt",
              old = as.character(sys.call(sys.parent()))[1L])
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
#' (Deprecated) Obtains the name of a cohort. 
#' This function has been deprecated. As an alternative please use getCohortDefinition
#'
#' @template BaseUrl
#' @template CohortId
#' @param formatName     Should the name be formatted to remove prefixes and underscores?
#'
#' @return
#' The name of the cohort.
#'
#' @export
getCohortDefinitionName <- function(baseUrl, cohortId, formatName = FALSE) {
  .checkBaseUrl(baseUrl)
  .Deprecated(new = "getCohortDefinition", 
              package="ROhdsiWebApi", 
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
              package="ROhdsiWebApi", 
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