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

#' Get definitions for incident rate analysis
#'
#' @details
#' Returns an R-objects with definitions for incident rate analysis
#'
#' @template BaseUrl
#' @param incidenceRateId    The WebApi ID for incidence rate analysis.
#' @return                   Get definitions for incident rate analysis
#'
#' @examples
#' \dontrun{
#'  getIncidenceRateDefinition <- function(baseUrl "http://server.org:80/WebAPI",
#'                                          incidenceRateId = 296)
#'                                          )
#' }
#' @export
getIncidenceRateDefinition <- function(baseUrl,
                                       incidenceRateId) {
  .checkBaseUrl(baseUrl)
  #ir specifications
  url <- sprintf("%1s/ir/%2s", baseUrl, incidenceRateId)
  json <- httr::GET(url)
  data <- httr::content(json)
  if (!is.null(data$payload$message)) {
    stop(data$payload$message)
  }
  data$expression <- RJSONIO::fromJSON(data$expression)
  data$createdDate <- .millisecondsToDate(data$createdDate)
  data$modifiedDate <- .millisecondsToDate(data$modifiedDate)
  return(data)
}

#' Get incident rate generation information
#'
#' @details
#' Get incident rate generation information
#'
#' @template BaseUrl
#' @param incidenceRateId The WebApi ID for incidence rate analysis.
#' @return                A tibble data-frame R-object with incidence rate
#'                        generation information.
#'
#' @examples
#' \dontrun{
#' getIncidenceRateGenerationInformation(incidenceRateId = 296, baseUrl = baseUrl)
#'                                          )
#' }
#' @export
getIncidenceRateGenerationInformation <- function(incidenceRateId, baseUrl) {
  .checkBaseUrl(baseUrl)
  #generation
  url <- sprintf("%1s/ir/%2s/info", baseUrl, incidenceRateId)
  url <- httr::GET(url)
  json <- httr::content(url, as = "text", type = "application/json", encoding = 'UTF-8')
  if (json == '[]') {
    stop(paste0("Please check if incident rate id:", incidenceRateId, "exists."))
  }
  data <- jsonlite::fromJSON(txt = json, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
  data$summaryList <- NULL
  names(data) <- names(data) %>%
    stringr::str_replace_all(pattern = 'executionInfo.', replacement = '') %>%
    stringr::str_replace_all(pattern = 'id.', replacement = '')  
  
  data <- data %>%
    dplyr::mutate(startTime = .millisecondsToDate(.data$startTime)) %>%
    dplyr::as_tibble()
  return(data)
}

#' Get results from incidence rate analysis.
#'
#' @details
#' Given a sourceKey, targetCohortId, outcomeCohortId combinations, this function will return the 
#' results of incidence rate analysis.
#'
#' @template BaseUrl
#' @param incidenceRateId    The WebApi ID for incidence rate analysis.
#' @param sourceKey          The sourceKey for the incident rate analysis.
#' @param targetCohortId     The cohort id of the target cohort in the incident rate analysis.
#' @param outcomeCohortId    The cohort id of the outcome cohort in the incidence rate analysis.
#' @return                   A list with incident rate results summary, stratified results and treemap data.
#'
#' @examples
#' \dontrun{
#' getIncidenceRateResults(incidenceRateId = 296,
#'                         baseUrl ="http://server.org:80/WebAPI",
#'                                     targetCohortId = 432423, 
#'                                     outcomeCohortId = 324,
#'                                     sourceKey = "HCUP"
#'                                    )
#' }
#' @export
getIncidenceRateResults <- function(incidenceRateId,
                                    baseUrl,
                                    sourceKey,
                                    targetCohortId,
                                    outcomeCohortId) {
  .checkBaseUrl(baseUrl)
  
  url <- sprintf("%1s/ir/%2s/report/%3s?targetId=%4s&outcomeId=%5s", 
                 baseUrl, 
                 incidenceRateId, 
                 sourceKey, 
                 targetCohortId, 
                 outcomeCohortId
  )
  url <- httr::GET(url)
  json <- httr::content(url, as = "text", type = "application/json", encoding = 'UTF-8')
  if (json == '[]') {
    stop(paste0("Please check if incident rate id:", incidenceRateId, "exists."))
  }
  data <- jsonlite::fromJSON(txt = json, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
  
  data$summary <- dplyr::bind_rows(data$summary)
  data$summary$proportionP1K <- (data$summary$cases/data$summary$totalPersons)*1000
  data$summary$rateP1K <- (data$summary$cases/data$summary$timeAtRisk)*1000
  data$summary$sourceKey <- sourceKey
  data$summary$incidenceRateId <- incidenceRateId
  
  data$stratifyStats <- dplyr::bind_rows(data$stratifyStats)
  if (nrow(data$stratifyStats) > 0) {
    data$stratifyStats$proportionP1K = (data$stratifyStats$cases/data$stratifyStats$totalPersons)*1000
    data$stratifyStats$rateP1K = (data$stratifyStats$cases/data$stratifyStats$timeAtRisk)*1000
    data$stratifyStats$sourceKey <- sourceKey
    data$stratifyStats$incidenceRateId <- incidenceRateId
  } else {
    data$stratifyStats$proportionP1K = 0.0
    data$stratifyStats$rateP1K = 0.0
    data$stratifyStats$sourceKey <- sourceKey
    data$stratifyStats$incidenceRateId <- incidenceRateId
    data$stratifyStats <- data$stratifyStats %>% dplyr::slice(0)
  }
  
  # recursively flattenTree
  treeMapResult <- list(name = c(), size = c())
  jsonTreeMapData <- RJSONIO::fromJSON(data$treemapData)
  treeMapResult <- .flattenTree(jsonTreeMapData,treeMapResult) 
  treeMapResultDf <- dplyr::tibble(bits = treeMapResult$name, size = treeMapResult$size)
  treeMapResultDf <- treeMapResultDf %>% dplyr::mutate(
    'SatisfiedNumber' = stringr::str_count(string = treeMapResultDf$bits, pattern = '1'),
    'SatisfiedRules' = stringr::str_locate_all(string = treeMapResultDf$bits, pattern = '1') %>% paste()
  )
  
  data$treemapData <- treeMapResultDf
  data$treemapData$sourceKey <- sourceKey
  data$treemapData$targetId <- targetCohortId
  data$treemapData$outcomeId <- outcomeCohortId
  data$treemapData$incidenceRateId <- incidenceRateId
  
  if (nrow(data$stratifyStats) == 0) {
    data$treemapData <- data$treemapData %>% dplyr::slice(0)
  }
  return(data)
}