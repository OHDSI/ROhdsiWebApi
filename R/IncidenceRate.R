# @file IncidenceRate
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

#' Get definitions for incident rate analysis
#'
#' @details
#' Returns an R-objects with definitions for incident rate analysis
#'
#' @param baseUrl            The base URL for the WebApi instance, for example:
#'                           "http://server.org:80/WebAPI".
#' @param incidenceRateId    The Atlas ID for incidence rate analysis.
#' @return                   Get definitions for incident rate analysis
#'
#' @examples
#' \dontrun{
#' getIncidenceRateDefinitions <- function(baseUrl "http://server.org:80/WebAPI",
#'                                          incidenceRateId = 296)
#'                                          )
#' }
#' @export
getIncidenceRateDefinitions <- function(baseUrl,
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


#' Get incident rate generation information.
#'
#' @details
#' Get incident rate generation information.
#'
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param incidenceRateId The Atlas ID for incidence rate analysis.
#' @return                An R-object
#'
#' @examples
#' \dontrun{
#' getIncidenceRateGenerationInformation <- function(baseUrl "http://server.org:80/WebAPI",
#'                                          incidenceRateId = 296)
#'                                          )
#' }
#' @export
getIncidenceRateGenerationInformation <- function(baseUrl,
                                                  incidenceRateId) {
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
    dplyr::mutate(
      startTime = millisecondsToDate(startTime)
    ) %>%
    dplyr::as_tibble()
  return(data)
}
