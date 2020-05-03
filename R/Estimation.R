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

#' Get the estimation specification
#'
#' @details
#' Get an R list object with expression from WebAPI for a given estimation specification
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param estimationId   The Atlas id for the estimation specification
#' @return               A list of R-objects with specifications for estimation
#'
#' @examples
#' \dontrun{
#' getEstimation(baseUrl = "http://server.org:80/WebAPI", estimationId = 3434)
#' }
#'
#' @export
getEstimation <- function(baseUrl, estimationId){
  .checkBaseUrl(baseUrl)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(estimationId)
  checkmate::reportAssertions(errorMessage)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(estimationId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- sprintf("%s/estimation/%d/", baseUrl, estimationId)

  json <- httr::GET(url)
  data <- httr::content(json)
  data$expression <- data$specification #this expression cannot be imported back - why?
  
  data$createdDate <- .millisecondsToDate(data$createdDate)
  data$modifiedDate <- .millisecondsToDate(data$modifiedDate)
  data$specification <- jsonlite::fromJSON(txt = data$specification)
  
  data$specification$cohortDefinitions <- dplyr::as_tibble(data$specification$cohortDefinitions)
  data$specification$conceptSets <- dplyr::as_tibble(data$specification$conceptSets)
  data$specification$conceptSetCrossReference <- dplyr::as_tibble(data$specification$conceptSetCrossReference)
  data$specification$negativeControls <- dplyr::as_tibble(data$specification$negativeControls)
  
  targetComparatorOutcomeIdDf <- list()
  temp <- data$specification$estimationAnalysisSettings$analysisSpecification$targetComparatorOutcomes
  
  targetComparatorOutcomeIdDf <- data.frame(expand.grid(targetId = temp$targetId %>% unlist() %>% unique(),
                                                        comparatorId = temp$comparatorId %>% unlist() %>% unique(),
                                                        outcomeId = temp$outcomeIds %>% purrr::reduce(c) %>% unique())
  )
  data$specification$estimationAnalysisSettings$analysisSpecification$targetComparatorOutcomes <-
    targetComparatorOutcomeIdDf
  
  return(data)
}

