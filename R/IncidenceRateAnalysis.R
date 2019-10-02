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


#' Get precomputed incidence rate analysis results from a specified Atlas incidence rate analyis ID
#' for the specified combination of Atlas targetId, Atlas outcomeId and Data Source.
#' 
#' @details 
#' Obtain a data frame with results
#' 
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param irAnalysisId    The Atlas ID for incidence rate analysis.
#' @param targetId        The Atlas ID for cohort definition corresponding to the target cohort.
#' @param outcomeId       The Atlas ID for cohort definition corresponding to the outcome cohort.
#' @param sourceKey       The source key for a CDM instance in WebAPI, as defined in the Atlas Configuration page.
#' @return 
#' A list of Data Frame objects corresponding to the results of the Incidence Rate Analysis.
#' 
#' @examples
#' \dontrun{
#' # This will obtain a list of Data Frame with results of the Incidence Rate Analysis:
#' getIncidenceRateAnalysisResults <- function(baseUrl "http://server.org:80/WebAPI",
#'                                          irAnalysisId = 296,
#'                                          sourceKey = 'YOUR_DATA_SOURCE',
#'                                          targetId =10817,
#'                                          outcomeId = 10873)
#'                                          )
#' }
#'                        
#' @export
getIncidenceRateAnalysisResults <- function(baseUrl = NULL,
                                            irAnalysisId = NULL,
                                            sourceKey = NULL,
                                            targetId =NULL,
                                            outcomeId = NULL) {
  .checkBaseUrl(baseUrl)
  
  url <- paste(baseUrl, "ir", irAnalysisId, "report", paste0(sourceKey, '?targetId=',targetId, "&outcomeId=", outcomeId), sep = "/")
  json <- httr::GET(url)
  json <- httr::content(json)
  summaryDf <- as.data.frame(json$summary)
  
  stratifyStats <- lapply(json$stratifyStats, function(j) {
    list(id = j$id,
         targetId = j$targetId,
         outcomeId = j$outcomeId,
         name = j$name,
         totalPersons = j$totalPersons,
         cases = j$cases,
         timeAtRisk = j$timeAtRisk
         )  
  })
  stratifyStatsDf <- do.call(rbind.data.frame, stratifyStats)
  
  irSpecification <- data.frame("irAnalysisId" = irAnalysisId,
                                "sourceKey" = sourceKey,
                                "url" = url)
  
  results <- list(irSpecification = irSpecification, summary = summaryDf, stratifyStats = stratifyStatsDf)
}