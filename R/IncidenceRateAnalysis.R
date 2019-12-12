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
#' @return 
#' A list of Data Frame objects corresponding to the results of the Incidence Rate Analysis.
#' 
#' @examples
#' \dontrun{
#' # This will obtain a list of Data Frame with results of the Incidence Rate Analysis:
#' getIncidenceRateAnalysis <- function(baseUrl "http://server.org:80/WebAPI",
#'                                          irAnalysisId = 296)
#'                                          )
#' }
#'                        
#' @export
getIncidenceRateAnalysis <- function(baseUrl,
                                     irAnalysisId) {
  .checkBaseUrl(baseUrl)
  
  url <- sprintf("%1s/ir/%2s", baseUrl, irAnalysisId)
  json <- httr::GET(url)
  json <- httr::content(json)
  json$description <- '' #json$description is NULL by default. IR does not have an option for description.

  irDetails <- data.frame(id = json$id,
                          name = json$name,
                          description = json$description,
                          createdDate = as.POSIXct(json$createdDate/1000,origin="1970-01-01"),
                          createdBy = json$createdBy,
                          modifiedDate = as.POSIXct(json$modifiedDate/1000,origin="1970-01-01"),
                          modifiedBy = json$modifiedBy
                        )
    
  irExpression <- RJSONIO::fromJSON(json$expression)
  irTimeAtRisk <- irExpression$timeAtRisk
  irStrata <- irExpression$strata
  irTargetCohorts <- list()
  for (i in (1:length(irExpression$targetCohorts))){
    a <- data.frame(targetCohortId = as.integer(irExpression$targetCohorts[[i]]$id),
                    targetCohortName = irExpression$targetCohorts[[i]]$name
                    )
    irTargetCohorts[[i]] <- a
  }
  irTargetCohorts <- do.call(rbind, irTargetCohorts)
  irOutcomeCohorts <- list()
  for (i in (1:length(irExpression$outcomeCohorts))){
    a <- data.frame(outcomeCohortId = as.integer(irExpression$outcomeCohorts[[i]]$id),
                    outcomeCohortName = irExpression$outcomeCohorts[[i]]$name
    )
    irOutcomeCohorts[[i]] <- a
  }
  irOutcomeCohorts <- do.call(rbind, irOutcomeCohorts)

 
  url <- sprintf("%1s/ir/%2s/info", baseUrl, irAnalysisId)
  json <- httr::GET(url)
  json <- httr::content(json)
  irGeneration <- list()
  for (i in (1:length(json))){
    a <- data.frame(
                      analysisId = json[[i]]$executionInfo$id$analysisId,
                      sourceId = json[[i]]$executionInfo$id$sourceId,
                      startTime = as.POSIXct(json[[i]]$executionInfo$startTime/1000,origin="1970-01-01"),
                      executionDuration = json[[i]]$executionInfo$executionDuration,
                      status = json[[i]]$executionInfo$status,
                      isValid = json[[i]]$executionInfo$isValid,
                      isCanceled = json[[i]]$executionInfo$isCanceled,
                      message = json[[i]]$executionInfo$message,
                      canceled = json[[i]]$executionInfo$canceled
                    )
    irGeneration[[i]] <- a
  }
  irGeneration <- do.call(rbind, irGeneration)
  cdmDataSources <- getCdmSources(baseUrl = baseUrl)
  irGeneration <- left_join(irGeneration, cdmDataSources, by = 'sourceId')
  
  
  irResults <- list()
  for (targetIdLoop in irTargetCohorts$targetCohortId) {
    for (outcomeIdLoop in irOutcomeCohorts$outcomeCohortId) {
      for (sourceKeyLoop in irGeneration$sourceKey) {
      
        url <- sprintf("%1s/ir/%2s/report/%3s?targetId=%4s&outcomeId=%5s", baseUrl, irAnalysisId, sourceKeyLoop, targetIdLoop, outcomeIdLoop)
        json <- httr::GET(url)
        json <- httr::content(json)
        json$summary$proportionP1K <- (json$summary$cases/json$summary$totalPersons)*1000
        json$summary$rateP1K <- (json$summary$cases/json$summary$timeAtRisk)*1000
      
        irSummary <- as.data.frame(json$summary)
        
        irStratifyStats <- list()
        for (i in (1:length(json$stratifyStats))) {
          a <- data.frame(
                            id = json$stratifyStats[[i]]$id,
                            name = json$stratifyStats[[i]]$name,
                            targetId = json$stratifyStats[[i]]$targetId,
                            outcomeId = json$stratifyStats[[i]]$outcomeId,
                            totalPersons = json$stratifyStats[[i]]$totalPersons,
                            cases = json$stratifyStats[[i]]$cases,
                            timeAtRisk = json$stratifyStats[[i]]$timeAtRisk,
                            proportionP1K = (json$stratifyStats[[i]]$cases/json$stratifyStats[[i]]$totalPersons)*1000,
                            rateP1K = (json$stratifyStats[[i]]$cases/json$stratifyStats[[i]]$timeAtRisk)*1000
                        )
          irStratifyStats[[i]] <- a
        }
        irStratifyStats <- do.call(rbind, irStratifyStats)
        
        #TODO - treeMapData

        
        cdmDatabaseSchema2 <- irGeneration %>% 
                                filter(sourceKey == sourceKeyLoop) %>% 
                                select(cdmDatabaseSchema) %>% 
                                unique() %>% 
                                str_remove('.dbo')
        
        irResults[[paste0('targetId_', targetIdLoop)]][[paste0('outcomeId_', outcomeIdLoop)]][[paste0('sourceKey_', cdmDatabaseSchema2)]][['irStratifyStats']] <- irStratifyStats
        irResults[[paste0('targetId_', targetIdLoop)]][[paste0('outcomeId_', outcomeIdLoop)]][[paste0('sourceKey_', cdmDatabaseSchema2)]][['irSummary']] <- irSummary
        irResults[[paste0('targetId_', targetIdLoop)]][[paste0('outcomeId_', outcomeIdLoop)]][[paste0('sourceKey_', cdmDatabaseSchema2)]][['irTreeMapData']] <- 'ToDo'
      }
    }
  }
  
  results <- list(irDetails = irDetails, 
                  irExpression = irExpression, 
                  irTimeAtRisk = irTimeAtRisk,
                  irStrata = irStrata,
                  irTargetCohorts = irTargetCohorts,
                  irOutcomeCohorts = irOutcomeCohorts,
                  irGeneration = irGeneration,
                  irResults = irResults
                  )
  
}
