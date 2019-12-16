# @file Estimation
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

#' Get the results from individual Achilles reports by data Source definition expression
#'
#' @details
#' Obtain the JSON expression from WebAPI for a given dataSource
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param estimationId   The Atlas id for the estimation specification
#' @return
#' A list of objects with specifications for estimation
#'
#' @examples
#' \dontrun{
#' # This will obtain pre-computed summary regarding dataSource
#'
#' getEstimationSpecification(baseUrl = "http://server.org:80/WebAPI", estimationId = 3434)
#' }
#'
#' @export


getEstimationSpecification <- function(baseUrl,
                                        estimationId
                                        ){
  estimationId <- as.integer(estimationId)
  url <- sprintf("%s/estimation/%d/", baseUrl, estimationId)
  json <- httr::GET(url)
  json <- httr::content(json)
  
  estimationSpecification <- list()
  estimationSpecification[['native']] <- json
  estimationSpecification[['parsed']] <- RJSONIO::fromJSON(json$specification)
  
  listToConvertToDataFrame <- c('cohortDefinitions',
                              'conceptSets',
                              'conceptSetCrossReference'
                              )
  
  for (i in (1:length(listToConvertToDataFrame))){
    temp <- estimationSpecification$parsed[[listToConvertToDataFrame[[i]]]]
    df = data.frame(matrix(unlist(temp), nrow = length(temp), byrow = TRUE))
    names(df) = temp[[1]] %>% names
    estimationSpecification$parsed[[listToConvertToDataFrame[[i]]]] <- df
  }

  listToConvertToDataFrame2 <- c('targetComparatorOutcomes',
                            'cohortMethodAnalysisList'
                            )
  
  for (i in (1:length(listToConvertToDataFrame2))){
    temp <- estimationSpecification$parsed$estimationAnalysisSettings$analysisSpecification[[listToConvertToDataFrame2[[i]]]]
    df = data.frame(matrix(unlist(temp), nrow = length(temp), byrow = TRUE))
    names(df) = temp[[1]] %>% names
    estimationSpecification$parsed$estimationAnalysisSettings$analysisSpecification[[listToConvertToDataFrame2[[i]]]] <- df
  }

listTODO <- c(
  'negativeControls',
  'positiveControlSynthesisArgs',
  'negativeControlOutcomeCohortDefinition',
  'negativeControlExposureCohortDefinition',
  'estimationAnalysisSettings')  

estimationSpecification
  
  

}
