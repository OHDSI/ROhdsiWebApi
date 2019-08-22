# @file CohortCharacterization
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




#' Get Cohort Characterization Results
#' 
#' @param baseUrl               The base URL for the WebApi instance, for example:
#'                              "http://server.org:80/WebAPI".
#' @param characterizationId    The id of the cohort characterization in Atlas
#' @param generationId          Used to specify the id of a particular generation of a cohort characterization.
#'                              By default, the latest execution is retrieved
#' @param sourceKey             The source key for a CDM instance in WebAPI, as defined in the Configuration page
#' @param cohortIds             (OPTIONAL) Which cohort definition ids would you like to retrieve? 
#'                              By default, all cohorts are retrieved.
#' @param domains               (OPTIONAL) Which feature domains would you like to retrieve?
#'                              By default, all domains are retrieved.
#' @param analysisNames         (OPTIONAL) Which feature analysis names would you like to retrieve?
#'                              By default, all analyses are retrieved.
#' 
#' @export                             
getCohortCharacterizationResults <- function(baseUrl, 
                                             characterizationId, 
                                             generationId = NULL,
                                             sourceKey,
                                             cohortIds = c(),
                                             domains = c(),
                                             analysisNames = c()) {
  if (is.null(generationId)) {
    generationId <- .getLatestGenerationId(baseUrl = baseUrl, characterizationId = characterizationId, sourceKey = sourceKey)  
  }
  
  designUrl <- sprintf("%s/cohort-characterization/generation/%d/design", baseUrl, generationId)
  designJson <- httr::content(httr::GET(designUrl))
  
  resultUrl <- sprintf("%s/cohort-characterization/generation/%d/result", baseUrl, generationId)
  resultJson <- httr::content(httr::GET(resultUrl))
  
  distResults <- resultJson[sapply(resultJson, function(r) toupper(r$resultType) == "DISTRIBUTION") ]
  prevResults <- resultJson[sapply(resultJson, function(r) toupper(r$resultType) == "PREVALENCE") ]
  
  if (length(cohortIds) > 0) {
    distResults <- distResults[sapply(distResults, function(r) r$cohortId %in% cohortIds)]
    prevResults <- distResults[sapply(prevResults, function(r) r$cohortId %in% cohortIds)]
  }
  
  if (length(domains) > 0) {
    featureAnalyses <- designJson$featureAnalyses
    featureAnalyses <- featureAnalyses[sapply(featureAnalyses, function(f) f$domain %in% domains)]
    featureIds <- lapply(featureAnalyses, function(f) f$id)
    prevResults <- prevResults[sapply(prevResults, function(r) r$id %in% featureIds)]
    distResults <- distResults[sapply(distResults, function(r) r$id %in% featureIds)]
  }
  
  if (length(analysisNames) > 0) {
    distResults <- distResults[sapply(distResults, function(r) r$analysisName %in% analysisNames)] 
    prevResults <- prevResults[sapply(prevResults, function(r) r$analysisName %in% analysisNames)] 
  }
  
  distResults <- lapply(distResults, function(r) {
    r[sapply(r, is.null)] <- NA
    as.data.frame(r)
  })

  prevResults <- lapply(prevResults, function(r) {
    r[sapply(r, is.null)] <- NA
    as.data.frame(r)
  })
  
  distResultsDf <- do.call(rbind, distResults)
  prevResultsDf <- do.call(rbind, prevResults)
  
  list(sourceKey = sourceKey,
       characterizationId = characterizationId,
       generationId = generationId,
       distribution = distResultsDf,
       prevalence = prevResultsDf)
}

.getLatestGenerationId <- function(baseUrl, characterizationId, sourceKey) {
  url <- sprintf("%s/cohort-characterization/%d/generation", baseUrl, characterizationId)
  json <- httr::GET(url)
  results <- httr::content(json)
  
  generations <- results[sapply(results, function(r) {
    toupper(r$status) == "COMPLETED" & r$sourceKey == sourceKey
  })]
  
  df <- do.call(rbind.data.frame, generations)
  df <- df[which.max(df$id),]
  if (nrow(df) > 0) {
    df$id
  } else {
    stop("Cohort characterization results not found. Please generate the cohort characterization.")
  }
}

