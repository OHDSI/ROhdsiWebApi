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
  
  distResults <- resultJson[sapply(resultJson, function(r) r$resultType == "DISTRIBUTION") ]
  prevResults <- resultJson[sapply(resultJson, function(r) r$resultType == "PREVALENCE") ]
  
  
  distResults <- lapply(distResults, function(r) {
    r[sapply(r, is.null)] <- NA
    as.data.frame(r)
  })

  prevResults <- lapply(prevResults, function(r) {
    r[sapply(r, is.null)] <- NA
    as.data.frame(r)
  })
  
  distResultsDf <- do.call("rbind", distResults)
  prevResultsDf <- do.call("rbind", prevResults)
  
  if (length(domains) > 0 | length(analysisNames) > 0) {
    features <- lapply(designJson$featureAnalyses, function(f) {
      f[sapply(f, is.null)] <- NA
      as.data.frame(f)
    })
    
    featureDf <- do.call("rbind", features)
    
    if (length(domains) > 0) {
      featureDf <- featureDf[featureDf$domain %in% domains,]
    }
    
    if (length(analysisNames) > 0) {
      featureDf <- featureDf[featureDf$name %in% analysisNames,]
    }  
    
    distResultsDf <- distResultsDf[distResultsDf$id %in% featureDf$id,]
    prevResultsDf <- prevResultsDf[prevResultsDf$id %in% featureDf$id,]
  }
  
  if (length(cohortIds) > 0) {
    distResultsDf <- distResultsDf[distResultsDf$cohortId %in% cohortIds,]
    prevResultsDf <- prevResultsDf[prevResultsDf$cohortId %in% cohortIds,]
  }
  
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
    r$status == "COMPLETED" & r$sourceKey == sourceKey
  })]
  
  df <- do.call(rbind.data.frame, generations)
  df <- df[which.max(df$id),]
  if (nrow(df) > 0) {
    df$id
  } else {
    stop("Please generate the cohort characterization")
  }
}

