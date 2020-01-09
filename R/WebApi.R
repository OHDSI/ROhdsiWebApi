# @file WebApi
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


# .checkBaseUrl <- function(baseUrl) {
#   patterns <- list("https?:\\/\\/[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})+(\\/.*)?\\/WebAPI$",
#                    "https?:\\/\\/(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])(:[0-9]{1,5})+(\\/.*)?\\/WebAPI$")
#   results <- lapply(patterns, function(p) {
#     grepl(pattern = p, 
#           x = baseUrl, 
#           ignore.case = FALSE)
#   })
#   success <- any(as.logical(results))
# 
#   if (!success) {
#     stop("Base URL not valid, should be like http://server.org:80/WebAPI")
#   }
# }
# 
.convertNulltoNA <- function(thisList) {
  for (n in names(thisList)) {
    if (is.null(thisList[n][[1]])) {
      thisList[n] <- NA
    }
  }
  thisList
}

#' Get Priority Vocab Source Key
#'
#' @details
#' Obtains the source key of the default OMOP Vocab in Atlas.
#'
#' @param baseUrl   The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'
#' @return
#' A string with the source key of the default OMOP Vocab in Atlas.
#'
#' @export
getPriorityVocabKey <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  url <- gsub("@baseUrl", baseUrl, "@baseUrl/source/priorityVocabulary")
  json <- httr::GET(url)
  json <- httr::content(json)
  json$sourceKey
}

#' Get the version of the WebAPI
#'
#' @details
#' Obtains the WebAPI version number
#'
#' @param baseUrl   The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'
#' @return
#' The WebAPI version
#'
#' @export
getWebApiVersion <- function(baseUrl) {
  url <- sprintf("%s/info", baseUrl)
  json <- httr::GET(url)
  (httr::content(json))$version
}

.getSourceIdFromKey <- function(baseUrl, sourceKey) {
  .checkBaseUrl(baseUrl)

  url <- sprintf("%1s/source/%2s", baseUrl, sourceKey)

  json <- httr::GET(url)
  json <- httr::content(json)
  if (is.null(json$sourceId))
    json$sourceId <- -1
  json$sourceId
}

.formatName <- function(name) {
  gsub("_", " ", gsub("\\[(.*?)\\]_", "", gsub(" ", "_", name)))
}


#' Get the data sources in the WebAPI instance
#'
#' @details
#' Obtains the data sources configured in the WebAPI instance
#'
#' @param baseUrl   The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'
#' @return
#' A data frame of data source information
#'
#' @export
getCdmSources <- function(baseUrl) {

  url <- sprintf("%s/source/sources", baseUrl)
  request <- httr::GET(url)
  httr::stop_for_status(request)
  sources <- httr::content(request)

  sourceDetails <- lapply(sources, function(s) {
    cdmDatabaseSchema <- NA
    vocabDatabaseSchema <- NA
    resultsDatabaseSchema <- NA
    if (length(s$daimons) > 0) {
      for (i in 1:length(s$daimons)) {
        if (!is.na(s$daimons[[i]]$daimonType)) {
          if (toupper(s$daimons[[i]]$daimonType) == toupper("cdm")) {
          cdmDatabaseSchema <- s$daimons[[i]]$tableQualifier
          }
          if (toupper(s$daimons[[i]]$daimonType) == toupper("vocabulary")) {
          vocabDatabaseSchema <- s$daimons[[i]]$tableQualifier
          }
          if (toupper(s$daimons[[i]]$daimonType) == toupper("results")) {
          resultsDatabaseSchema <- s$daimons[[i]]$tableQualifier
          }
        }
      }
    }
    data.frame(sourceName = s$sourceName,
               sourceKey = s$sourceKey,
               sourceDialect = s$sourceDialect,
               cdmDatabaseSchema = cdmDatabaseSchema,
               vocabDatabaseSchema = vocabDatabaseSchema,
               resultsDatabaseSchema = resultsDatabaseSchema,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, sourceDetails)
}


#' Get configuration information for source keys.
#'
#' @details
#' Source keys are commonly used as aliases to identify a data source. 
#' A data source is uniquely identified a combination of sourceName and/or CDM name.
#' sourceName/CDM name may also have version information. 
#' WebApi calls rely on sourceKey for query. But relying on aliases
#' is not reliable, as the underlying data source may change.
#' This function, will give the configuration information for data sources
#' given source keys. It may be used to unambigously identify the data source
#' in a study.
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param sourceKeys     A list of string names for the source keys as in
#'                       configuration page of Atlas.
#' @return
#' A list of two objects called native and parsed. Native is the json 
#' specification, and parsed is the simplified version returned as a data.frame
#' object. The data.frame object may be used for creating reports.
#'
#' @examples
#' \dontrun{
#' #
#'
#' getSourceKeyConfiguration(baseUrl = "http://server.org:80/WebAPI", 
#'                                      sourceKeys = c('source1', 'source2')
#'                                    )$parsed
#' would return a data.frame with items like sourceId, sourceName, sourceDialect,
#' sourceKey, priority, CDM, CEM.
#' }
#'
#' @export
getSourceKeyConfiguration <- function(baseUrl,sourceKeys){
  .checkBaseUrl(baseUrl)
  
  a <- .getCdmSources(baseUrl)
  a$parsed <- a$parsed %>%
    dplyr::filter(toupper(sourceKey) %in% toupper(sourceKeys))
  
  result <- list(
    native = a$native,
    parsed = a$parsed
  )
  result
}
