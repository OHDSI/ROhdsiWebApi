# @file WebApi
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Get Priority Vocabulary Source Key 
#' @details
#' Obtains the source key of the default OMOP Vocabulary in WebApi.
#'
#' @template BaseUrl
#'
#' @return
#' A string.
#'
#' @export
getPriorityVocabularyKey <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  url <- gsub("@baseUrl", baseUrl, "@baseUrl/source/priorityVocabulary")
  json <- .GET(url)
  json <- httr::content(json)
  json$sourceKey
}

#' Get the WebAPI version number 
#' @details
#' Obtains the WebAPI version number. This function is used to check that 
#' WebAPI baseUrl can be accessed and is a good first check to make sure 
#' you can access a WebAPI endpoint.
#'
#' @template BaseUrl
#'
#' @return
#' The WebApi versions as a string.
#' 
#' @examples 
#' \dontrun{
#' getWebApiVersion("http://server.org:80/WebAPI")
#' }
#' @export
getWebApiVersion <- function(baseUrl) {
  
  # TODO convert to checkmate
  stopifnot(is.character(baseUrl), nchar(baseUrl) > 0)
  
  if (grepl("/$", baseUrl)) {
    rlang::abort(paste0("baseUrl '", baseUrl, "' should not end with a /"))
  }
  
  url <- paste0(baseUrl, "/info")
  
  response <- httr::GET(url)
  if (response$status %in% c(200)) {
    version <- (httr::content(response))$version
  } else {
    rlang::abort(paste0("Could not reach WebApi. Possibly the base URL is not valid or is not reachable?\n",
                        "Please verify\n",
                        "- is it in the form http://server.org:80/WebAPI,\n",
                        "- are you are connected to the network",
                        "Status code: ",
                        response$status))
  }
  return(version)
}

#' Get the data sources in the WebAPI instance 
#' @details
#' Obtains the data sources configured in the WebAPI instance.
#'
#' @template BaseUrl
#'
#' @return
#' A data frame.
#'
#' @export
getCdmSources <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  url <- sprintf("%s/source/sources", baseUrl)
  request <- .GET(url)
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
    tibble::tibble(sourceId = s$sourceId,
                   sourceName = s$sourceName,
                   sourceKey = s$sourceKey,
                   sourceDialect = s$sourceDialect,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   vocabDatabaseSchema = vocabDatabaseSchema,
                   resultsDatabaseSchema = resultsDatabaseSchema)
  })

  return(dplyr::bind_rows(sourceDetails))
}


#' Check if an id is valid. 
#' @details
#' Checks if a set of id for a category is valid, i.e. checks if all the ids exists in the WebApi i.e.
#' valid.
#'
#' @template BaseUrl
#' @template Category
#' @param ids   A list of integer id(s) of the category to be tested for validity.
#' @return
#' A logical vector indicating if an ID is valid.
#'
#' @examples
#' \dontrun{
#' isValidId(ids = c(13242, 3423, 34), baseUrl = "http://server.org:80/WebAPI", category = "cohort")
#' }
#' @export
isValidId <- function(ids, baseUrl, category) {
  baseUrl <- gsub("/$", "", baseUrl)
  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == !!category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(ids, add = errorMessage)
  checkmate::assertChoice(x = category, choices = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  validIds <- getDefinitionsMetadata(baseUrl = baseUrl,
                                     category = argument$categoryStandard) %>% dplyr::select(.data$id) %>%
    dplyr::distinct() %>% dplyr::pull(.data$id) %>% as.integer()
  return(as.integer(ids) %in% validIds)
}


#' Check if source key is valid. 
#' @details
#' Checks if a set of sourceKey(s) are valid, i.e. checks if all the sourceKey(s) exists in the WebApi
#' i.e. valid.
#'
#' @template BaseUrl
#' @param sourceKeys   The source key(s) for a CDM instance in WebAPI, as defined in the Configuration
#'                     page.
#' @return
#' A logical vector indicating if an ID is valid.
#'
#' @examples
#' \dontrun{
#' isValidSourceKey(sourceKeys = c("HCUP", "CCA"),
#'                  baseUrl = "http://server.org:80/WebAPI",
#'                  category = "cohort")
#' }
#' @export
isValidSourceKey <- function(sourceKeys, baseUrl) {
  cdmSources <- getCdmSources(baseUrl)
  validSourceKeys <- cdmSources %>% dplyr::select(.data$sourceKey) %>% dplyr::distinct() %>% dplyr::pull(.data$sourceKey)
  return(sourceKeys %in% validSourceKeys)
}
