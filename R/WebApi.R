# @file WebApi
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


.checkBaseUrl <- function(baseUrl) {
  success <- tryCatch({
    getWebApiVersion(baseUrl = baseUrl)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!success) {
    stop("Could not reach WebApi. Possibly the base URL is not valid? (please verify it is like http://server.org:80/WebAPI)")
  }
  
  return(success)
}

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




#' Retrieve the details of all Atlas definitions, by atlas functional category.
#'
#' @details
#' Obtains the details of Atlas study definitions such as id, name, created/modified details, hash object, etc.
#' The following atlas function categories are supported. Concept-set, Cohort-definition,
#' Cohort-characterization, Pathway-analysis, Incidence rate (ir), estimation and prediction.
#' This function is useful to retrieve the current definitions in one atlas instance and comparing it
#' to another atlas instance, or for version control.
#'
#'
#' @param baseUrl   The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'
#' @return
#' A data frame of atlas study definitions with details. Note: modifiedDate and createdDate are
#' returned as text/character (to be worked on in future version).
#'
#' @export
getAtlasDefinitionsDetails <- function(baseUrl) {
  atlasCategories <- c('conceptset',
                       'cohortdefinition',
                       'ir',
                       'estimation',
                       'prediction')
  
  listOfAtlasIds <- list()
  for (i in (1:length(atlasCategories))) {
    #i  = 1
    atlasCategory <- atlasCategories[[i]]
    url <-
      paste(baseUrl, atlasCategory, '?size=100000000', sep = "/")
    request <- httr::GET(url)
    httr::stop_for_status(request)
    listOfAtlasIds[[atlasCategory]] <- httr::content(request) %>%
      purrr::map(function(x)
        purrr::map(x, function(y)
          ifelse(is.null(y), NA, y))) %>% # convert NULL to NA in list
      dplyr::bind_rows() %>%
      dplyr::mutate(atlasCategory = atlasCategory) %>%
      dplyr::mutate(createdDate = as.character(createdDate),
                    modifiedDate = as.character(modifiedDate))
  }
  
  # there is difference in how webApi returns for 'cohort-characterization' and 'pathway-analysis'
  # the return are nested within 'content'
  atlasCategories <- c('cohort-characterization',
                       'pathway-analysis')
  for (i in (1:length(atlasCategories))) {
    atlasCategory <- atlasCategories[[i]]
    url <-
      paste(baseUrl, atlasCategory, '?size=100000000', sep = "/")
    request <- httr::GET(url)
    httr::stop_for_status(request)
    listOfAtlasIds[[atlasCategory]] <-
      httr::content(request)$content %>%
      purrr::map(function(x)
        purrr::map(x, function(y)
          ifelse(is.null(y), NA, y))) %>%  # convert NULL to NA in list
      dplyr::bind_rows()
    
    if (atlasCategory == 'cohort-characterization') {
      listOfAtlasIds[[atlasCategory]] <-
        listOfAtlasIds[[atlasCategory]] %>%
        dplyr::rename(
          createdDate = createdAt,
          modifiedDate = updatedAt,
          modifiedBy = updatedBy
        )
    }
    
    listOfAtlasIds[[atlasCategory]] <-
      listOfAtlasIds[[atlasCategory]] %>%
      dplyr::mutate(atlasCategory = atlasCategory) %>%
      dplyr::mutate(createdDate = as.character(createdDate),
                    modifiedDate = as.character(modifiedDate))
  }
  # to do: createdDate and modifiedDate are in character format. Need to make them date/time.
  # but this does not appear to be consistent.
  listOfAtlasIds <- dplyr::bind_rows(listOfAtlasIds) %>%
    dplyr::mutate(
      atlasCategory = dplyr::case_when(
        atlasCategory == 'conceptset' ~ 'conceptSets',
        atlasCategory == 'cohortdefinition' ~ 'cohortDefinitions',
        atlasCategory == 'ir' ~ 'incidenceRates',
        atlasCategory == 'estimation' ~ 'estimation',
        atlasCategory == 'prediction' ~ 'prediction',
        atlasCategory == 'cohort-characterization' ~ 'characterizations',
        atlasCategory == 'pathway-analysis' ~ 'cohortPathways'
      )
    ) %>%
    dplyr::mutate(atlasCategory = SqlRender::camelCaseToTitleCase(atlasCategory))
  return(listOfAtlasIds)
}



#' Post a atlas study definition
#'
#' @details
#' Posts a definition JSON into WebAPI
#'
#' @template BaseUrl 
#' @param name             A valid name for atlas study definition.Please make sure that the
#'                         name is unique in the target webapi - otherwise, an error might be thrown.
#' @param definitionType   Currently only 'cohort' is supported and this corresponds to 'Cohort-definition'.
#' @param jsonExpression   A valid JSON expression for an atlas study definition. Note: this is
#'                         expected to be a JSON object. Atlas names should be specified seperately
#'                         and are not part of this JSON expression. AtlasId's will be assigned by WebApi.                       
#' @return                 This function will return a dataframe object with one row
#'                         describing the posted atlas study definitions and its details.
#'                         See \code{\link{getAtlasDefinitionsDetails}}. 
#'                         If unsuccessful a STOP message will be shown.
#'
#' @examples
#' \dontrun{
#' validJsonExpression <- getCohortDefinition(baseUrl = baseUrl, cohortId = 15873)
#' validJsonExpression <- RJSONIO::toJSON(cohortDefinition$expression)
#' postAtlasDefinition(name = 'new atlas cohort name', 
#'                      baseUrl = "http://server.org:80/WebAPI", 
#'                      jsonExpression = validJsonExpression)
#' }
#' @export
postAtlasDefinition <- function(baseUrl, 
                                name,
                                definitionType = 'cohort',
                                jsonExpression) {
  .checkBaseUrl(baseUrl)
  
  if (definitionType == 'cohort') {
    json_body <- paste0("{\"name\":\"",
                        as.character(name),
                        "\",\"expressionType\": \"SIMPLE_EXPRESSION\", \"expression\":",
                        jsonExpression,
                        "}")
    # POST the JSON
    response <- httr::POST(url = paste0(baseUrl, "/cohortdefinition/"),
                           body = json_body,
                           config = httr::add_headers(.headers =c('Content-Type' = 'application/json')))
    # Expect a "200" response that it worked
    if (response$status_code != 200) {
      stop(paste0("Post attempt failed for cohort : ", name))
    } else {
      atlasDefinitionDetails <- getAtlasDefinitionsDetails(baseUrl = baseUrl) %>% 
        dplyr::filter(atlasCategory == 'Cohort Definitions',
                      name == !!name
        )
      return(atlasDefinitionDetails)
    }
  } else {
    stop(paste0('definitionType = ', definitionType, " is not supported in this version. Post attempt failed."))
  }
}