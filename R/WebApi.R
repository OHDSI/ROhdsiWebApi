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
    stop("Could not reach WebApi. Possibly the base URL is not valid or is not reachable?\n",
         "Please verify\n",
         "- is it in the form http://server.org:80/WebAPI,\n",
         "- are you are connected to the network")
  }
  
  return(success)
}

#' Get Priority Vocabulary Source Key
#'
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
  json <- httr::GET(url)
  json <- httr::content(json)
  json$sourceKey
}

#' Get the WebAPI version number
#'
#' @details
#' Obtains the WebAPI version number.
#'
#' @template BaseUrl
#'
#' @return
#' A string.
#'
#' @export
getWebApiVersion <- function(baseUrl) {
  url <- sprintf("%s/info", baseUrl)
  json <- httr::GET(url)
  (httr::content(json))$version
}

.formatName <- function(name) {
  gsub("_", " ", gsub("\\[(.*?)\\]_", "", gsub(" ", "_", name)))
}

#' Get the data sources in the WebAPI instance
#'
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


#' Retrieve the meta data of all WebApi definitions
#' 
#' @param categories        These are the categories in WebApi. The valid options are conceptSet, 
#'                          cohort, incidenceRate, estimation, prediction, characterization, pathway.
#'
#' @details
#' Obtains the meta data of WebApi specifications such as id, name, created/modified 
#' details, hash object, etc. The following function categories are supported. 
#' Concept-set, Cohort-definition, Cohort-characterization, Pathway-analysis, Incidence rate (ir), 
#' estimation and prediction. This function is useful to retrieve the current specifications.
#'
#' @template BaseUrl
#'
#' @return          
#' A tibble of specification metadata. Note: modifiedDate and createdDate are
#' returned as text/character.
#' 
#' @examples
#' \dontrun{
#' getDefinitionsMetadata(baseUrl = "http://server.org:80/WebAPI")
#' }
#' 
#' @export
getDefinitionsMetadata <- function(baseUrl, categories) {
  .checkBaseUrl(baseUrl)
  
  # there is difference in how WebApi returns for 'cohort-characterization' and 'pathway-analysis'
  # the return are nested within 'content'
  # group1 and group2 are categories that are different based on how WebApi is implemented.
  group1 <- c('conceptSet','cohort','incidenceRate','estimation','prediction')
  group2 <- c('characterization','pathway')
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(categories, min.len = 1, add = errorMessage)
  checkmate::assertNames(x = categories, subset.of = c(group1, group2) )
  checkmate::reportAssertions(errorMessage)
  
  listOfIds <- list()
  for (i in length(categories)) {
    category <- categories[[i]]
    if (category %in% group1) {
      if (category == 'conceptSet') 
      {
        categoryUrl = 'conceptset'
      } else if (category == 'cohort') 
      {
        categoryUrl = 'cohortdefinition'
      } else if (category == 'incidenceRate') 
      {
        categoryUrl = 'ir'
      } else if (category == 'estimation') 
      {
        categoryUrl = 'estimation'
      } else if (category == 'prediction') 
      {
        categoryUrl = 'prediction'
      }
      
      url <- paste(baseUrl, categoryUrl, '?size=100000000', sep = "/")
      request <- httr::GET(url)
      httr::stop_for_status(request)
      listOfIds[[category]] <- httr::content(request) %>%
        purrr::map(function(x)
          purrr::map(x, function(y)
            ifelse(is.null(y), NA, y))) %>% # convert NULL to NA in list
        dplyr::bind_rows() %>%
        dplyr::mutate(createdDate = as.character(.data$createdDate),
                      modifiedDate = as.character(.data$modifiedDate))
      
    } else if (category %in% group2) {
      if (category == 'characterization') 
      {
        categoryUrl = 'cohort-characterization'
      } else if (category == 'pathway') 
      {
        categoryUrl = 'pathway-analysis'
      }
      
      url <- paste(baseUrl, categoryUrl, '?size=100000000', sep = "/")
      request <- httr::GET(url)
      httr::stop_for_status(request)
      listOfIds[[category]] <-
        httr::content(request)$content %>%
        purrr::map(function(x)
          purrr::map(x, function(y)
            ifelse(is.null(y), NA, y))) %>%  # convert NULL to NA in list
        dplyr::bind_rows()
      
      if (category == 'characterization') {
        listOfIds[[category]] <-
          listOfIds[[category]] %>%
          dplyr::rename(createdDate = .data$createdAt,
                        modifiedDate = .data$updatedAt,
                        modifiedBy = .data$updatedBy )
      }
      
      listOfIds[[category]] <-
        listOfIds[[category]] %>%
        dplyr::mutate(category = category) %>%
        dplyr::mutate(createdDate = as.character(.data$createdDate),
                      modifiedDate = as.character(.data$modifiedDate))
      
    }
  }
  # to do: createdDate and modifiedDate are in character format. Need to make them date/time.
  # but this does not appear to be consistent.
  listOfIds <- dplyr::bind_rows(listOfIds) %>%
    dplyr::mutate(category = SqlRender::camelCaseToTitleCase(category))
  return(listOfIds)
}





#' Post a definition into WebApi
#'
#' @details
#' Post a definition into WebAPI
#'
#' @template BaseUrl 
#' @param name        A valid name for the definition. WebApi will use this name (if valid) as
#'                    the name of the definition. WebApi checks for validity,
#'                    such as uniqueness, absence of unacceptable character etc. An error might be thrown.
#' @param category    The category of expression in WebApi. Currently only 'cohort' is supported,but
#'                    we will support posting the definitions of Concept-set, Cohort-definition, 
#'                    Cohort-characterization, Pathway-analysis, Incidence rate (ir), 
#'                    estimation and prediction. They should be referenced using string 
#'                    'conceptSet','cohort', 'characterization', 'pathway', 'incidenceRate', 
#'                    'estimation','prediction' only. 
#' @param expression  An R list object containing the expression for the specification. 
#'                    This will be converted to JSON expression by function and posted into the WebApi.
#'                    Note: only limited checks are performed in R to check the validity of this
#'                    expression.               
#' @return            This function will return a dataframe object with one row
#'                    describing the posted WebApi expression and its details.
#'                    If unsuccessful a STOP message will be shown.
#'                         
#' @examples
#' \dontrun{
#' validRExpression <- getCohortDefinition(baseUrl = baseUrl, 
#'                                            cohortId = 15873)$expression
#' postDefinition(name = 'new name for expression in sdaddaddd',
#'                baseUrl = "http://server.org:80/WebAPI",
#'                expression = validRExpression,
#'                category = 'cohort')
#' }
#' @export
postDefinition <- function(baseUrl, 
                           name,
                           category,
                           expression) {
  .checkBaseUrl(baseUrl)
  arguments <- ROhdsiWebApi:::.getStandardCategories()
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertNames(x = category, subset.of = arguments$categoryStandard)
  checkmate::assertList(x = expression, 
                        types = c('character', 'list', 'integer', 'numeric', 'logical'), 
                        any.missing = TRUE,
                        null.ok = FALSE, 
                        add = errorMessage
  )
  checkmate::reportAssertions(errorMessage)
  
  argument <- arguments %>% 
              dplyr::filter(.data$categoryStandard == !!category)
  
  # convert R-object to JSON expression.
  jsonExpression <- RJSONIO::toJSON(expression)
  
  if (category %in% c('cohort')) { #conceptSet and Characterization is posting empty definitions
    # create json body
    json_body <- paste0("{\"name\":\"",
                        as.character(name),
                        "\",\"expressionType\": \"SIMPLE_EXPRESSION\", \"expression\":",
                        jsonExpression,
                        "}")
    # POST the JSON
    response <- httr::POST(url = paste0(baseUrl, "/", argument$categoryAsUsedInWebApi, "/"),
                           body = json_body,
                           config = httr::add_headers(.headers = c('Content-Type' = 'application/json')))
    # Check response
    if (response$status_code != 200) {
      errorMessage <- paste0("Post attempt failed for ", 
                             category, " : ", 
                             name, 
                             ". Name already exists. ", 
                             httr::http_status(response)$message)
      definitionsMetaData <- getDefinitionsMetadata(baseUrl = baseUrl, categories = category) %>% 
        dplyr::filter(name == !!name)
      if (nrow(definitionsMetaData)) {
        stop(paste0(errorMessage, " Name already exists in WebApi." ))
      } else {
      stop(errorMessage)
      }
    } else {
      metaDataSpecifications <- getDefinitionsMetadata(baseUrl = baseUrl, categories = category) %>% 
        dplyr::filter(.data$name == !!name)
      return(metaDataSpecifications)
    }
  } else {
    stop(paste0('category = ', category, " is not supported in this version. Post not attempted."))
  }
}