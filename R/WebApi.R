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
    tibble::tibble(sourceName = s$sourceName,
                   sourceKey = s$sourceKey,
                   sourceDialect = s$sourceDialect,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   vocabDatabaseSchema = vocabDatabaseSchema,
                   resultsDatabaseSchema = resultsDatabaseSchema,
                   stringsAsFactors = FALSE)
  })
  
  do.call(rbind, sourceDetails)
}

#' Retrieve the meta data of all WebApi definitions
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
#' getMetadataForAllSpecifications(baseUrl = "http://server.org:80/WebAPI")
#' }
#' 
#' @export
getMetadataForAllSpecifications <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  
  categories <- c('conceptSet',
                  'cohort',
                  'incidenceRate',
                  'estimation',
                  'prediction')
  listOfIds <- list()
  for (i in (1:length(categories))) {
    #categoryUrl crosswalks standard category names to 
    #names of the category used in WebApi end point.
    category <- categories[[i]]
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
      dplyr::mutate(category = category) %>%
      dplyr::mutate(createdDate = as.character(createdDate),
                    modifiedDate = as.character(modifiedDate))
  }
  
  # there is difference in how WebApi returns for 'cohort-characterization' and 'pathway-analysis'
  # the return are nested within 'content'
  categories <- c('characterization',
                  'pathway')
  for (i in (1:length(categories))) {
    category <- categories[[i]]
    if (category == 'characterization') 
    {
      categoryUrl = 'cohort-characterization'
    } else if (category == 'pathway') 
    {
      categoryUrl = 'pathway-analysis'
    }
    url <-
      paste(baseUrl, categoryUrl, '?size=100000000', sep = "/")
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
        dplyr::rename(
          createdDate = createdAt,
          modifiedDate = updatedAt,
          modifiedBy = updatedBy
        )
    }
    
    listOfIds[[category]] <-
      listOfIds[[category]] %>%
      dplyr::mutate(category = category) %>%
      dplyr::mutate(createdDate = as.character(createdDate),
                    modifiedDate = as.character(modifiedDate))
  }
  # to do: createdDate and modifiedDate are in character format. Need to make them date/time.
  # but this does not appear to be consistent.
  listOfIds <- dplyr::bind_rows(listOfIds) %>%
    dplyr::mutate(category = SqlRender::camelCaseToTitleCase(category))
  return(listOfIds)
}

# recursively flattens tree based structure.
.flattenTree <- function(node, accumulated) {
  if (is.null(node$children)) {
    accumulated$name <- c(accumulated$name, node$name);
    accumulated$size <- c(accumulated$size, node$size);
    return(accumulated)
  } else {
    for (child in node$children) {
      accumulated <- .flattenTree(child, accumulated)
    }
    return(accumulated)
  }
}

# converts time in integer/milliseconds to date-time with timezone.
# assumption is that the system timezone = time zone of the local server running WebApi.
.millisecondsToDate <- function(milliseconds) {
  sec <- milliseconds/1000
  return(as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone()))
}

#' Post a definition into WebApi
#'
#' @details
#' Post a definition into WebAPI
#'
#' @template BaseUrl 
#' @param name        A valid name for the definition. WebApi will use this name (if valid) as
#'                    the name of the definition. WebApi checks for validity,
#'                    such as uniqueness, unaccepted character etc. An error might be thrown.
#' @param type        The type of expression in WebApi. Currently only 'cohort' is supported 
#'                    to refer cohort definition specification expression.
#' @param object      An R list object containing the expression for the specification. 
#'                    This will be converted to JSON by function and posted into the WebApi.
#'                    Note: only limited checks are performed in R to check the validity of this
#'                    expression.               
#' @return            This function will return a dataframe object with one row
#'                    describing the posted WebApi expression and its details.
#'                    If unsuccessful a STOP message will be shown.
#'                    See \code{\link{getMetadataForAllSpecifications}}. 
#'                         
#' @examples
#' \dontrun{
# validJsonExpression <- getCohortDefinition(baseUrl = baseUrl, cohortId = 15873)$expression
# postSpecification(name = 'new name for expression in target',
#                   baseUrl = "http://server.org:80/WebAPI",
#                   jsonExpression = validJsonExpression)
#' }
#' @export
postDefinition <- function(baseUrl, 
                           name,
                           type = 'cohort',
                           object) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, add = errorMessage)
  checkmate::assertCharacter(type, add = errorMessage)
  checkmate::assertList(x = object, 
                        types = c('character', 'list'), 
                        any.missing = FALSE,
                        null.ok = FALSE, 
                        add = errorMessage
  )
  checkmate::reportAssertions(errorMessage)
  
  jsonExpression <- RJSONIO::toJSON(object)
  
  if (type == 'cohort') {
    json_body <- paste0("{\"name\":\"",
                        as.character(name),
                        "\",\"expressionType\": \"SIMPLE_EXPRESSION\", \"expression\":",
                        jsonExpression,
                        "}")
    # POST the JSON
    response <- httr::POST(url = paste0(baseUrl, "/cohortdefinition/"),
                           body = json_body,
                           config = httr::add_headers(.headers = c('Content-Type' = 'application/json')))
    # Expect a "200" response that it worked
    if (response$status_code != 200) {
      stop(paste0("Post attempt failed for cohort : ", name))
    } else {
      metadataForAllSpecifications <- getMetadataForAllSpecifications(baseUrl = baseUrl) %>% 
        dplyr::filter(category == 'cohortdefinition',
                      name == !!name
        )
      return(metadataForAllSpecifications)
    }
  } else {
    stop(paste0('type = ', type, " is not supported in this version. Post attempt failed."))
  }
}