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



#' Retrieve the details of all WebApi definitions, by functional category.
#'
#' @details
#' Obtains the meta data details of WebApi specifications such as id, name, created/modified 
#' details, hash object, etc. The following function categories are supported. 
#' Concept-set, Cohort-definition, Cohort-characterization, Pathway-analysis, Incidence rate (ir), 
#' estimation and prediction. This function is useful to retrieve the current specifications.
#'
#' @param baseUrl   The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'
#' @return          A tibble of specification metadata. Note: modifiedDate and createdDate are
#'                  returned as text/character (to be worked on in future version).
#' @examples
#' \dontrun{
#' getMetadataForAllSpecifications(baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
getMetadataForAllSpecifications <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  categories <- c('conceptset',
                  'cohortdefinition',
                  'ir',
                  'estimation',
                  'prediction')
  
  listOfIds <- list()
  for (i in (1:length(categories))) {
    category <- categories[[i]]
    url <- paste(baseUrl, category, '?size=100000000', sep = "/")
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
  
  # there is difference in how webApi returns for 'cohort-characterization' and 'pathway-analysis'
  # the return are nested within 'content'
  categories <- c('cohort-characterization',
                  'pathway-analysis')
  for (i in (1:length(categories))) {
    category <- categories[[i]]
    url <-
      paste(baseUrl, category, '?size=100000000', sep = "/")
    request <- httr::GET(url)
    httr::stop_for_status(request)
    listOfIds[[category]] <-
      httr::content(request)$content %>%
      purrr::map(function(x)
        purrr::map(x, function(y)
          ifelse(is.null(y), NA, y))) %>%  # convert NULL to NA in list
      dplyr::bind_rows()
    
    if (category == 'cohort-characterization') {
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
    dplyr::mutate(
      category = dplyr::case_when(
        category == 'conceptset' ~ 'conceptSets',
        category == 'cohortdefinition' ~ 'cohortDefinitions',
        category == 'ir' ~ 'incidenceRates',
        category == 'estimation' ~ 'estimation',
        category == 'prediction' ~ 'prediction',
        category == 'cohort-characterization' ~ 'characterizations',
        category == 'pathway-analysis' ~ 'cohortPathways'
      )
    ) %>%
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
# assumption is that the system timezone = time zone of the local server running webApi.
.millisecondsToDate <- function(milliseconds) {
  sec <- milliseconds/1000
  as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone())
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