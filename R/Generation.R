# @file InvokeGeneration
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


#' Invoke generation. \lifecycle{stable}
#' @details
#' Invoke generation (execution) information.
#'
#' @template BaseUrl
#' @template Id
#' @template Category
#' @template SourceKey
#' @return
#' A dataframe with generation information such as status, jobName, and time.
#'
#'
#' @examples
#' \dontrun{
#' invokeGeneration(id = 13242, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
invokeGeneration <- function(id, baseUrl, sourceKey, category) {
  baseUrl <- gsub("/$", "", baseUrl)
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories() %>%
    dplyr::filter(.data$categoryStandard %in% c("cohort",
                                                "characterization",
                                                "pathway",
                                                "incidenceRate"))

  argument <- arguments %>%
    dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertSubset(x = category, choices = argument$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  if (!all(isValidSourceKey(sourceKeys = sourceKey, baseUrl = baseUrl))) {
    stop(paste0(sourceKey, " is not present in WebApi."))
  }

  urlRoot <- paste0(baseUrl,
                    "/",
                    argument$categoryUrl,
                    "/",
                    id,
                    "/",
                    argument$categoryUrlGeneration)
  url <- paste0(urlRoot, "/", sourceKey)

  if (argument$categoryStandard %in% c("cohort", "incidenceRate")) {
    response <- .GET(url)
  }
  if (argument$categoryStandard %in% c("characterization", "pathway")) {
    response <- .POST(url)
  }
  if (!response$status_code == 200) {
    if (isValidId(ids = id, baseUrl = baseUrl, category = category)) {
      error <- paste0(argument$categoryFirstUpper, " ", id, " is not present in WebApi.")
    } else {
      error <- ""
    }
    stop(paste0(error, response$status_code))
  }
  response <- httr::content(response)
  response <- response %>%
    purrr::map(function(x) {
      purrr::map(x, function(y) {
        ifelse(is.null(y), NA, y)
      })
    }) %>%
    unlist(recursive = TRUE, use.names = TRUE) %>%
    as.matrix() %>%
    t() %>%
    tidyr::as_tibble() %>%
    .removeStringFromDataFrameName(string = "jobInstance.") %>%
    .removeStringFromDataFrameName(string = "jobParameters.") %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("_")), .funs = SqlRender::snakeCaseToCamelCase) %>%
    utils::type.convert(as.is = TRUE, dec = ".") %>%
    .addSourceKeyToSourceId(baseUrl = baseUrl) %>%
    .addSourceNameToSourceKey(baseUrl = baseUrl) %>%
    .normalizeDateAndTimeTypes()

  writeLines(paste0("Generation of ",
                    argument$categoryFirstUpper,
                    " definition id: ",
                    id,
                    " for sourceKey: ",

    sourceKey, " invoked."))
  return(response)
}


#' Get generation information. \lifecycle{stable}
#' @details
#' Get generation (execution) information.
#'
#' @template BaseUrl
#' @param id         An integer id representing the id that uniquely identifies a definition for the
#'                   category in a WebApi instance.
#' @param category   The category of expression in WebApi. Only the following strings are accepted:
#'                   'cohort', 'characterization', 'pathway', 'incidenceRate'.
#' @return
#' An R object with the generation information.
#'
#' @examples
#' \dontrun{
#' getGenerationInformation(id = 13242,
#'                          category = "cohort",
#'                          baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
getGenerationInformation <- function(id, category, baseUrl) {
  baseUrl <- gsub("/$", "", baseUrl)
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories() %>%
    dplyr::filter(.data$categoryStandard %in% c("cohort",
                                                "characterization",
                                                "pathway",
                                                "incidenceRate"))

  argument <- arguments %>%
    dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertSubset(x = category, choices = argument$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  urlRoot <- paste0(baseUrl,
                    "/",
                    argument$categoryUrl,
                    "/",
                    id,
                    "/",
                    argument$categoryUrlGenerationInformation)

  cdmSources <- getCdmSources(baseUrl)
  validSourceKeys <- cdmSources %>%
    dplyr::select(.data$sourceKey) %>%
    dplyr::distinct() %>%
    dplyr::pull(.data$sourceKey)

  ##### cohort/characterization/pathway ####
  if (argument$categoryStandard %in% c("cohort", "characterization", "pathway")) {
    url <- urlRoot
    response <- .GET(url)
    if (!response$status_code == 200) {
      definitionsMetaData <- getDefinitionsMetadata(baseUrl = baseUrl, category = category)
      if (!id %in% definitionsMetaData$id) {
        error <- paste0(argument$categoryFirstUpper, ": ", id, " not found.")
      } else {
        error <- ""
      }
      stop(paste0(error, "Status code = ", httr::content(response)$status_code))
    }
    response <- httr::content(response)
    if (!length(response) == 0) {
      responseAll <- list()
      for (i in (1:length(response))) {
        responseAll[[i]] <- response[[i]] %>%
          purrr::map(function(x) {
          purrr::map(x, function(y) {
            ifelse(is.null(y), NA, y)
          })
          }) %>%
          unlist(recursive = TRUE, use.names = TRUE) %>%
          as.matrix() %>%
          t() %>%
          tidyr::as_tibble() %>%
          .removeStringFromDataFrameName(string = "id.") %>%
          utils::type.convert(as.is = TRUE, dec = ".") %>%
          .addSourceKeyToSourceId(baseUrl = baseUrl) %>%
          .addSourceNameToSourceKey(baseUrl = baseUrl) %>%
          .normalizeDateAndTimeTypes()
      }
      response <- dplyr::bind_rows(responseAll)
      denominator <- nrow(response)
      numerator <- nrow(response %>%
        dplyr::filter(.data$status %in% c("COMPLETE", "COMPLETED")))
    } else {
      denominator <- 0
      numerator <- 0
      response <- tidyr::tibble()
    }
  }

  ##### incidence rate ####
  if (argument$categoryStandard == "incidenceRate") {
    executionInfo <- list()
    summaryList <- list()
    # looping through sourceKeys. https://github.com/OHDSI/ROhdsiWebApi/issues/102
    for (i in (1:length(validSourceKeys))) {
      url <- paste0(urlRoot, "/", validSourceKeys[[i]])
      response <- .GET(url)
      if (!response$status_code == 200) {
        definitionsMetaData <- getDefinitionsMetadata(baseUrl = baseUrl, category = category)
        if (!id %in% definitionsMetaData$id) {
          error <- paste0(argument$categoryFirstUpper, ": ", id, " not found.")
        } else {
          error <- ""
        }
        stop(paste0(error, "Status code = ", httr::content(response)$status_code))
      }
      response <- httr::content(response)
      if (length(response$executionInfo) > 0) {
        executionInfo[[i]] <- response$executionInfo %>%
          purrr::map(function(x) {
          purrr::map(x, function(y) {
            ifelse(is.null(y), NA, y)
          })
          }) %>%
          unlist(recursive = TRUE, use.names = TRUE) %>%
          as.matrix() %>%
          t() %>%
          tidyr::as_tibble() %>%
          .removeStringFromDataFrameName(string = "id.") %>%
          utils::type.convert(as.is = TRUE, dec = ".") %>%
          .addSourceKeyToSourceId(baseUrl = baseUrl) %>%
          .addSourceNameToSourceKey(baseUrl = baseUrl) %>%
          .normalizeDateAndTimeTypes()
      }
      if (length(response$summaryList) > 0) {
        summaryList[[i]] <- dplyr::bind_rows(response$summaryList)
      }
    }
    response <- list(executionInfo = dplyr::bind_rows(executionInfo),
                     summaryList = dplyr::bind_rows(summaryList))
    denominator <- nrow(response$executionInfo)
    numerator <- nrow(response$executionInfo %>%
      dplyr::filter(.data$status %in% c("COMPLETE", "COMPLETED")))
  }
  writeLines(paste0("Found ",
                    numerator,
                    " generations for ",
                    argument$categoryFirstUpper,
                    " of which ",

    scales::percent(x = numerator/denominator, accuracy = 0.1), " had a status = 'COMPLETED'"))
  return(response)
}


#' Invoke generation. \lifecycle{stable}
#' @details
#' Invoke generation (execution) information.
#'
#' @template BaseUrl
#' @template Id
#' @template Category
#' @template SourceKey
#' @return
#' Error message if invoke failed.
#'
#'
#' @examples
#' \dontrun{
#' cancelGeneration(id = 13242, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
cancelGeneration <- function(id, baseUrl, sourceKey, category) {
  baseUrl <- gsub("/$", "", baseUrl)
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories() %>%
    dplyr::filter(.data$categoryStandard %in% c("cohort",
                                                "characterization",
                                                "pathway",
                                                "incidenceRate"))

  argument <- arguments %>%
    dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertSubset(x = category, choices = argument$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  if (!all(isValidSourceKey(sourceKeys = sourceKey, baseUrl = baseUrl))) {
    stop(paste0(sourceKey, " is not present in WebApi."))
  }

  urlRoot <- paste0(baseUrl, "/", argument$categoryUrl, "/", id, "/", argument$categoryUrlCancel)
  url <- paste0(urlRoot, "/", sourceKey)

  if (argument$categoryStandard %in% c("cohort")) {
    response <- .GET(url)
  }
  if (argument$categoryStandard %in% c("characterization", "pathway", "incidenceRate")) {
    response <- .DELETE(url)
  }
  if (!response$status_code %in% c(200, 204)) {
    if (isValidId(ids = id, baseUrl = baseUrl, category = category)) {
      error <- paste0(argument$categoryFirstUpper, " ", id, " is not present in WebApi.")
    } else {
      error <- ""
    }
    stop(error, response$status_code)
  }
  warning(paste0("Generation of ",
                 argument$categoryFirstUpper,
                 " definition id: ",
                 id,
                 " for sourceKey: ",

    sourceKey, " requested to be stopped."))
  # nothing to return.
}

