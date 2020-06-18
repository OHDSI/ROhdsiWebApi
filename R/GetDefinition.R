# @file GetDefinition
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


#' Get the definition for an id of chosen category in WebApi. \lifecycle{stable}
#' @details
#' Get the definition for an id of chosen category in WebApi. The return object will be a R
#' representation of the definition, that may be reconverted to JSON.
#'
#' @template BaseUrl
#' @template Id
#' @template Category
#' @return
#' An R object representing the definition
#'
#' @examples
#' \dontrun{
#' getDefinition(id = 13242, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
getDefinition <- function(wc, id, category) {
  stopifnot("WebApiConnection" %in% class(wc))
  baseUrl <- wc[["baseUrl"]]
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertChoice(x = category, choices = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  url <- paste0(baseUrl, "/", argument$categoryUrl, "/", id)
  if(is.null(wc[["authHeader"]])) {
    response <- httr::GET(url)
  } else {
    response <- httr::GET(url, httr::add_headers(Authorization = wc[["authHeader"]]))
  }
  if (!response$status_code == 200) {
    if (response$status_code == 401) {
      err <- paste0("WebApi Authentication expired or not provided.\n", 
                    "Status code = 401 Unauthorized")
      ParallelLogger::logError(err)
      stop(err)
    }
    definitionsMetaData <- getDefinitionsMetadata(baseUrl = baseUrl, category = category)
    if (!id %in% definitionsMetaData$id) {
      error <- paste0(argument$categoryFirstUpper, ": ", id, " not found.")
    } else {
      error <- ""
    }
    err <- paste0(error, "Status code = ", httr::content(response)$status_code)
    ParallelLogger::logError(err)
    stop(err)
  }
  response <- httr::content(response)

  if (is.null(response$expression)) {
    if (!is.null(response$specification)) {
      response$expression <- response$specification
      response$specification <- NULL
    } else if (!is.null(response$design)) {
      response$expression <- response$design
      response$design <- NULL
    } else {
      if (argument$categoryUrlGetExpression != "") {
        urlExpression <- paste0(baseUrl,
                                "/",
                                argument$categoryUrl,
                                "/",
                                id,
                                "/",
                                argument$categoryUrlGetExpression)
        expression <- httr::GET(urlExpression)
        expression <- httr::content(expression)
        response$expression <- expression
      } else {
        response$expression <- response
        response$expression$name <- NULL
      }
    }
  }
  if (is.character(response$expression)) {
    if (jsonlite::validate(response$expression)) {
      response$expression <- RJSONIO::fromJSON(response$expression, nullValue = NA)
    }
  }
  return(response)
}
