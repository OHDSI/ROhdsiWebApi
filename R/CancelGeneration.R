# @file CancelGeneration
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


#' Invoke generation. \lifecycle{stable}
#' @details
#' Invoke generation (execution) information.
#'
#' @template WebApiConnection
#' @template Id
#' @template Category
#' @template SourceKey
#' @return
#' Error message if invoke failed.
#'
#'
#' @examples
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' cancelGeneration(id = 13242, category = "cohort")
#' }
#' @export
cancelGeneration <- function(wc, id, sourceKey, category) {
  .checkBaseUrl(wc$baseUrl)

  arguments <- .getStandardCategories() %>% dplyr::filter(.data$categoryStandard %in% c("cohort",
                                                                                        "characterization",
                                                                                        "pathway",
                                                                                        "incidenceRate"))

  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertSubset(x = category, choices = argument$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  if (!all(isValidSourceKey(sourceKeys = sourceKey, baseUrl = wc$baseUrl))) {
    err <- paste0(sourceKey, " is not present in WebApi.")
    ParallelLogger::logError(err)
    stop(err)
  }

  urlRoot <- paste0(wc$baseUrl, "/", argument$categoryUrl, "/", id, "/", argument$categoryUrlCancel)
  url <- paste0(urlRoot, "/", sourceKey)

  if (argument$categoryStandard %in% c("cohort")) {
    response <- GET(url, authHeader = wc$authHeader)
  }
  if (argument$categoryStandard %in% c("characterization", "pathway", "incidenceRate")) {
    response <- DELETE(url, authHeader = wc$authHeader)
  }
  if (!response$status_code %in% c(200, 204)) {
    if (isValidId(ids = id, baseUrl = wc$baseUrl, category = category)) {
      error <- paste0(argument$categoryFirstUpper, " ", id, " is not present in WebApi.")
    } else {
      error <- ""
    }
    err <- paste0(error, response$status_code)
    ParallelLogger::logError(err)
    stop(err)
  }
  ParallelLogger::logInfo("Generation of ",
                          argument$categoryFirstUpper,
                          " definition id: ",
                          id,
                          " for sourceKey: ",
                          sourceKey,
                          " requested to be stopped.")
  # nothing to return.
}
