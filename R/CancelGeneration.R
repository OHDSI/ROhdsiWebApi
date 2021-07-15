# @file CancelGeneration
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
                 sourceKey,
                 " requested to be stopped."))
  # nothing to return.
}
