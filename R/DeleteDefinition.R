# @file deleteDefinition
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


#' Delete a definition id of a chosen category. \lifecycle{stable}
#' @details
#' Delete the definition for an id of chosen category in WebApi.
#'
#' @template BaseUrl
#' @template category
#' @template id
#' @return
#' None, unless error.
#'
#' @examples
#' \dontrun{
#' deleteDefinition(id = 13242, baseUrl = "http://server.org:80/WebAPI", category = "cohort")
#' }
#' @export
deleteDefinition <- function(id, baseUrl, category) {
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertChoice(x = category, choices = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  url <- paste0(baseUrl, "/", argument$categoryUrl, "/", id)
  request <- httr::DELETE(url)

  if (!request$status %in% c(200, 204)) {
    if (!isTRUE(isValidId(ids = id, baseUrl = baseUrl, category = category))) {
      error <- paste0(argument$categoryFirstUpper, " definition id: ", id, " not found. ")
    } else {
      error <- ""
    }
    err <- paste0(error, "Request status code: ", httr::http_status(request)$message)
    ParallelLogger::logError(err)
    stop(err)
  } else {
    ParallelLogger::logInfo("Successfully deleted ",
                            category,
                            " definition id ",
                            id,
                            ". Request status code: ",
                            httr::http_status(request)$message)
  }
}
