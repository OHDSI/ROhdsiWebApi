# @file InvokeGeneration
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
#' invokeGeneration(id = 13242, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
invokeGeneration <- function(id, baseUrl, sourceKey, category) {
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
    ParallelLogger::logError(sourceKey, " is not present in WebApi.")
    stop()
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
    response <- httr::GET(url)
  }
  if (argument$categoryStandard %in% c("characterization", "pathway")) {
    response <- httr::POST(url)
  }
  if (!response$status_code == 200) {
    if (isValidId(ids = id, baseUrl = baseUrl, category = category)) {
      error <- paste0(argument$categoryFirstUpper, " ", id, " is not present in WebApi.")
    } else {
      error <- ""
    }
    ParallelLogger::logError(error, response$status_code)
    stop()
  }
  response <- httr::content(response)
  response <- response %>% purrr::map(function(x) {
    purrr::map(x, function(y) {
      ifelse(is.null(y), NA, y)
    })
  }) %>% unlist(recursive = TRUE,
                use.names = TRUE) %>% as.matrix() %>% t() %>% tidyr::as_tibble() %>%
    .removeStringFromDataFrameName(string = "jobInstance.") %>% .removeStringFromDataFrameName(string = "jobParameters.") %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("_")),
                     .funs = SqlRender::snakeCaseToCamelCase) %>%
    utils::type.convert(as.is = TRUE, dec = ".") %>% .addSourceKeyToSourceId(baseUrl = baseUrl) %>%
    .addSourceNameToSourceKey(baseUrl = baseUrl) %>% .normalizeDateAndTimeTypes()

  ParallelLogger::logInfo("Generation of ",
                          argument$categoryFirstUpper,
                          " definition id: ",
                          id,
                          " for sourceKey: ",
                          sourceKey,
                          " invoked.")
  return(response)
}
