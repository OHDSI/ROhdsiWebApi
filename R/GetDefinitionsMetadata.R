# @file GetDefinitionsMetadata
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

#' Retrieve the meta data for WebApi definitions of a certain category
#'
#' @details
#' Obtains the meta data of WebApi specifications such as id, name, created/modified details, hash
#' object, etc for a certain category. The following function categories are supported. Concept-set,
#' Cohort-definition, Cohort-characterization, Pathway-analysis, Incidence rate (ir), estimation and
#' prediction. This function is useful to retrieve the current specifications.
#'
#' @template BaseUrl
#' @template Category
#'
#' @return
#' A tibble of specification metadata.
#'
#' @examples
#' \dontrun{
#' getDefinitionsMetadata(baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getDefinitionsMetadata <- function(baseUrl, category) {
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == !!category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(category, min.len = 1, add = errorMessage)
  checkmate::assertNames(x = category, subset.of = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)


  categoryUrl <- argument %>% dplyr::pull(.data$categoryUrl)
  url <- paste(baseUrl, categoryUrl, "?size=100000000", sep = "/")
  request <- httr::GET(url)

  if (!request$status == 200) {
    ParallelLogger::logError(argument$categoryFirstUpper,
                             " definitions not found. Unable to retrieve meta data. Please try later.")
    stop()
  }

  # there is difference in how WebApi returns for 'cohort-characterization' and 'pathway-analysis' the
  # return are nested within 'content' group1 and group2 are categories that are different based on how
  # WebApi is implemented.
  group1 <- c("conceptSet", "cohort", "incidenceRate", "estimation", "prediction")
  group2 <- c("characterization", "pathway")
  if (category %in% group1) {
    request <- httr::content(request)
  } else if (category %in% group2) {
    request <- httr::content(request)$content
  }
  request <- tidyr::tibble(request = request) %>% tidyr::unnest_wider(request) %>% utils::type.convert(as.is = TRUE,
                                                                                                       dec = ".") %>% .standardizeColumnNames() %>% .normalizeDateAndTimeTypes()
  return(request)
}
