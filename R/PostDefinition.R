# @file postDefinition
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

#' Post a definition into WebApi \lifecycle{maturing}
#' @details
#' Post a definition into WebAPI. Currently only cohort and concept-set are supported.
#'
#' @template BaseUrl
#' @param name         A valid name for the definition. WebApi will use this name (if valid) as the
#'                     name of the definition. WebApi checks for validity, such as uniqueness, absence
#'                     of unacceptable character etc. An error might be thrown.
#' @template Category
#' @param definition   An R list object containing the expression for the specification. This will be
#'                     converted to JSON expression by function and posted into the WebApi. Note: only
#'                     limited checks are performed in R to check the validity of this expression.
#' @return
#' This function will return a dataframe object with one row describing the posted WebApi expression
#' and its details. If unsuccessful a STOP message will be shown.
#'
#' @examples
#' \dontrun{
#' definition <- getCohortDefinition(baseUrl = baseUrl, cohortId = 15873)
#' postDefinition(name = "new name for expression in sdaddaddd",
#'                baseUrl = "http://server.org:80/WebAPI",
#'                expression = definition,
#'                category = "cohort")
#' }
#' @export
postDefinition <- function(baseUrl, name, category, definition) {
  .checkBaseUrl(baseUrl)
  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertNames(x = category, subset.of = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)

  if (!category %in% c("cohort", "conceptSet")) {
    stop(paste0("Posting definitions of ", category, " is not supported."))
  }

  if ("expression" %in% names(definition)) {
    expression <- definition$expression
  } else {
    expression <- definition
  }
  # convert R-object to JSON expression.
  jsonExpression <- .toJSON(expression)
  # create json body
  json <- paste0("{\"name\":\"", as.character(name), "\",\"expressionType\": \"SIMPLE_EXPRESSION\", \"expression\":",
                 jsonExpression,
                 "}")
  # POST Json
  url <- paste0(baseUrl, "/", argument$categoryUrl, "/")
  if (category == "characterization") {
    url <- paste0(url, argument$categoryUrlPostExpression, "/")
  }
  response <- .postJson(url = url, json = json)

  if (!response$status_code == 200) {
    definitionsMetaData <- getDefinitionsMetadata(baseUrl = baseUrl, category = category)
    if (name %in% definitionsMetaData$name) {
      error <- paste0(argument$categoryFirstUpper, ": ", name, " already exists.")
    } else {
      error <- ""
    }
    stop(paste0(error, "Status code = ", httr::status_code(response)))
  }
  response <- httr::content(response)
  structureCreated <- response
  response$expression <- NULL

  # create expression in the structure required to POST or PUT
  if (category %in% c("conceptSet")) {
    items <- convertConceptSetDefinitionToTable(conceptSetDefinition = definition) %>% dplyr::mutate(id = dplyr::row_number(),
                                                                                                     conceptId = .data$conceptId,
                                                                                                     conceptSetId = structureCreated$id,
                                                                                                     isExcluded = as.integer(.data$isExcluded),
                                                                                                     includeMapped = as.integer(.data$includeMapped),
                                                                                                     includeDescendants = as.integer(.data$includeDescendants)) %>%
      dplyr::select(.data$id,
                    .data$conceptId,
                    .data$conceptSetId,
                    .data$isExcluded,
                    .data$includeMapped,
                    .data$includeDescendants)

    itemsTranspose <- apply(items, 1, function(item) {
      list(item)
    })

    expression <- .toJSON(x = itemsTranspose, pretty = TRUE)
    responsePut <- .putJson(url = paste0(baseUrl,
                                         "/",
                                         argument$categoryUrl,
                                         "/",
                                         structureCreated$id,
                                         "/",
                                         argument$categoryUrlPut), json = expression)
    if (!responsePut$status_code == 200) {
      stop(paste0("Failed to post ",
                  argument$categoryFirstUpper,
                  " definition. Status code = ",
                  httr::content(responsePut)$status_code))
    }
  }

  if (category %in% c("characterization")) {
    characterizationPostObject <- structureCreated
    characterizationPostObject$cohorts <- definition$expression$cohorts
    characterizationPostObject$featureAnalyses <- definition$expression$featureAnalyses
    characterizationPostObject$parameters <- definition$expression$parameters
    characterizationPostObject$stratas <- definition$expression$stratas
    characterizationPostObject$strataOnly <- definition$expression$strataOnly
    characterizationPostObject$strataConceptSets <- definition$expression$strataConceptSets
    characterizationPostObject$stratifiedBy <- definition$expression$stratifiedBy

    expressionCharacterization <- list()
    expressionCharacterization$name <- characterizationPostObject$name
    expressionCharacterization$cohorts <- characterizationPostObject$cohorts
    expressionCharacterization$featureAnalyses <- characterizationPostObject$featureAnalyses
    expressionCharacterization$parameters <- characterizationPostObject$parameters
    expressionCharacterization$stratas <- characterizationPostObject$stratas
    expressionCharacterization$strataOnly <- characterizationPostObject$strataOnly
    expressionCharacterization$strataConceptSets <- characterizationPostObject$strataConceptSets
    expressionCharacterization$createdAt <- characterizationPostObject$createdAt
    expressionCharacterization$updatedAt <- characterizationPostObject$updatedAt
    expressionCharacterization$skeletonType <- characterizationPostObject$skeletonType
    expressionCharacterization$skeletonVersion <- characterizationPostObject$skeletonVersion
    expressionCharacterization$packageName <- characterizationPostObject$packageName
    expressionCharacterization$organizationName <- characterizationPostObject$organizationName
    expressionCharacterization$stratifiedBy <- characterizationPostObject$stratifiedBy

    expressionCharacterization <- jsonlite::toJSON(x = expressionCharacterization,
                                                   auto_unbox = TRUE,
                                                   digits = 23)

    response <- .putJson(url = paste0(baseUrl,
                                      "/",
                                      argument$categoryUrl,
                                      "/",
                                      structureCreated$id,
                                      "/",
                                      argument$categoryUrlPut), json = expressionCharacterization)
  }
  writeLines(paste0("Post ", argument$categoryFirstUpper, " definition was successful"))
  output <- response %>% list() %>% purrr::map_df(.f = purrr::flatten) %>% utils::type.convert(as.is = TRUE,
                                                                                               dec = ".") %>% .normalizeDateAndTimeTypes()
  return(output)
}
