# @file GetDefinition
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


#' Get the definition for an id of chosen category in WebApi. 
#' @details
#' Get the definition for an id of chosen category in WebApi. The return object will be a R
#' representation of the definition, that may be reconverted to JSON.
#'
#' @template Id
#' @template BaseUrl
#' @template Category
#' @return
#' An R object representing the definition
#'
#' @examples
#' \dontrun{
#' getDefinition(id = 13242, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
getDefinition <- function(id, baseUrl, category) {
  baseUrl <- gsub("/$", "", baseUrl)
  .checkBaseUrl(baseUrl)
  
  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertChoice(x = category, choices = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste0(baseUrl, "/", argument$categoryUrl, "/", id)
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
        expression <- .GET(urlExpression)
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
      response$expression <- RJSONIO::fromJSON(response$expression, nullValue = NA, digits = 23)
      namesResponse <- names(response)
      for (i in (1:length(namesResponse))) {
        if (stringr::str_detect(string = tolower(namesResponse[[i]]), pattern = "date")) {
          if (length(namesResponse[[i]]) == 1) {
            response[[namesResponse[[i]]]] <- .convertToDateTime(response[[namesResponse[[i]]]])
          }
        }
      }
    }
  }
  return(response)
}


#' Post a definition into WebApi 
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
  baseUrl <- gsub("/$", "", baseUrl)
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

    expression <- .toJSON(x = purrr::flatten(itemsTranspose), pretty = TRUE)
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

  # TODO implement posting of characterization
  # if (category %in% c("characterization")) {
  #   characterizationPostObject <- structureCreated
  #   characterizationPostObject$cohorts <- definition$expression$cohorts
  #   characterizationPostObject$featureAnalyses <- definition$expression$featureAnalyses
  #   characterizationPostObject$parameters <- definition$expression$parameters
  #   characterizationPostObject$stratas <- definition$expression$stratas
  #   characterizationPostObject$strataOnly <- definition$expression$strataOnly
  #   characterizationPostObject$strataConceptSets <- definition$expression$strataConceptSets
  #   characterizationPostObject$stratifiedBy <- definition$expression$stratifiedBy
  #   
  #   expressionCharacterization <- list()
  #   expressionCharacterization$name <- characterizationPostObject$name
  #   expressionCharacterization$cohorts <- characterizationPostObject$cohorts
  #   expressionCharacterization$featureAnalyses <- characterizationPostObject$featureAnalyses
  #   expressionCharacterization$parameters <- characterizationPostObject$parameters
  #   expressionCharacterization$stratas <- characterizationPostObject$stratas
  #   expressionCharacterization$strataOnly <- characterizationPostObject$strataOnly
  #   expressionCharacterization$strataConceptSets <- characterizationPostObject$strataConceptSets
  #   expressionCharacterization$createdAt <- characterizationPostObject$createdAt
  #   expressionCharacterization$updatedAt <- characterizationPostObject$updatedAt
  #   expressionCharacterization$skeletonType <- characterizationPostObject$skeletonType
  #   expressionCharacterization$skeletonVersion <- characterizationPostObject$skeletonVersion
  #   expressionCharacterization$packageName <- characterizationPostObject$packageName
  #   expressionCharacterization$organizationName <- characterizationPostObject$organizationName
  #   expressionCharacterization$stratifiedBy <- characterizationPostObject$stratifiedBy
  #   
  #   expressionCharacterization <- jsonlite::toJSON(x = expressionCharacterization,
  #                                                  auto_unbox = TRUE,
  #                                                  digits = 23)
  #   
  #   response <- .putJson(url = paste0(baseUrl,
  #                                     "/",
  #                                     argument$categoryUrl,
  #                                     "/",
  #                                     structureCreated$id,
  #                                     "/",
  #                                     argument$categoryUrlPut), json = expressionCharacterization)
  # }
  writeLines(paste0("Post ", argument$categoryFirstUpper, " definition was successful"))
  output <- response %>% list() %>% purrr::map_df(.f = purrr::flatten) %>% utils::type.convert(as.is = TRUE,
                                                                                               dec = ".") %>% .normalizeDateAndTimeTypes()
  return(output)
}

#' Update definition 
#' @details
#' Update a definition in WebAPI. Currently only cohorts are supported.
#' Takes the definition as a parameter and converts it to json. This is the full definition
#' (i.e. including name and id fields)
#' @template BaseUrl
#' @template Category
#' @param definition        An R list object containing the expression for the specification. This will be
#'                          converted to JSON expression by function and posted into the WebApi.
#' @examples
#' \dontrun{
#' definition <- getDefinition(
#'   id = 13242, 
#'   baseUrl = "http://server.org:80/WebAPI", 
#'   category = "cohort"
#' )
#' definition$name <- "My new name for this"
#' updateDefinition(definition, baseUrl, category = "cohort")
#' }
#' @export
updateDefinition <- function(definition, baseUrl, category) {
  baseUrl <- gsub("/$", "", baseUrl)
  .checkBaseUrl(baseUrl)
  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(definition$name, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertNames(x = category, subset.of = arguments$categoryStandard)
  checkmate::assertInteger(definition$id)
  checkmate::reportAssertions(errorMessage)

  # Check entity exists - this will throw a meaningful error if it doesn't
  tryCatch({
      getDefinition(definition$id, baseUrl, category)
  }, error = function(err) {
      stop(paste("Could not find", category, "definition with this id"))
  })
  entryUrl <- paste(baseUrl, argument$categoryUrl, definition$id, sep = "/")
  # Check that name does not collide with other entities
  namesCheckUrl <- paste0(entryUrl, "/exists?name=", definition$name)
  response <- .GET(namesCheckUrl)

  if (response$status_code != 200) {
    stop("Error checking name, potential problem with WebAPI instance")
  }

  content <- httr::content(response)
  if (content == 1) {
    stop(paste(definition$name, "Is not a valid cohort name."))
  }
  
  if (category == "cohort") {
    jsonExpression <- .toJSON(definition$expression)
    checkUrl <- paste(baseUrl, argument$categoryUrl, "check", sep = "/")

    tryCatch({
      response <- .postJson(checkUrl, jsonExpression)
      content <- httr::content(response)
    }, error = function (error) {
        stop(paste("Error with", category, "definition:", error))
    })
  }

  jsonExpression <- .toJSON(definition)
  response <- .putJson(entryUrl, jsonExpression)

  if (!response$status_code == 200) {
    stop(paste("Error updating definition",
               "Status code =", httr::status_code(response)))
  }

  writeLines(paste("Success: updated", argument$categoryFirstUpper, definition$id, definition$name))
  invisible()
}

#' Delete a definition id of a chosen category. 
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
  baseUrl <- gsub("/$", "", baseUrl)
  .checkBaseUrl(baseUrl)
  
  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(id, add = errorMessage)
  checkmate::assertChoice(x = category, choices = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste0(baseUrl, "/", argument$categoryUrl, "/", id)
  request <- .DELETE(url)
  
  if (!request$status %in% c(200, 204)) {
    if (!isTRUE(isValidId(ids = id, baseUrl = baseUrl, category = category))) {
      error <- paste0(argument$categoryFirstUpper, " definition id: ", id, " not found. ")
    } else {
      error <- ""
    }
    stop(paste0(error, "Request status code: ", httr::http_status(request)$message))
  } else {
    writeLines(paste0("Successfully deleted ",
                      category,
                      " definition id ",
                      id,
                      ". Request status code: ",
                      httr::http_status(request)$message))
  }
}

