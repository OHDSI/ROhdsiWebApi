# @file postDefinition
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

#' Post a definition into WebApi \lifecycle{maturing}
#' @details
#' Post a definition into WebAPI. Currently only cohort and concept-set are supported.
#'
#' @template BaseUrl
#' @param name             A valid name for the definition. WebApi will use this name (if valid) as the
#'                         name of the definition. WebApi checks for validity, such as uniqueness,
#'                         absence of unacceptable character etc. An error might be thrown.
#' @template Category
#' @param definition       An R list object containing the expression for the specification. This will
#'                         be converted to JSON expression by function and posted into the WebApi.
#'                         Note: only limited checks are performed in R to check the validity of this
#'                         expression.
#' @param duplicateNames   How to handle importing a definition with a name that already exists in
#'                         ATLAS. 'error' will throw an error, 'overwrite' will attempt to overwrite
#'                         the existing definition, 'rename' will append the new defintion name with
#'                         (1) until the name is unique
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
postDefinition <- function(baseUrl, name, category, definition, duplicateNames) {
  
  .checkBaseUrl(baseUrl)
  arguments <- .getStandardCategories()
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, add = errorMessage)
  checkmate::assertCharacter(category, add = errorMessage)
  checkmate::assertNames(x = category, subset.of = arguments$categoryStandard)
  checkmate::reportAssertions(errorMessage)
  
  if (!category %in% c("cohort", "conceptSet", "pathway", "characterization")) {
    ParallelLogger::logError("Posting definitions of ", category, " is not supported.")
    stop()
  }
  
  if ("expression" %in% names(definition)) {
    expression <- definition$expression
  } else {
    expression <- definition
  }
  
  name <- .checkModifyDefinitionName(name = name, baseUrl = baseUrl, category = category, duplicateNames = duplicateNames)
  
  if (category %in% c("pathway")) {
    
    expression$targetCohorts <- purrr::map(expression$targetCohorts, .postModifyCohortDef, baseUrl, duplicateNames)
    expression$eventCohorts <- purrr::map(expression$eventCohorts, .postModifyCohortDef, baseUrl, duplicateNames)
    
  }
  
  if (category %in% c("characterization")) {
    
    expression$cohorts <- purrr::map(expression$cohorts, .postModifyCohortDef, baseUrl, duplicateNames)
    
  }
  
  json <- .definitionToJson(expression = expression, category = category, name = name)
  
  url <- paste0(baseUrl, "/", argument$categoryUrl, "/")
  
  if (category == "characterization") {
    url <- paste0(url, argument$categoryUrlPostExpression, "/")
  }
  
  response <- .postJson(url = url, json = json)
  
  response <- httr::content(response)
  structureCreated <- response
  response$expression <- NULL
  
  if (category %in% c("pathway")) {
    response$targetCohorts <- NULL
    response$eventCohorts <- NULL
    response$createdBy <- NULL
  }
  
  if (category %in% c("characterization")) {
    response <- response[c("id","name","createdAt","status","hashCode")]
  }
  
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
    expression <- jsonlite::toJSON(x = items)
    responsePut <- .putJson(url = paste0(baseUrl,
                                         "/",
                                         argument$categoryUrl,
                                         "/",
                                         structureCreated$id,
                                         "/",
                                         argument$categoryUrlPut), json = expression)
    if (!responsePut$status_code == 200) {
      ParallelLogger::logError("Failed to post ",
                               argument$categoryFirstUpper,
                               " definition. Status code = ",
                               httr::content(responsePut)$status_code)
      stop()
    }
  }
  
  ParallelLogger::logInfo("Post ", argument$categoryFirstUpper, " definition was successful")
  output <- response %>% list() %>% purrr::map_df(.f = purrr::flatten) %>% utils::type.convert(as.is = TRUE,
                                                                                               dec = ".") %>% .normalizeDateAndTimeTypes()
  return(output)
}

.definitionToJson <- function(expression, category, name) {
  
  if (category %in% c("cohort", "conceptSet")) {
    
    jsonExpression <- RJSONIO::toJSON(expression)
    
    json <- paste0("{\"name\":\"", as.character(name), "\",\"expressionType\": \"SIMPLE_EXPRESSION\", \"expression\":",
                   jsonExpression,
                   "}")
  }
  
  if (category %in% c("pathway", "characterization")) {
    
    expression$name <- as.character(name)
    # convert R-object to JSON expression.
    json <- RJSONIO::toJSON(expression)
  }
  
  return(json)
  
}

.checkModifyDefinitionName <- function(baseUrl, name, category, duplicateNames) {
  
  categoryMetaData <- getDefinitionsMetadata(baseUrl = baseUrl, category = category)
  
  if (name %in% categoryMetaData$name) {
    
    ParallelLogger::logWarn(name, " already exists in ATLAS")
    
    if (duplicateNames == "error") {
      ParallelLogger::logError("Cannot write ", category)
      ParallelLogger::logError("<<",name, ">> ALREADY EXISTS in ATLAS")
      stop()
    }
    
    if (duplicateNames == "overwrite") {
      deleteId <- categoryMetaData[categoryMetaData$name == name, ]$id
      
      tryCatch({
        output <- deleteDefinition(deleteId, baseUrl, category)
      }, error = function(cond) {
        ParallelLogger::logError("Overwriting cohort <<", name, ">> was UNSUCESSFUL")
        ParallelLogger::logError("Error message: ", cond)
        stop()
      })
      
      ParallelLogger::logWarn("Deleted existing cohort_definition_id ", deleteId)
      ParallelLogger::logWarn("In order to post <<", name, ">>")
      
    }
    
    if (duplicateNames == "rename") {
      
      name_orig <- name
      
      while (name %in% categoryMetaData$name) {
        name <- paste0(name, "(1)")
      }
      
      ParallelLogger::logWarn("Renamed: <<", name_orig, ">>")
      ParallelLogger::logWarn("To: <<", name, ">>")
      return(name)
    }
    
  }
  
  return(name)
  
}

.postModifyCohortDef <- function(cohortDef, baseUrl, duplicateNames) {
  output <- postCohortDefinition(cohortDef$name, cohortDef, baseUrl, duplicateNames)
  cohortDef$name <- output$name
  cohortDef$id <- output$id
  return(cohortDef)
}
