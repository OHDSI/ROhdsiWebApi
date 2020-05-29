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


#' Resolve a concept set to the included standard concept IDs
#' 
#' @template vocabularySourceKey
#' 
#' @template BaseUrl
#' @template ConceptSetDefinition
#' @return
#' A vector of standard concept ids.
#'
#' @examples
#' \dontrun{
#' conceptSetDefinition <- getConceptSetDefinition(conceptSetId = 282, 
#'                                                 baseUrl = "http://server.org:80/WebAPI")
#' conceptIds <- resolveConceptSet(conceptSetDefinition = conceptSetDefinition,
#'                                 baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
resolveConceptSet <- function(conceptSetDefinition, baseUrl, vocabularySourceKey = NULL) {
  
  .checkBaseUrl(baseUrl)
  
  if (missing(vocabularySourceKey) || is.null(vocabularySourceKey)) {
    vocabularySourceKey <- getPriorityVocabularyKey(baseUrl = baseUrl)
  }
  
  url <- sprintf("%1s/vocabulary/%2s/resolveConceptSetExpression", baseUrl, vocabularySourceKey)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  expression <- RJSONIO::toJSON(conceptSetDefinition$expression)
  data <- httr::POST(url, body = expression, config = httr::add_headers(httpheader))
  data <- httr::content(data)
  if (!is.null(data$payload$message)) {
    stop(data$payload$message)
  }
  data <- unlist(data)
  return(data)
}

#' Convert a concept set definition to a table
#' 
#' @template ConceptSetDefinition
#' @template SnakeCaseToCamelCase
#' 
#' @return
#' A tibble representing the concept set expression.
#'
#' @examples
#' \dontrun{
#' conceptSetDefinition <- getConceptSetDefinition(conceptSetId = 282, 
#'                                                 baseUrl = "http://server.org:80/WebAPI")
#' convertConceptSetDefinitionToTable(conceptSetDefinition = conceptSetDefinition)
#' }
#'
#' @export
convertConceptSetDefinitionToTable <- function(conceptSetDefinition, snakeCaseToCamelCase = TRUE) {
  lists <- lapply(conceptSetDefinition$expression$items, function(x) {
    x <- append(x$concept, x)
    x$concept <- NULL
    return(tibble::as_tibble(x))
  })
  result <- dplyr::bind_rows(lists)
  if (snakeCaseToCamelCase) {
    colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  }
  return(result)
}

.setExpressionToDf <- function(json) {
  
  lists <- lapply(json$items, function(j) {
    as.data.frame(j)
  })
  
  do.call("rbind", lists)
}

.getIncludedConceptsDf <- function(baseUrl, vocabularySourceKey, includedConcepts) {
  url <- sprintf("%s/vocabulary/%s/lookup/identifiers", baseUrl, vocabularySourceKey)
  body <- RJSONIO::toJSON(includedConcepts, digits = 23)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  req <- httr::content(req)
  
  lists <- lapply(req, function(r) {
    as.data.frame(r)
  })
  
  do.call("rbind", lists)
}

.getMappedConceptsDf <- function(baseUrl, vocabularySourceKey, includedConcepts) {
  url <- sprintf("%s/vocabulary/%s/lookup/mapped", baseUrl, vocabularySourceKey)
  body <- RJSONIO::toJSON(includedConcepts, digits = 23)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  req <- httr::content(req)
  
  lists <- lapply(req, function(r) {
    modList <- .convertNulltoNA(r)
    as.data.frame(modList, stringsAsFactors = FALSE)
  })
  
  do.call("rbind", lists)
}

#' Save a set of concept sets expressions, included concepts, and mapped concepts into a workbook
#'
#' @param conceptSetIds   A vector of concept set IDs.
#' @param fileName        The name of the XLSX workbook file.
#' @template BaseUrl
#' @param included        Should included concepts be included in the workbook?
#' @param mapped          Should mapped concepts be included in the workbook?
#'
#' @return
#' A xlsx workbook that includes a list of all concept set IDs and names
#' and a worksheet for the concepts in each set. Options to include an included concepts and mapped
#' concepts worksheet for each concept set are available.
#'
#' @export
createConceptSetWorkbook <- function(conceptSetIds,
                                     fileName,
                                     baseUrl,
                                     included = FALSE,
                                     mapped = FALSE) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(conceptSetIds, add = errorMessage)
  checkmate::assertPathForOutput(fileName, overwrite = TRUE, add = errorMessage)
  checkmate::assertLogical(included, add = errorMessage)
  checkmate::assertLogical(mapped, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  if (!is.vector(conceptSetIds))
    stop("conceptSetIds argument must be a numeric vector")
  
  conceptSetDefinitions <- lapply(conceptSetIds, getConceptSetDefinition, baseUrl = baseUrl) 
  
  conceptSets <- data.frame(conceptSetId = sapply(conceptSetDefinitions, function(x) x$id), 
                            conceptSetName = sapply(conceptSetDefinitions, function(x) x$name))
  
  wb <- openxlsx::createWorkbook()
  .createSheet(wb = wb,
               label = "conceptSetIds",
               contents = conceptSets)
  
  for (conceptSetDefinition in conceptSetDefinitions) {
    .createSheet(wb = wb,
                 label = sprintf("expression_%s", conceptSetDefinition$id),
                 contents = convertConceptSetDefinitionToTable(conceptSetDefinition))
    
    if (included || mapped) {
      standardConceptsIds <- resolveConceptSet(conceptSetDefinition, baseUrl)
      
      if (included) {
        .createSheet(wb = wb,
                     label = sprintf("included_%s", conceptSetDefinition$id),
                     contents = getConcepts(standardConceptsIds, baseUrl))
      }
      
      if (mapped) {
        .createSheet(wb = wb,
                     label = sprintf("mapped_%s", conceptSetDefinition$id),
                     contents = getSourceConcepts(standardConceptsIds, baseUrl = baseUrl))
      }
    }
  }
  openxlsx::saveWorkbook(wb = wb, file = fileName, overwrite = TRUE)
}

.createSheet <- function(wb, label, contents) {
  openxlsx::addWorksheet(wb = wb, sheetName = label)
  openxlsx::writeDataTable(wb = wb,
                           sheet = label,
                           x = contents,
                           colNames = TRUE,
                           rowNames = FALSE,
                           withFilter = FALSE)
  openxlsx::setColWidths(wb = wb,
                         sheet = label,
                         cols = 1:ncol(contents),
                         widths = "auto")
}
