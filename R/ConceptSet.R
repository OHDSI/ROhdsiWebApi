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


#' Resolve a concept set to the included standard concept IDs \lifecycle{stable}
#' @details
#' Resolve a concept set to the included standard concept IDs
#'
#' @template BaseUrl
#' @template ConceptSetDefinition
#' @template vocabularySourceKey
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

  url <- paste0(baseUrl, "/vocabulary/", vocabularySourceKey, "/resolveConceptSetExpression")

  if ("expression" %in% names(conceptSetDefinition)) {
    expression <- conceptSetDefinition$expression
  } else {
    expression <- conceptSetDefinition
  }
  expression <- .toJSON(expression)
  response <- .postJson(url = url, json = expression)
  if (!response$status_code == 200) {
    stop(paste0("The concept set definition was not accepted by the WebApi. Status code = ",
                httr::content(response)$status_code))
  }
  response <- httr::content(response)
  response <- unlist(response) %>% unique() %>% sort()
  return(response)
}


#' Convert a concept set definition to a table \lifecycle{maturing}
#' @template ConceptSetDefinition
#'
#' @return
#' Takes a R (list) representation of the Concept Set expression and returns a table (dataframe)
#' representing the concept set expression. This is useful to create publication friendly output of
#' the concept set expression.
#'
#' @examples
#' \dontrun{
#' conceptSetDefinition <- getConceptSetDefinition(conceptSetId = 282,
#'                                                 baseUrl = "http://server.org:80/WebAPI")
#' convertConceptSetDefinitionToTable(conceptSetDefinition = conceptSetDefinition)
#' }
#'
#' @export
convertConceptSetDefinitionToTable <- function(conceptSetDefinition) {
  if ("expression" %in% names(conceptSetDefinition)) {
    expression <- conceptSetDefinition$expression
  } else {
    expression <- conceptSetDefinition
  }
  simplify <- expression %>% jsonlite::toJSON() %>% jsonlite::fromJSON(simplifyVector = TRUE,
                                                                       simplifyDataFrame = TRUE,
                                                                       flatten = TRUE)

  df <- .removeStringFromDataFrameName(dataFrame = simplify$items,
                                       string = "concept.") %>% dplyr::rename_at(dplyr::vars(dplyr::contains("_")), .funs = SqlRender::snakeCaseToCamelCase) %>% .normalizeDateAndTimeTypes()

  df <- tidyr::unnest(data = df, colnames(df))

  return(df)
}

#' Save a set of concept sets expressions, included concepts, and mapped concepts into a workbook
#' \lifecycle{maturing}
#' @param conceptSetIds   A vector of concept set IDs.
#' @param fileName        The name of the XLSX workbook file.
#' @template BaseUrl
#' @param included        Should included concepts be included in the workbook?
#' @param mapped          Should mapped concepts be included in the workbook?
#'
#' @return
#' A xlsx workbook that includes a list of all concept set IDs and names and a worksheet for the
#' concepts in each set. Options to include an included concepts and mapped concepts worksheet for
#' each concept set are available.
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
  .createSheet(wb = wb, label = "conceptSetIds", contents = conceptSets)

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
  openxlsx::setColWidths(wb = wb, sheet = label, cols = 1:ncol(contents), widths = "auto")
}

