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

#' Get concepts 
#' @template vocabularySourceKey
#'
#' @template BaseUrl
#' @template SnakeCaseToCamelCase
#' @param conceptIds   A vector of concept IDs.
#'
#' @return
#' A tibble containing information on the concepts.
#'
#' @examples
#' \dontrun{
#' conceptSet <- getConceptSet(conceptSetId = 282, baseUrl = "http://server.org:80/WebAPI")
#' conceptIds <- resolveConceptSet(conceptSet = conceptSet, baseUrl = "http://server.org:80/WebAPI")
#' concepts <- getConcepts(conceptIds = conceptIds, baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getConcepts <- function(conceptIds,
                        baseUrl,
                        vocabularySourceKey = NULL,
                        snakeCaseToCamelCase = TRUE) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(conceptIds, add = errorMessage)
  checkmate::assertLogical(snakeCaseToCamelCase, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  if (missing(vocabularySourceKey) || is.null(vocabularySourceKey)) {
    vocabularySourceKey <- getPriorityVocabularyKey(baseUrl = baseUrl)
  }

  url <- sprintf("%s/vocabulary/%s/lookup/identifiers", baseUrl, vocabularySourceKey)
  body <- .toJSON(conceptIds)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  req <- .POST(url, body = body, config = httr::add_headers(httpheader))
  req <- httr::content(req)

  lists <- lapply(req, function(x) {
    idx <- sapply(x, is.null)
    idx <- names(idx)[idx]
    x[idx] <- NA
    tibble::as_tibble(x)
  })
  result <- dplyr::bind_rows(lists)
  if (snakeCaseToCamelCase) {
    colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  }
  return(result)
}



#' Get source concepts that map to standard concepts 
#' @template vocabularySourceKey
#' @template BaseUrl
#' @template SnakeCaseToCamelCase
#' @param conceptIds   A list of concept IDs referring to standard concepts.
#'
#' @return
#' A tibble containing information on the source concepts.
#'
#' @examples
#' \dontrun{
#' conceptSet <- getConceptSetDefinition(conceptSetId = 282,
#'                                       baseUrl = "http://server.org:80/WebAPI")
#' conceptIds <- resolveConceptSet(conceptSet = conceptSet, baseUrl = "http://server.org:80/WebAPI")
#' sourceConcepts <- getSourceConcepts(conceptIds = conceptIds,
#'                                     baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getSourceConcepts <- function(conceptIds,
                              baseUrl,
                              vocabularySourceKey = NULL,
                              snakeCaseToCamelCase = TRUE) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(conceptIds, add = errorMessage)
  checkmate::assertLogical(snakeCaseToCamelCase, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  if (missing(vocabularySourceKey) || is.null(vocabularySourceKey)) {
    vocabularySourceKey <- getPriorityVocabularyKey(baseUrl = baseUrl)
  }

  url <- sprintf("%s/vocabulary/%s/lookup/mapped", baseUrl, vocabularySourceKey)
  body <- .toJSON(conceptIds)
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  req <- .POST(url, body = body, config = httr::add_headers(httpheader))
  req <- httr::content(req)

  lists <- lapply(req, function(x) {
    idx <- sapply(x, is.null)
    idx <- names(idx)[idx]
    x[idx] <- NA
    tibble::as_tibble(x)
  })

  result <- dplyr::bind_rows(lists)
  if (snakeCaseToCamelCase) {
    colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  }
  return(result)
}
