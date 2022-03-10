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



#' Fetch concept set definition from WebAPI by SourceKey \lifecycle{stable}
#' @details
#' Fetches a concept set definition from WebAPI by SourceKey. If SourceKey is not specified, the
#' priority vocabulary will be used.
#'
#' @template BaseUrl
#' @param conceptSetId   the id of the concept set to retrieve.
#' @template vocabularySourceKey
#' @return
#' An R object representing the ConceptSet definition
#'
#' @examples
#' \dontrun{
#' conceptSetDefinition <- getConceptSetDefinitionBySourceKey(conceptSetId = 282,
#'                                                            baseUrl = "http://server.org:80/WebAPI",
#'
#'   vocabularySourceKey = "MY_VOCAB")
#' conceptIds <- resolveConceptSet(conceptSetDefinition = conceptSetDefinition,
#'                                 baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getConceptSetDefinitionBySourceKey <- function(conceptSetId, baseUrl, vocabularySourceKey = NULL) {
  .checkBaseUrl(baseUrl)

  if (missing(vocabularySourceKey) || is.null(vocabularySourceKey)) {
    vocabularySourceKey <- getPriorityVocabularyKey(baseUrl = baseUrl)
  }

  urlDef <- paste0(baseUrl, "/conceptset/", conceptSetId)
  urlExpresion <- paste0(baseUrl, "/conceptset/", conceptSetId, "/expression/", vocabularySourceKey)

  responseDef <- .GET(url = urlDef)
  if (!responseDef$status_code == 200) {
    stop(paste0("Failed to load concept set definition: ",
                conceptSetId,
                " by the WebApi. Status code = ",

      httr::content(responseDef)$status_code))
  }

  responseExpression <- .GET(url = urlExpresion)
  if (!responseExpression$status_code == 200) {
    stop(paste0("Failed to load concept set expression: ",
                conceptSetId,
                " by the WebApi. Status code = ",

      httr::content(responseDef)$status_code))
  }


  responseDef <- httr::content(responseDef)
  responseExpression <- httr::content(responseExpression)
  result <- responseDef
  result$expression <- responseExpression
  return(result)

}
