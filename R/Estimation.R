# @file CohortDefinition
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

#' Get an estimation definition
#'
#' @details
#' Obtain the estimation study specification from WebAPI for a given estimation id
#'
#' @template BaseUrl
#'
#' @param estimationId   The number indicating which estimation specification to fetch.
#' 
#' @return
#' An Robject representing the estimation defintion
#'
#' @examples
#' \dontrun{
#' estimation = getEstimation(estimationId = 124, 
#'                            baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getEstimation <- function(estimationId, baseUrl) {
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(estimationId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste(baseUrl, "estimation", estimationId, sep = "/")
  json <- httr::GET(url)
  data <- httr::content(json)
  if (!is.null(data$payload$message)) {
    stop(data$payload$message)
  }
  # Data at root level appears completely redundant with data in specification, so dropping root level:
  data <- RJSONIO::fromJSON(data$specification)
  return(data)
}
