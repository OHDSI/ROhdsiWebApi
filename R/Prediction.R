# @file Prediction
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


#' Get a prediction definition
#'
#' @details
#' Obtain the prediction definition from WebAPI for a given prediction id
#'
#' @template BaseUrl
#' @template PredictionId
#' 
#' @return
#' An R object representing the cohort definition
#'
#' @examples
#' \dontrun{
#' getPredictionDefinition(predictionId = 282, baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getPredictionDefinition <- function(predictionId, baseUrl) {
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(predictionId, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste(baseUrl, "prediction", predictionId, sep = "/")
  json <- httr::GET(url)
  data <- httr::content(json)
  if (!is.null(data$payload$message)) {
    stop(data$payload$message)
  }
  data$expression <- RJSONIO::fromJSON(data$specification)
  return(data)
}