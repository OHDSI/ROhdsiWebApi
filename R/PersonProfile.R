# @file ROhdsiWebApi
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

#' Get person profile data
#'
#' @details
#' Get a R object with person profile data. This function may be used for 
#' visualizing a patients profile in tables or visualization.
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param sourceKey      The source key for a CDM instance in WebAPI, as defined in the
#'                       Configuration page
#' @param personId       The personId of the person whoose profile is being reviewed
#' @param cohortId       <OPTIONAL> The cohortId of the cohort that the person belongs to
#' @return               A tibble data frame object
#'
#' @examples
#' \dontrun{#'
#' getPersonProfile(baseUrl = "http://server.org:80/WebAPI", 
#'                  sourceKey = 'sourceKey',
#'                  personId = 342342)
#' }
#'
#' @export
getPersonProfile <- function(baseUrl,
                             sourceKey,
                             personId,
                             cohortId = NULL){
  if (is.null(cohortId)) {
    url <- sprintf("%1s/%2s/person/%3s", baseUrl, sourceKey, personId)
  } else {
    url <- sprintf("%1s/%2s/person/%3s?cohort=%4s", baseUrl, sourceKey, personId, cohortId)
  }
  errorMessage <- checkmate::makeAssertCollection()
  # checkmate::assertInt(personId, add = errorMessage) -- fails for big integer. TODO
  # if (!is.null(cohortId)) {
  #   checkmate::assertInt(cohortId, add = errorMessage)  -- fails for big integer. TODO
  # }
  checkmate::assertScalar(sourceKey, add = errorMessage)
  checkmate::assertCharacter(sourceKey, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  data <- .getApiResponseParse(url)
  
  # records
  data$parsed$records$startDate <- .millisecondsToDate(data$parsed$records$startDate)
  data$parsed$records$startDate <- lubridate::as_datetime(data$parsed$records$startDate)
  data$parsed$records$endDate <- .millisecondsToDate(data$parsed$records$endDate)
  data$parsed$records$endDate <- dplyr::case_when(is.na(data$parsed$records$endDate) ~ 
                                                    lubridate::as_datetime(data$parsed$records$startDate +
                                                                             (data$parsed$records$endDay - 
                                                                                data$parsed$records$startDay
                                                                             )
                                                    ),
                                                  TRUE ~ lubridate::as_datetime(data$parsed$records$endDate)
  )
  
  # cohorts
  data$parsed$cohorts$startDate <- .millisecondsToDate(data$parsed$cohorts$startDate)
  data$parsed$cohorts$startDate <- lubridate::as_datetime(data$parsed$cohorts$startDate)
  data$parsed$cohorts$endDate <- .millisecondsToDate(data$parsed$cohorts$endDate)
  data$parsed$cohorts$endDate <- lubridate::as_datetime(data$parsed$cohorts$endDate)
  data$parsed$cohorts$personId <- personId
  
  # observation period
  data$parsed$observationPeriods$startDate <- .millisecondsToDate(data$parsed$observationPeriods$startDate)
  data$parsed$observationPeriods$startDate <- lubridate::as_datetime(data$parsed$observationPeriods$startDate)
  data$parsed$observationPeriods$endDate <- .millisecondsToDate(data$parsed$observationPeriods$endDate)
  data$parsed$observationPeriods$endDate <- lubridate::as_datetime(data$parsed$observationPeriods$endDate)
  
  return(data$parsed)
}

# converts time in integer/milliseconds to date-time with timezone.
# assumption is that the system timezone = time zone of the local server running webApi.
.millisecondsToDate <- function(milliseconds) {
  sec <- milliseconds/1000
  as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone())
}

# Parse API to native (json) and parsed (r-friendly format)
.getApiResponseParse <- function(url){#url <- baseUrl
  # .checkBaseUrl(baseUrl)
  getUrl <- httr::GET(url)
  if (httr::http_type(getUrl) != "application/json") {
    stop(paste0(url, " API for did not return json"), call. = FALSE)
  } 
  native <- httr::content(getUrl, as = 'text', type = "application/json", encoding = 'UTF-8')
  if (stringr::str_detect(string = native, pattern = "An exception ocurred")) {
    stop(paste0(url, " API call returned an Exception error"), call. = FALSE)
  } else {
    parsed <- jsonlite::fromJSON(txt = native, simplifyVector = TRUE, simplifyDataFrame = TRUE)
  }
  result <- list(
    native = native,
    parsed = parsed
  )
  return(result)
}