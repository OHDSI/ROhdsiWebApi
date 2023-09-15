# @file ROhdsiWebApi
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

#' Get person profile data 
#' @details
#' Get a R object with person profile data. This function may be used for visualizing a patients
#' profile in tables or visualization.
#'
#' @template BaseUrl
#' @template SourceKey
#' @param personId        The personId of the person whose profile is being reviewed
#' @param indexCohortId   <OPTIONAL> Do you want to use a particular cohortId as the index cohort? If
#'                        left NULL, the WebApi will identify the earliest cohort for the person by
#'                        cohort start date and use it as the index cohort. WebApi uses the cohort
#'                        start date of the index cohort to calculate the person's index age
#'                        (ageAtIndex). WebApi will also return the relative position, in days, for
#'                        each event compared to the index cohorts start date. These relative positions
#'                        are useful to study the relationship of various events with respect to the
#'                        index cohort start date.
#' @return
#' A list of tibble data frame objects corresponding to cohorts, observationPeriod, records and
#' person.
#' @examples
#' \dontrun{
#' getPersonProfile(baseUrl = "http://server.org:80/WebAPI",
#'                  sourceKey = "sourceKey",
#'                  personId = 342342)
#' }
#'
#' @export
getPersonProfile <- function(baseUrl, sourceKey, personId, indexCohortId = NULL) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertScalar(sourceKey, add = errorMessage)
  checkmate::assertCharacter(sourceKey, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  if (is.null(indexCohortId)) {
    url <- sprintf("%s/%s/person/%s", baseUrl, sourceKey, personId)
  } else {
    url <- sprintf("%s/%s/person/%s?cohort=%s", baseUrl, sourceKey, personId, indexCohortId)
  }

  getUrl <- .GET(url)
  if (httr::http_type(getUrl) != "application/json") {
    stop(paste0(url, " API for did not return json"), call. = FALSE)
  }

  json <- httr::content(getUrl, as = "text", type = "application/json", encoding = "UTF-8")
  if (stringr::str_detect(string = json, pattern = "An exception ocurred")) {
    stop(paste0(url, " API call returned an Exception error"), call. = FALSE)
  } else {
    data <- jsonlite::fromJSON(txt = json,
                               simplifyVector = TRUE,
                               simplifyDataFrame = TRUE,
                               flatten = TRUE)
  }

  # records
  data$records$startDate <- .millisecondsToDate(data$records$startDate)
  data$records$startDate <- lubridate::as_datetime(data$records$startDate)
  data$records$endDate <- .millisecondsToDate(data$records$endDate)
  data$records$endDate <- dplyr::case_when(is.na(data$records$endDate) ~ lubridate::as_datetime(data$records$startDate + (data$records$endDay - data$records$startDay)),
                                           TRUE ~ lubridate::as_datetime(data$records$endDate))

  # cohorts
  data$cohorts$startDate <- .millisecondsToDate(data$cohorts$startDate)
  data$cohorts$startDate <- lubridate::as_datetime(data$cohorts$startDate)
  data$cohorts$endDate <- .millisecondsToDate(data$cohorts$endDate)
  data$cohorts$endDate <- lubridate::as_datetime(data$cohorts$endDate)
  data$cohorts$personId <- personId

  # observation period
  data$observationPeriods$startDate <- .millisecondsToDate(data$observationPeriods$startDate)
  data$observationPeriods$startDate <- lubridate::as_datetime(data$observationPeriods$startDate)
  data$observationPeriods$endDate <- .millisecondsToDate(data$observationPeriods$endDate)
  data$observationPeriods$endDate <- lubridate::as_datetime(data$observationPeriods$endDate)

  # person
  data$person <- dplyr::tibble(personId = personId,
                               gender = data$gender,
                               yearOfBirth = data$yearOfBirth,
                               ageAtIndex = data$ageAtIndex,
                               recordCount = data$recordCount,
                               indexCohortId = indexCohortId)
  data <- list(records = data$records %>% tidyr::tibble(),
               cohorts = data$cohorts %>% tidyr::tibble(),
               observationPeriods = data$observationPeriods %>% tidyr::tibble(),
               person = data$person)
  return(data)
}
