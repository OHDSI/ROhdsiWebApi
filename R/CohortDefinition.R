# @file CohortDefinition
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

#' Get SQL query for Cohort definition.
#'
#' @details
#' Given a valid Cohort definition R-object (not JSON) this function will return the parameterized SQL
#' in OHDSI SQL dialect. This SQL may be used along with OHDSI R-package 'SQLRender' to
#' render/translate to target SQL dialect and parameters rendered.
#'
#' @template BaseUrl
#' @param cohortDefinition   An R list object (not JSON) representing the Cohort definition. It is the
#'                           output R expression object of list object from \code{CohortDefinition}
#' @param generateStats      Should the SQL include the code for generating inclusion rule statistics?
#'                           Note that if TRUE, several additional tables are expected to exists as
#'                           described in the details. By default this is TRUE.
#' @return
#' An R object containing the SQL for Cohort definition.
#'
#' @examples
#' \dontrun{
#' getCohortSql(CohortDefinition = (getCohortDefinition(cohortId = 13242, baseUrl = baseUrl)),
#'              baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
getCohortSql <- function(cohortDefinition, baseUrl, generateStats = TRUE) {
  .checkBaseUrl(baseUrl)

  arguments <- .getStandardCategories()
  argument <- arguments %>%
    dplyr::filter(.data$categoryStandard == "cohort")

  if (!"cohort" %in% c("cohort")) {
    stop(paste0("Retrieving SQL for ", argument$categoryFirstUpper, " is not supported"))
  }

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertList(x = cohortDefinition, min.len = 1, add = errorMessage)
  checkmate::reportAssertions(errorMessage)

  url <- paste0(baseUrl, "/", argument$categoryUrl, "/sql/")
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")

  if ("expression" %in% names(cohortDefinition)) {
    expression <- cohortDefinition$expression
  } else {
    expression <- cohortDefinition
  }
  listGenerateStats <- list(expression = expression, options = list(generateStats = generateStats))

  validJsonExpression <- .toJSON(listGenerateStats)
  response <- .POST(url, body = validJsonExpression, config = httr::add_headers(httpheader))
  if (response$status == 200) {
    response <- httr::content(response)
    sql <- response$templateSql
    return(sql)
  } else {
    stop(paste0("Error: No Sql returned for cohort definition"))
  }
}
