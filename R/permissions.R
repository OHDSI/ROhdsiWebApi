# @file permissions
#
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

#' Assign cohort definition permissions to user.
#'
#' @details
#' The function will call the webservice to assign permissions to a cohort definition for the given userId.
#'
#' @param baseUrl     The URL to WebAPI
#' @param cohortId    The ID of the cohort definition
#' @param userId      The userId to grant permission
#' @param permission  The permission type to assign.  Can be READ or WRITE. Defaults to "WRITE".    
#'
#' @examples
#' \dontrun{
#' getCohortSql(CohortDefinition = (getCohortDefinition(cohortId = 13242, baseUrl = baseUrl)),
#'              baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
addCohortPermission <- function(baseUrl, cohortId, userId, permission="WRITE") {
  
  if (!permission %in% c("READ", "WRITE")) {
    stop(paste0("Invalid Permission Type:", permission, ".  Valid types are READ or WRITE."))
  }
  
  url <- paste0(baseUrl, "/permission/access/COHORT_DEFINITION/", cohortId, "/role/",userId)
  payload <- paste0("{\"accessType\":\"", permission, "\"}")
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")

  response <- .postJson(url, payload)
  if (response$status_code != 204) {
    stop(paste0("Error: Failed to assign permissions"))
  }
  invisible()
}