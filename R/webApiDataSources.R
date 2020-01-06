# @file dataSources
#
# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of StudyManagement
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

#' Get the results from individual Achilles reports by data Source definition expression
#'
#' @details
#' Obtain the JSON expression from WebAPI for a given dataSource
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param sourceKey      The source key for a CDM instance in WebAPI, as defined in the
#'                       Configuration page
#' @param reportType     The precomputed report type in Atlas (Achilles report e.g. dashboard, datadensity, person etc)
#' @return
#' A list of object with pre computed Achilles results for the sourceKey and Achilles report
#'
#' @examples
#' \dontrun{
#' # This will obtain pre-computed summary regarding dataSource
#'
#' getDataSourceReportForSourceKey(baseUrl = "http://server.org:80/WebAPI", dataSource = 'dataSource', reportType = 'dashboard')
#' }
#'
#' @export

getDataSourceReportForSourceKey <- function(baseUrl,
                                            sourceKey,
                                            reportType){
  url <- sprintf("%s/cdmresults/%s/%s", baseUrl, sourceKey, reportType)
  .getApiResponseParse(url)
}

#' Get the all the results from all Achilles reports for all provided data Sources
#'
#' @details
#' Obtains the JSON expression from WebAPI for a list of dataSource, derived from given list of CDMschemas and returns list of tibble dataframes
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param sourceKeys    A list of source keys for a CDM instance in WebAPI, as defined in the
#'                       Configuration page.
#' @return
#' A list of object representing pre computed Achilles results for the list of sourceKey's
#'
#' @examples
#' \dontrun{
#' # This will obtain pre-computed summary regarding dataSource
#'
#' getDataSource(baseUrl = "http://server.org:80/WebAPI", cdmDatabaseSchemas = c('schema1','schema2')
#' }
#'
#' @export

getDataSource <- function(baseUrl,sourceKeys){
  require(dplyr)
  .checkWebApiBaseUrl(baseUrl)

  #validate source keys
  sourceKeys <- .getValidSourceKeys(baseUrl, sourceKeys)

  reportTypes <- c('dashboard', 'datadensity', 'person', 'visit', 'condition', 'conditionera',
                      'procedure', 'drug', 'drugera', 'measurement', 'observation', 'death', 'achillesheel'
                      )

  writeLines(paste0('Downloading Achilles data into R'))
  result <- list() #i = 1
  for (i in (1:length(sourceKeys))) {
    sourceKey = sourceKeys[[i]]
    configuration <- getConfiguration(baseUrl = baseUrl,
                                                       sourceKeys = sourceKey
                                                    )$parsed

    writeLines(paste0('Working on SourceKey = ', sourceKey))
    for (j in (1:length(reportTypes))) {
      reportType <- reportTypes[j]
      writeLines(paste0('    Downloading = ', reportType))

      result[[sourceKey]][[reportType]] <-
        ROhdsiWebApi::getDataSourceReportForSourceKey(baseUrl = baseUrl
                                                         , sourceKey = sourceKey
                                                         , reportType = reportType
                                                        )


      jsonParsed <- result[[sourceKey]][[reportType]]$parsed

      jsonNames <- names(jsonParsed)

      if (is.null(jsonNames)) {
        result[[sourceKey]][[reportType]]$parsed <- dplyr::bind_rows(jsonParsed)
      } else {
          for (k in (1:length(jsonNames))) {
            writeLines(paste0('        Downloading = ', jsonNames[[k]]))
            if (isTRUE(class(jsonParsed[[jsonNames[[k]]]]) %in% c('list', 'data.frame'))) {
              result[[sourceKey]]$reportType$parsed[[jsonNames[[k]]]] <-
                dplyr::bind_rows(jsonParsed[[jsonNames[[k]]]])
            } 
          }
      }
    }
  }
  result
}
