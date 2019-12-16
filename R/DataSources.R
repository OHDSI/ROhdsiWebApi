# @file CohortDefinition
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
#' getDataSourceReportForSourceKey(baseUrl = "http://server.org:80/WebAPI", dataSource = 'dataSource')
#' }
#'
#' @export

getDataSourceReportForSourceKey <- function(baseUrl,
                                            sourceKey,
                                            reportType){
    url <- sprintf("%s/cdmresults/%s/%s", baseUrl, sourceKey, reportType)
    json <- httr::GET(url)
    json <- httr::content(json)
}




#' Get the all the results from all Achilles reports for all provided data Sources 
#'
#' @details
#' Obtain the JSON expression from WebAPI for a given dataSource
#'
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://server.org:80/WebAPI".
#' @param sourceKeys      A list of data source key's for a CDM instance in WebAPI, as defined in the
#'                       Configuration page
#' @return
#' A list of object representing pre computed Achilles results for the list of sourceKey's
#'
#' @examples
#' \dontrun{
#' # This will obtain pre-computed summary regarding dataSource
#'
#' getDataSourceReportsForSourceKeys(baseUrl = "http://server.org:80/WebAPI", dataSources = c('dataSource1','dataSource2')
#' }
#'
#' @export


getDataSourceReportsForSourceKeys <- function(baseUrl,
                                              listSourceKeys){
  
listReportType <- c('dashboard', 'datadensity', 'person', 'visit', 'condition', 'conditionera', 
                    'procedure', 'drug', 'drugera', 'measurement', 'observation', 'death', 'achillesheel'
                    )
listDataSourceResults <- list()

i = 1
j = 1
k = 3

for (i in (1:length(listSourceKeys))){
  for (j in (1:length(listReportType))){
    
    sourceKey <- listSourceKeys[i]
    reportType <- listReportType[j]
    
    dataSourceJson <- getDataSourceReportForSourceKey(baseUrl = baseUrl
                                                     , sourceKey = sourceKey
                                                     , reportType = reportType
                                                    )

    if (!is.null(names(dataSourceJson))){
      namesDataSourceJson <- names(dataSourceJson)
      for (k in (1:length(namesDataSourceJson))){
        b <- dataSourceJson[[namesDataSourceJson[k]]]
      
        #converts any NULL in items of the list to NA, so that it is r-friendly
        for (l in (1:length(b))){
          for (n in names(b[[l]])) {
            if (is.null(b[[l]][[n]][[1]])) {
              b[[l]][[n]][[1]] <- NA
            }
          }
        }
       
        listDataSourceResults[[sourceKey]][[reportType]][[namesDataSourceJson[k]]] <- do.call(rbind.data.frame, b)
      }
    }
    else {
      listDataSourceResults[[sourceKey]][[reportType]] <- do.call(rbind.data.frame, dataSourceJson)
    }
  }
}
listDataSourceResults
}
