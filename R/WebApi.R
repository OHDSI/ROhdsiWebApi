# @file WebApi
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


.checkBaseUrl <- function(baseUrl) {
  patterns <- list("https?:\\/\\/[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})+(\\/.*)?\\/WebAPI$",
                   "https?:\\/\\/(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])(:[0-9]{1,5})+(\\/.*)?\\/WebAPI$")
  results <- lapply(patterns, function(p) {
    result <- grepl(pattern = p, 
                    x = baseUrl, 
                    ignore.case = FALSE)
  })
  success <- any(as.logical(results))
  
  if (!success) {
    stop("Base URL not valid, should be like http://server.org:80/WebAPI")  
  }
}

#' Get the version of the WebAPI
#'
#' @details
#' Obtains the WebAPI version number
#'
#' @param baseUrl      The base URL for the WebApi instance, for example:
#'                     "http://server.org:80/WebAPI".
#'
#' @return
#' The WebAPI version
#'
#' @export
getWebApiVersion <- function(baseUrl) {
  url <- sprintf("%s/info", baseUrl)
  json <- httr::GET(url)
  (httr::content(json))$version
}

.getSourceIdFromKey <- function(baseUrl, sourceKey) {
  .checkBaseUrl(baseUrl)
  
  url <- sprintf("%1s/source/%2s", baseUrl, sourceKey)
  
  json <- httr::GET(url)
  json <- httr::content(json)
  if (is.null(json$sourceId))
    json$sourceId <- -1
  json$sourceId
}

.formatName <- function(name) {
  gsub("_", " ", gsub("\\[(.*?)\\]_", "", gsub(" ", "_", name)))
}