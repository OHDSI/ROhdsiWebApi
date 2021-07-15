# @file Private
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

.isValidUrl <- function(urlToCheck) {
  connection <- base::url(urlToCheck)
  # try to open connection
  openConnection <- suppressWarnings(try(expr = open.connection(connection,
                                                                open = "r",
                                                                timeout = 3), silent = TRUE))
  # close connection
  suppressWarnings(try(close.connection(connection), silent = TRUE))
  response <- if (is.null(openConnection)) {
    TRUE
  } else {
    FALSE
  }
  return(response)
}

.checkBaseUrl <- function(baseUrl) {
  baseUrl <- gsub("/$", "", baseUrl)
  webApiVersion <- getWebApiVersion(baseUrl = baseUrl)
  if (is.null(webApiVersion) | length(webApiVersion) == 0) {
    stop(paste0("Could not reach WebApi. Possibly the base URL is not valid or is not reachable?\n",
                "Please verify\n",
                "- is it in the form http://server.org:80/WebAPI,\n",
                "- are you are connected to the network"))
  }
}


.getStandardCategories <- function() {
  dplyr::tibble(categoryStandard = c("conceptSet",
                                     "cohort",
                                     "incidenceRate",
                                     "estimation",
                                     "prediction",
                                     "characterization",
                                     "pathway")) %>% dplyr::mutate(categoryFirstUpper = paste0(toupper(substr(.data$categoryStandard, 1, 1)), substr(.data$categoryStandard, 2, nchar(.data$categoryStandard)))) %>% dplyr::mutate(categoryUrl = dplyr::case_when(categoryStandard ==
    "conceptSet" ~ "conceptset", categoryStandard == "cohort" ~ "cohortdefinition", categoryStandard ==
    "characterization" ~ "cohort-characterization", categoryStandard == "pathway" ~ "pathway-analysis", categoryStandard == "incidenceRate" ~ "ir", categoryStandard == "estimation" ~ "estimation", categoryStandard == "prediction" ~ "prediction", TRUE ~ "")) %>% dplyr::mutate(categoryUrlGetExpression = dplyr::case_when(categoryStandard ==
    "conceptSet" ~ "expression", categoryStandard == "characterization" ~ "design", TRUE ~ "")) %>%
    dplyr::mutate(categoryUrlGenerationInformation = dplyr::case_when(categoryStandard == "cohort" ~
      "info", categoryStandard == "characterization" ~ "generation", categoryStandard == "pathway" ~
      "generation", categoryStandard == "incidenceRate" ~ "info", TRUE ~ "")) %>% dplyr::mutate(categoryUrlGeneration = dplyr::case_when(categoryStandard ==
    "cohort" ~ "generate", categoryStandard == "characterization" ~ "generation", categoryStandard ==
    "pathway" ~ "generation", categoryStandard == "incidenceRate" ~ "execute", TRUE ~ "")) %>% dplyr::mutate(categoryUrlCancel = dplyr::case_when(categoryStandard ==
    "cohort" ~ "cancel", categoryStandard == "characterization" ~ "generation", categoryStandard ==
    "pathway" ~ "generation", categoryStandard == "incidenceRate" ~ "execute", TRUE ~ "")) %>% dplyr::mutate(categoryUrlPut = dplyr::case_when(categoryStandard ==
    "conceptSet" ~ "items", TRUE ~ "")) %>% dplyr::mutate(categoryUrlPostExpression = dplyr::case_when(categoryStandard ==
    "conceptSet" ~ "items", categoryStandard == "cohort" ~ "", categoryStandard == "characterization" ~
    "import", categoryStandard == "pathway" ~ "", categoryStandard == "incidenceRate" ~ "", TRUE ~
    "")) %>% return()
}

.standardizeColumnNames <- function(dataFrame) {
  if ("createdAt" %in% names(dataFrame)) {
    dataFrame <- dataFrame %>% dplyr::rename(createdDate = .data$createdAt)
  }
  if ("updatedAt" %in% names(dataFrame)) {
    dataFrame <- dataFrame %>% dplyr::rename(modifiedDate = .data$updatedAt)
  }
  if ("updatedBy" %in% names(dataFrame)) {
    dataFrame <- dataFrame %>% dplyr::rename(modifiedBy = .data$updatedBy)
  }
  return(dataFrame)
}

# converts time in integer/milliseconds to date-time with timezone.  assumption is that the system
# timezone = time zone of the local server running WebApi.
.millisecondsToDate <- function(milliseconds) {
  if (is.numeric(milliseconds)) {
    # we assume that WebApi returns in milliseconds when the value is numeric
    sec <- milliseconds/1000
    milliseconds <- lubridate::as_datetime(x = sec, tz = Sys.timezone())
  }
  return(milliseconds)
}

.convertToDateTime <- function(x) {
  if (is.numeric(x)) {
    x <- .millisecondsToDate(milliseconds = x)
  } else if (is.character(x)) {
    x <- stringr::str_trim(x)
    x <- lubridate::as_datetime(x = x,
                                tz = Sys.timezone(),
                                lubridate::guess_formats(x = x, orders = c("y-m-d H:M",
                                                                                                       "y-m-d H:M:S",
                                                                                                       "ymdHMS",
                                                                                                       "ymd HMS"))[1])
  }
  return(x)
}

.normalizeDateAndTimeTypes <- function(df) {
  df <- dplyr::mutate_if(.tbl = df,
                         .predicate = (stringr::str_detect(string = tolower(colnames(df)),
                                                           pattern = "date") | stringr::str_detect(string = tolower(colnames(df)), pattern = "time")),
                         .funs = .convertToDateTime)
  return(df)
}

# recursively flattens tree based structure.
.flattenTree <- function(node, accumulated) {
  if (is.null(node$children)) {
    accumulated$name <- c(accumulated$name, node$name)
    accumulated$size <- c(accumulated$size, node$size)
    return(accumulated)
  } else {
    for (child in node$children) {
      accumulated <- .flattenTree(child, accumulated)
    }
    return(accumulated)
  }
}


.addSourceKeyToSourceId <- function(dataFrame, baseUrl) {
  if ("sourceId" %in% colnames(dataFrame)) {
    cdmDataSources <- getCdmSources(baseUrl = baseUrl) %>% dplyr::select(.data$sourceId,
                                                                         .data$sourceKey)
    dataFrame <- dataFrame %>% dplyr::left_join(y = cdmDataSources, by = c("sourceId"))
  }
  return(dataFrame)
}

.addSourceNameToSourceKey <- function(dataFrame, baseUrl) {
  if ("sourceKey" %in% colnames(dataFrame)) {
    cdmDataSources <- getCdmSources(baseUrl = baseUrl) %>% dplyr::select(.data$sourceKey,
                                                                         .data$sourceName)
    dataFrame <- dataFrame %>% dplyr::left_join(y = cdmDataSources, by = c("sourceKey"))
  }
  return(dataFrame)
}

.removeStringFromDataFrameName <- function(dataFrame, string) {
  if (any(stringr::str_detect(string = colnames(dataFrame), pattern = string))) {
    names <- colnames(dataFrame)
    names <- stringr::str_replace(string = names, pattern = string, replacement = "")
    colnames(dataFrame) <- names
  }
  return(dataFrame)
}
.postJson <- function(url, json) {
  # POST the JSON
  .POST(url = url,
        body = json,
        encode = "json",
        config = httr::add_headers(.headers = c(`Content-Type` = "application/json")))
}
.putJson <- function(url, json) {
  # PUT the JSON
  .PUT(url = url,
       body = json,
       encode = "json",
       config = httr::add_headers(.headers = c(`Content-Type` = "application/json")))
}

# This function is used in places where RJSONIO::toJSON was previously used to centralize
# seralization to JSON and to ensure the proper formatting is used to prevent
# https://github.com/OHDSI/ROhdsiWebApi/issues/152
.toJSON <- function(x, pretty = FALSE) {
  return(RJSONIO::toJSON(x = x, digits = 23, pretty = pretty))
}
