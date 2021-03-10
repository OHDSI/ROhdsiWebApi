# @file CohortDefinition
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

#' Get generation results \lifecycle{stable}
#' @details
#' Get the results objects from WebApi for a definition of a certain category in WebApi.
#'
#' @template BaseUrl
#' @template Id
#' @template Category
#' @return
#' Returns the result objects for a given id and category from the WebApi.
#' @examples
#' \dontrun{
#' getResults(id = 282, category = "cohort", baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
getResults <- function(id, baseUrl, category) {
  arguments <- .getStandardCategories() %>% dplyr::filter(.data$categoryStandard %in% c("cohort",
                                                                                        "characterization",
                                                                                        "pathway",
                                                                                        "incidenceRate"))
  # create empty output that will be produced if results are not generated.
  argument <- arguments %>% dplyr::filter(.data$categoryStandard == category)
  if (category == "cohort") {
    response <- list(inclusionRuleStats = tidyr::tibble(),
                     treemapData = tidyr::tibble(),
                     summary = tidyr::tibble())
  } else if (category == "characterization") {
    response <- tidyr::tibble()
  } else if (category == "pathway") {
    response <- list(summary = tidyr::tibble(),
                     stratifyStats = tidyr::tibble(),
                     treemapData = tidyr::tibble())
  } else if (category == "incidenceRate") {
    response <- list(eventCodes = tidyr::tibble(), pathwayGroups = tidyr::tibble())
  } else {
    stop()
  }

  # check generation information
  generationInformation <- getGenerationInformation(id = id, category = category, baseUrl = baseUrl)

  if (category %in% c("cohort", "characterization", "pathway")) {
    executionInfo <- generationInformation
  } else if (category %in% c("incidenceRate")) {
    executionInfo <- generationInformation$executionInfo
  }
  if (length(executionInfo) > 0) {
    generatedSuccess <- executionInfo %>% dplyr::filter(.data$status %in% c("COMPLETE",
                                                                            "COMPLETED")) %>%
      dplyr::filter(!is.na(.data$sourceKey))

    if ("canceled" %in% colnames(generatedSuccess)) {
      if (!is.null(generatedSuccess$canceled)) {
        generatedSuccess <- generatedSuccess %>% dplyr::filter(.data$canceled == "FALSE")
      }
    }
    if ("isCanceled" %in% colnames(generatedSuccess)) {
      if (!is.null(generatedSuccess$isCanceled)) {
        generatedSuccess <- generatedSuccess %>% dplyr::filter(.data$isCanceled == "FALSE")
      }
    }
  } else {
    generatedSuccess <- tidyr::tibble()
  }

  writeLines(paste0("Found ",
                    scales::comma(nrow(generatedSuccess)),
                    " (",
                    scales::percent(x = nrow(generatedSuccess)/nrow(executionInfo), accuracy = 0.1),
                    " of ",
                    scales::comma(nrow(executionInfo)),
                    " result generations) to have configured source key in WebApi. Results will be retrieved for this subset."))


  # get results for cohort generation
  if (category == "cohort" & nrow(generatedSuccess) > 0) {
    inclusionRuleStats <- list()
    treemapData <- list()
    summary <- list()

    for (i in (1:nrow(generatedSuccess))) {
      generation <- generatedSuccess %>% dplyr::slice(i)
      writeLines(paste0("   Retrieving results for ", generation$sourceName))
      inclusionRuleStatsMode <- list()
      treemapDataMode <- list()
      summaryMode <- list()
      for (mode in (0:1)) {
        if (mode == 0) {
          modeLong <- "event"
        } else if (mode == 1) {
          modeLong <- "person"
        }
        url <- paste0(baseUrl,
                      "/",
                      argument$categoryUrl,
                      "/",
                      id,
                      "/report/",
                      generation$sourceKey,
                      "?mode=",
                      mode)
        response <- .GET(url)
        if (response$status_code == "200") {
          response <- httr::content(response)
          if (is.null(response$summary$percentMatched)) {
          response$summary$percentMatched <- 0
          }
          summaryMode[[mode + 1]] <- response$summary %>% tidyr::as_tibble() %>% dplyr::mutate(mode = mode,
                                                                                               modeLong = modeLong) %>% dplyr::mutate(percentMatched = as.numeric(sub("%",
                                                                                                                                                                      "",
                                                                                                                                                                      .data$percentMatched))/100) %>%
          utils::type.convert(as.is = TRUE, dec = ".")

          if (length(response$inclusionRuleStats) > 0) {
          inclusionRuleStat <- response$inclusionRuleStats %>% purrr::map(function(x) {
            purrr::map(x, function(y) {
            ifelse(is.null(y), NA, y)
            })
          })
          inclusionRuleStatsMode[[mode + 1]] <- tidyr::tibble(inclusionRuleStat = inclusionRuleStat) %>%
            tidyr::unnest_wider(.data$inclusionRuleStat)
          } else {
          inclusionRuleStatsMode[[mode + 1]] <- tidyr::tibble()
          }

          if (nrow(inclusionRuleStatsMode[[mode + 1]]) > 0) {
          inclusionRuleStatsMode[[mode + 1]] <- inclusionRuleStatsMode[[mode + 1]] %>% dplyr::mutate(percentExcluded = as.numeric(sub("%",
                                                                                                                                      "",
                                                                                                                                      .data$percentExcluded))/100,
                                                                                                     percentSatisfying = as.numeric(sub("%",
                                                                                                                                        "",
                                                                                                                                        .data$percentSatisfying))/100) %>%
            utils::type.convert(as.is = TRUE,
                                dec = ".") %>% dplyr::mutate(mode = mode, modeLong = modeLong)
          }


          tMapData <- jsonlite::fromJSON(response$treemapData,
                                         simplifyDataFrame = FALSE,
                                         digits = 23)
          treeMapResult <- list(name = c(), size = c())
          treeMapResult <- .flattenTree(node = tMapData, accumulated = treeMapResult)
          if (is.null(treeMapResult$name) | is.null(treeMapResult$size)) {
          treemapDataMode[[mode + 1]] <- tidyr::tibble()
          } else {
          treemapDataMode[[mode + 1]] <- dplyr::tibble(bits = treeMapResult$name,
                                                       size = treeMapResult$size) %>%
            dplyr::mutate(SatisfiedNumber = stringr::str_count(string = .data$bits, pattern = "1"),
                          mode = mode,
                          modeLong = modeLong)
          }

        } else {
          inclusionRuleStats[[mode + 1]] <- tidyr::tibble()
          treemapData[[mode + 1]] <- tidyr::tibble()
          summary[[mode + 1]] <- tidyr::tibble()
        }
        inclusionRuleStats[[i]] <- dplyr::bind_rows(inclusionRuleStatsMode) %>% dplyr::mutate(sourceKey = generation$sourceKey,
                                                                                              sourceName = generation$sourceName)
        treemapData[[i]] <- dplyr::bind_rows(treemapDataMode) %>% dplyr::mutate(sourceKey = generation$sourceKey,
                                                                                sourceName = generation$sourceName)
        summary[[i]] <- dplyr::bind_rows(summaryMode) %>% dplyr::mutate(sourceKey = generation$sourceKey,
                                                                        sourceName = generation$sourceName)
      }
    }
    inclusionRuleStats <- dplyr::bind_rows(inclusionRuleStats)
    treemapData <- dplyr::bind_rows(treemapData)
    summary <- dplyr::bind_rows(summary)

    response <- list(inclusionRuleStats = inclusionRuleStats,
                     treemapData = treemapData,
                     summary = summary)
  }



  # get results for characterization generation
  if (category == "characterization" & nrow(generatedSuccess) > 0) {
    responseAll <- list()
    for (i in (1:nrow(generatedSuccess))) {
      generation <- generatedSuccess %>% dplyr::slice(i)
      writeLines(paste0("   Retrieving results for ",
                        argument$categoryFirstUpper,
                        " id:",
                        generation$id))
      url <- paste0(baseUrl, "/", argument$categoryUrl, "/generation/", generation$id, "/result/")
      response <- .GET(url)
      if (response$status_code == "200") {
        response <- httr::content(response)
        response <- response %>% tidyr::tibble(response = response) %>% tidyr::unnest_wider(.data$response) %>%
          utils::type.convert(as.is = TRUE,
                              dec = ".") %>% .addSourceNameToSourceKey(baseUrl = baseUrl) %>%
          dplyr::mutate(generationId = generation$id)
        response$. <- NULL
      } else {
        response <- tidyr::tibble()
      }
      responseAll[[i]] <- response
    }
    response <- dplyr::bind_rows(responseAll)
  }



  # get results for incidence rate generation
  if (category == "incidenceRate" & nrow(generatedSuccess) > 0) {
    stratifyStats <- list()
    treemapData <- list()
    summary <- list()
    generation <- tidyr::crossing(generatedSuccess %>% dplyr::select(.data$sourceKey),
                                  generationInformation$summaryList %>%
      dplyr::select(.data$targetId, .data$outcomeId)) %>% dplyr::distinct()
    for (i in (1:nrow(generation))) {
      generationLoop <- generation %>% dplyr::slice(i)
      url <- paste0(baseUrl,
                    "/",
                    argument$categoryUrl,
                    "/",
                    id,
                    "/report/",
                    generationLoop$sourceKey,
                    "?targetId=",
                    generationLoop$targetId,
                    "&outcomeId=",
                    generationLoop$outcomeId)
      response <- .GET(url)
      if (response$status_code == "200") {
        response <- httr::content(response)
        summary[[i]] <- response$summary %>% tidyr::as_tibble() %>% utils::type.convert(as.is = TRUE,
                                                                                        dec = ".") %>% dplyr::mutate(sourceKey = generationLoop$sourceKey, targetId = generationLoop$targetId, outcomeId = generationLoop$outcomeId, incidenceRateId = id)

        if (length(response$stratifyStats) > 0) {
          stratifyStat <- response$stratifyStats %>% purrr::map(function(x) {
          purrr::map(x, function(y) {
            ifelse(is.null(y), NA, y)
          })
          })
          stratifyStats[[i]] <- tidyr::tibble(stratifyStat = stratifyStat) %>% tidyr::unnest_wider(.data$stratifyStat) %>%
          utils::type.convert(as.is = TRUE,
                              dec = ".") %>% dplyr::mutate(sourceKey = generationLoop$sourceKey, targetId = generationLoop$targetId, outcomeId = generationLoop$outcomeId, incidenceRateId = id)
        }

        if (length(response$treemapData) > 0) {
          tMapData <- jsonlite::fromJSON(response$treemapData,
                                         simplifyDataFrame = FALSE,
                                         digits = 23)
          treeMapResult <- list(name = c(), size = c())
          treeMapResult <- .flattenTree(node = tMapData,
                                        accumulated = treeMapResult) %>% tidyr::replace_na()
          treemapData[[i]] <- dplyr::tibble(name = treeMapResult$name,
                                            size = treeMapResult$size) %>%
          dplyr::mutate(sourceKey = generationLoop$sourceKey,
                        targetId = generationLoop$targetId,
                        outcomeId = generationLoop$outcomeId,
                        incidenceRateId = id)
        }
      } else {
        stratifyStats[[i]] <- tidyr::tibble()
        treemapData[[i]] <- tidyr::tibble()
        summary[[i]] <- tidyr::tibble()
      }
    }
    stratifyStats <- dplyr::bind_rows(stratifyStats)
    treemapData <- dplyr::bind_rows(treemapData)
    summary <- dplyr::bind_rows(summary)
    response <- list(summary = summary, stratifyStats = stratifyStats, treemapData = treemapData)
  }


  # get results for pathway generation
  if (category == "pathway" & nrow(generatedSuccess) > 0) {
    eventCodes <- list()
    pathwayGroups <- list()
    for (i in (1:nrow(generatedSuccess))) {
      generation <- generatedSuccess %>% dplyr::slice(i)
      writeLines(paste0("   Retrieving results for ",
                        argument$categoryFirstUpper,
                        " generation id:",
                        generation$id))
      url <- paste0(baseUrl, "/", argument$categoryUrl, "/generation/", generation$id, "/result/")
      response <- .GET(url)
      if (response$status_code == "200") {
        response <- httr::content(response)
        eventCodesLoop <- response$eventCodes %>% purrr::map(function(x) {
          purrr::map(x, function(y) {
          ifelse(is.null(y), NA, y)
          })
        })
        eventCodes[[i]] <- eventCodesLoop %>% tidyr::tibble(eventCodesLoop = eventCodesLoop) %>%
          tidyr::unnest_wider(.data$eventCodesLoop) %>% utils::type.convert(as.is = TRUE,
                                                                            dec = ".") %>%
          dplyr::mutate(generationId = generation$id) %>% dplyr::select(-".") %>% dplyr::mutate(sourceKey = generation$sourceKey, sourceName = generation$sourceName, pathwayId = id)

        pathwayGroupsLoop <- response$pathwayGroups
        pathwayGroups[[i]] <- pathwayGroupsLoop %>% tidyr::tibble(pathwayGroupsLoop = pathwayGroupsLoop) %>%
          tidyr::unnest_wider(.data$pathwayGroupsLoop) %>% dplyr::mutate(generationId = generation$id) %>%
          tidyr::unnest_longer(.data$pathways) %>% dplyr::select(-".") %>% dplyr::mutate(.id = dplyr::row_number()) %>%
          tidyr::unnest_longer(.data$pathways) %>% dplyr::mutate(pathways = paste0(.data$pathways)) %>%
          tidyr::pivot_wider(names_from = .data$pathways_id, values_from = .data$pathways) %>%
          utils::type.convert(as.is = TRUE,
                              dec = ".") %>% dplyr::select(-.data$.id) %>% dplyr::mutate(sourceKey = generation$sourceKey, sourceName = generation$sourceName, pathwayId = id)
      } else {
        pathwayGroups[[i]] <- tidyr::tibble()
        eventCodes[[i]] <- tidyr::tibble()
      }
    }
    eventCodes <- dplyr::bind_rows(eventCodes)
    pathwayGroups <- dplyr::bind_rows(pathwayGroups)
    response <- list(eventCodes = eventCodes, pathwayGroups = pathwayGroups)
  }

  return(response)
}
