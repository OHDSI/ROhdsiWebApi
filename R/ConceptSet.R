# @file ConceptSet
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


#' Get a concept set's name from WebAPI
#'
#' @details
#' Obtains the name of a concept set.
#'
#' @param baseUrl      The base URL for the WebApi instance, for example:
#'                     "http://server.org:80/WebAPI".
#' @param setId        The concept set id in Atlas.
#' @param formatName   Should the name be formatted to remove prefixes and underscores?
#'
#' @return
#' The name of the concept set.
#'
#' @export
getConceptSetName <- function(baseUrl, setId, formatName = FALSE) {
  .checkBaseUrl(baseUrl)
  
  url <-
    gsub("@baseUrl",
         baseUrl,
         gsub("@setId", setId, "@baseUrl/conceptset/@setId"))
  json <- httr::GET(url)
  json <- httr::content(json)
  
  if (formatName) {
    .formatName(json$name)
  } else {
    json$name
  }
}


#' Get a concept set expression
#'
#' @details
#' Obtain the JSON expression from WebAPI for a given concept set
#'
#' @param setId         The concept set id in Atlas.
#' @param baseUrl       The base URL for the WebApi instance, for example:
#'                      "http://server.org:80/WebAPI".
#' @param asDataFrame   (OPTIONAL) Get expression as data frame
#'
#' @return
#' A JSON list object representing the concept set
#'
#' @examples
#' \dontrun{
#' # This will obtain a concept set's JSON expression:
#'
#' getConceptSetExpression(setId = 282, baseUrl = "http://server.org:80/WebAPI")
#' }
#'
#' @export
getConceptSetExpression <-
  function(baseUrl, setId, asDataFrame = FALSE) {
    .checkBaseUrl(baseUrl)
    
    url <- sprintf("%1s/conceptset/%2s/expression", baseUrl, setId)
    json <- httr::GET(url)
    json <- httr::content(json)
    
    if (asDataFrame) {
      .setExpressionToDf(json = json)
    } else {
      json
    }
  }

.setExpressionToDf <- function(json) {
  lists <- lapply(json$items, function(j) {
    as.data.frame(j)
  })
  
  do.call("rbind", lists)
}

.getIncludedConceptsDf <-
  function(baseUrl,
           vocabSourceKey,
           includedConcepts) {
    url <-
      sprintf("%s/vocabulary/%s/lookup/identifiers",
              baseUrl,
              vocabSourceKey)
    body <- RJSONIO::toJSON(includedConcepts, digits = 23)
    httpheader <-
      c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
    req <-
      httr::POST(url,
                 body = body,
                 config = httr::add_headers(httpheader))
    req <- httr::content(req)
    
    lists <- lapply(req, function(r) {
      as.data.frame(r)
    })
    
    do.call("rbind", lists)
  }


.getMappedConceptsDf <-
  function(baseUrl,
           vocabSourceKey,
           includedConcepts) {
    url <-
      sprintf("%s/vocabulary/%s/lookup/mapped", baseUrl, vocabSourceKey)
    body <- RJSONIO::toJSON(includedConcepts, digits = 23)
    httpheader <-
      c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
    req <-
      httr::POST(url,
                 body = body,
                 config = httr::add_headers(httpheader))
    req <- httr::content(req)
    
    lists <- lapply(req, function(r) {
      modList <- .convertNulltoNA(r)
      as.data.frame(modList, stringsAsFactors = FALSE)
    })
    
    do.call("rbind", lists)
  }


#' Insert a set of concept sets' concept ids into package
#'
#' @param fileName   Name of a CSV file in the inst/settings folder of the package specifying the
#'                   concept sets to insert. See details for the expected file format.
#' @param baseUrl    The base URL for the WebApi instance, for example: "http://server.org:80/WebAPI".
#'
#' @details
#' The CSV file should have: \describe{ \item{atlasId}{The concept set Id in ATLAS.} }
#'
#' @export
insertConceptSetConceptIdsInPackage <- function(fileName, baseUrl) {
  .checkBaseUrl(baseUrl)
  
  conceptSetsToCreate <-
    read.csv(file.path("inst/settings", fileName))
  if (!file.exists("inst/conceptsets")) {
    dir.create("inst/conceptsets", recursive = TRUE)
  }
  
  for (i in 1:nrow(conceptSetsToCreate)) {
    writeLines(paste("Inserting concept set:", conceptSetsToCreate$atlasId[i]))
    df <- as.data.frame(
      getConceptSetConceptIds(baseUrl = baseUrl,
                              setId = conceptSetsToCreate$atlasId[i])
    )
    names(df) <- c("CONCEPT_ID")
    fileConn <- file(file.path(
      "inst/conceptsets",
      paste(conceptSetsToCreate$atlasId[i], "csv", sep = ".")
    ))
    write.csv(
      x = df,
      file = fileConn,
      row.names = FALSE,
      quote = FALSE
    )
  }
}


#' Get Concepts from a Concept Set Expression
#'
#' @param baseUrl          The base URL for the WebApi instance, for example:
#'                         "http://server.org:80/WebAPI".
#' @param expression       A JSON string that represents the concept set expression
#' @param vocabSourceKey   The source key of the Vocabulary. By default, the priority Vocabulary is
#'                         used.
#' @return
#' A list of concept ids
#'
#' @examples
#' \dontrun{
#' # This will obtain the concept ids from a concept set expression:
#'
#' getSetExpressionConceptIds(baseUrl = "http://server.org:80/WebAPI",
#'                            expression = someJsonExpression)
#' }
#'
#' @export
getSetExpressionConceptIds <-
  function(baseUrl, expression, vocabSourceKey = NULL) {
    .checkBaseUrl(baseUrl)
    
    if (missing(vocabSourceKey) || is.null(vocabSourceKey)) {
      vocabSourceKey <- getPriorityVocabKey(baseUrl = baseUrl)
    }
    
    url <-
      sprintf("%1s/vocabulary/%2s/resolveConceptSetExpression",
              baseUrl,
              vocabSourceKey)
    httpheader <-
      c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
    req <-
      httr::POST(url,
                 body = expression,
                 config = httr::add_headers(httpheader))
    req <- httr::content(req)
    unlist(req)
  }


#' Get Concept Set Concept Ids
#'
#' @details
#' Obtains the full list of concept Ids in a concept set.
#'
#' @param baseUrl          The base URL for the WebApi instance, for example:
#'                         "http://server.org:80/WebAPI".
#' @param setId            The concept set id in Atlas.
#' @param vocabSourceKey   The source key of the Vocabulary. By default, the priority Vocabulary is
#'                         used.
#'
#' @return
#' A list of concept Ids.
#'
#' @export
getConceptSetConceptIds <-
  function(baseUrl, setId, vocabSourceKey = NULL) {
    .checkBaseUrl(baseUrl)
    
    if (missing(vocabSourceKey) || is.null(vocabSourceKey)) {
      vocabSourceKey <- getPriorityVocabKey(baseUrl = baseUrl)
    }
    
    expression <-
      RJSONIO::toJSON(getConceptSetExpression(baseUrl = baseUrl, setId = setId),
                      digits = 23)
    getSetExpressionConceptIds(baseUrl = baseUrl,
                               expression = expression,
                               vocabSourceKey = vocabSourceKey)
  }


#' Save a set of concept sets expressions, included concepts, and mapped concepts into a workbook
#'
#' @param conceptSetIds   A vector of concept set IDs.
#' @param workFolder      Directory location where the workbook will be saved, defaults to working
#'                        directory.
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://server.org:80/WebAPI".
#' @param included        Should included concepts be included in the workbook?
#' @param mapped          Should mapped concepts be included in the workbook?
#'
#' @return
#' A xlsx workbook (conceptSetExpressions.xlsx) that includes a list of all concept set IDs and names
#' and a worksheet for the concepts in each set. Options to include an included concepts and mapped
#' concepts worksheet for each concept set are avaialble.
#'
#' @export
createConceptSetWorkbook <- function(conceptSetIds,
                                     workFolder = NULL,
                                     baseUrl,
                                     included = FALSE,
                                     mapped = FALSE) {
  .checkBaseUrl(baseUrl)
  
  if (is.null(workFolder))
    workFolder <- getwd()
  
  if (!is.vector(conceptSetIds))
    stop("conceptSetIds argument must be a numeric vector")
  
  conceptSetNames <- NULL
  for (i in conceptSetIds) {
    conceptSetNames <- c(
      conceptSetNames,
      getConceptSetName(
        baseUrl = baseUrl,
        setId = i,
        formatName = FALSE
      )
    )
  }
  conceptSets <-
    data.frame(conceptSetId = conceptSetIds, conceptSetName = conceptSetNames)
  names(conceptSets) <-
    SqlRender::camelCaseToTitleCase(names(conceptSets))
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb = wb, sheetName = "conceptSetIds")
  openxlsx::writeDataTable(
    wb = wb,
    sheet = "conceptSetIds",
    x = conceptSets,
    colNames = TRUE,
    rowNames = FALSE,
    withFilter = FALSE
  )
  openxlsx::setColWidths(
    wb = wb,
    sheet = "conceptSetIds",
    cols = 1:ncol(conceptSets),
    widths = "auto"
  )
  
  createSheet <- function(data, label, setId) {
    openxlsx::addWorksheet(wb = wb,
                           sheetName = paste(label, setId, sep = "_"))
    openxlsx::writeDataTable(
      wb = wb,
      sheet = paste(label, setId, sep = "_"),
      x = data,
      colNames = TRUE,
      rowNames = FALSE,
      withFilter = FALSE
    )
    openxlsx::setColWidths(
      wb = wb,
      sheet = paste(label, setId, sep = "_"),
      cols = 1:ncol(data),
      widths = "auto"
    )
  }
  
  for (i in conceptSetIds) {
    resolvedConcepts <-
      resolveConceptSetId(baseUrl = baseUrl, setId = i)
    
    data = resolvedConcepts$expression
    names(data) = SqlRender::camelCaseToTitleCase(names(data))
    createSheet(data = data,
                label = "concepts",
                setId = i)
    
    if (included) {
      data = resolvedConcepts$includedConcepts
      names(data) = SqlRender::camelCaseToTitleCase(names(data))
      createSheet(data = data,
                  label = "included",
                  setId = i)
    }
    
    if (mapped) {
      data = resolvedConcepts$mappedConcepts
      names(data) = SqlRender::camelCaseToTitleCase(names(data))
      createSheet(data = data,
                  label = "mapped",
                  setId = i)
    }
    
  }
  openxlsx::saveWorkbook(
    wb = wb,
    file = file.path(workFolder,
                     "conceptSetExpressions.xlsx"),
    overwrite = TRUE
  )
}



#' Get resolved concept set expression
#'
#' @details
#' Given a concept set JSON expression, return the included and/or mapped concept details
#'
#' @param baseUrl           The base URL for the WebApi instance, for example:
#'                          "http://server.org:80/WebAPI".
#' @param setId             The concept set id in Atlas.
#' @param formatName        Should the name be formatted to remove prefixes and underscores?
#' @return
#' A list of data frame objects with concept set expression, included conceptset, mapped conceptsets.
#'
#' @examples
#' \dontrun{
#' # This will obtain resolved concept set expression:
#'
#' resolveConceptSet(baseUrl = "http://server.org:80/WebAPI", setId = 213423)
#' }
#'
#' @export
resolveConceptSetId <-
  function(baseUrl,
           setId,
           formatName = FALSE) {
    .checkBaseUrl(baseUrl)
    
    .readCsv <- function(fileToRead) {
      concepts <-
        readr::read_csv(
          file = fileToRead,
          guess_max = 1e7,
          col_types = readr::cols()
        )
      names(concepts) <-
        snakecase::to_lower_camel_case(names(concepts))
      return(concepts)
    }
    
    url <- paste(baseUrl, "conceptset", setId, "export", sep = "/")
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    r <- httr::GET(url = url)
    bin <- httr::content(r, "raw")
    
    tmpdir = tempdir()
    dir.create(file.path(tmpdir), showWarnings = FALSE)
    tempFile <-
      tempfile(
        pattern = paste0("conceptSet_", setId),
        fileext = ".zip",
        tmpdir = tmpdir
      )
    base::writeBin(object = bin, con = file.path(tempFile))
    files <-
      utils::unzip(zipfile = file.path(tempFile),
                   overwrite = TRUE,
                   exdir = tmpdir)
    # concept set
    concepts <- .readCsv(fileToRead = files[1])
    included <- .readCsv(fileToRead = files[2])
    mapped <- .readCsv(fileToRead = files[3])
    
    conceptName <-
      getConceptSetName(baseUrl, setId = setId, formatName = formatName)
    # remove zip and unzipped files
    unlink(tempFile)
    unlink(files[1])
    unlink(files[2])
    unlink(files[3])
    
    return(
      list(
        name = conceptName,
        expression = concepts,
        includedConcepts = included,
        mappedConcepts = mapped
      )
    )
  }
