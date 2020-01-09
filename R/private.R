# @file general
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of R OHDSI webApi package.
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

#private function recursively converts variables that are NULL to NA
.convertNullToNARecursive <- function(x) {
  nullToNARecursive <- function(x, fn)
  {
    if (is.list(x)) {
      lapply(x, nullToNARecursive, fn)
    } else if (is.data.frame(x)) {
      x[is.null(x)] <- NA
    } else {
      fn(x)
    }
  }
  nullToNARecursive(x, function(x) if (is.null(x)) NA else x)
}


#compares a reference vector of objects to a reference vector. If different it will return the difference.
.isVectorContained = function(vectorToCompare = c(), vectorReference = c()){
  x = vector(length = length(vectorToCompare))
  for (i in 1:length(vectorToCompare)) {
      x[i] = vectorToCompare[i] %in% vectorReference
      if (length(which(vectorToCompare[i] %in% vectorReference)) == 0) {
        vectorReference } else {
          vectorReference = vectorReference[-match(vectorToCompare[i], vectorReference)]
        }
  }
  if (all(x == T)) {
    TRUE
  } else {
    return(vectorToCompare[!x])
  }
}

# recursively flattens tree based structure.
.flattenTree <- function(node, accumulated) {
  if (is.null(node$children)) {
    accumulated$name <- c(accumulated$name, node$name);
    accumulated$size <- c(accumulated$size, node$size);
    return(accumulated)
  } else {
    for (child in node$children) {
      accumulated <- flattenTree(child, accumulated)
    }
    return(accumulated)
  }
}

# converts time in integer/milliseconds to date-time with timezone.
# assumption is that the system timezone = time zone of the local server running webApi.
.millisecondsToDate <- function(milliseconds) {
  sec <- milliseconds/1000
  as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone())
}

# checks if url conforms with expected structure for base url
.checkBaseUrl <- function(baseUrl){
  require(rex)
  
  valid_chars <- rex(except_some_of(".", "/", " ", "-"))
  
  baseUrlRegEx <- rex(
    start,
    
    # protocol identifier (optional) + //
    group(list("http", maybe("s")), "://"),
    
    # user:pass authentication (optional)
    maybe(non_spaces,
          maybe(":", zero_or_more(non_space)),
          "@"),
    
    #host name
    group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
    
    #domain name
    zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
    
    #TLD identifier
    group(".", valid_chars %>% at_least(2)),
    
    # server port number (required)
    zero_or_more(":", digit %>% between(2, 5)),
    
    # resource path (optional)
    maybe("/", non_space %>% zero_or_more()),
    
    end
  )
  success <- as.logical(grepl(baseUrlRegEx, baseUrl))
  if (!success) {
    stop("Base URL not valid, should be like http://server.org:80/WebAPI")
  }
}

# formats string/name
.formatName <- function(name) {
  gsub("_", " ", gsub("\\[(.*?)\\]_", "", gsub(" ", "_", name)))
}

# get valid source keys
.getValidSourceKeys <- function(baseUrl,sourceKeys){
  .checkBaseUrl(baseUrl)
  require(dplyr)
  .getSourceAndDaimonConfiguration(baseUrl) %$%
    parsed %>%
    dplyr::filter(toupper(sourceKey) %in% toupper(sourceKeys)) %>%
    dplyr::select(sourceKey) %>%
    pull()
}


# get source and daimon
.getSourceAndDaimonConfiguration <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  require(dplyr)
  require(tidyr)
  url <- sprintf("%s/source/sources", baseUrl)
  result <- .getApiResponseParse(url)
  
  for (i in (1:length(result$parsed$daimons))) {
    result$parsed$daimons[[i]]$sourceId <- result$parsed$sourceId[[i]]
  }
  
  daimons <- result$parsed$daimons %>%
    dplyr::bind_rows() %>%
    tidyr::spread(key = daimonType, value = tableQualifier) %>%
    dplyr::select(-sourceDaimonId) %>%
    base::replace(is.na(.), "") %>%
    dplyr::group_by(sourceId) %>%
    dplyr::summarise_all(max) %>%
    dplyr::mutate_all(na_if,"")
  
  result$parsed <- result$parsed %>%
    dplyr::select(-daimons) %>%
    dplyr::left_join(y = daimons, by = "sourceId")
  
  result
}

# Parse API to native (json) and parsed (r-friendly format)
.getApiResponseParse <- function(url){#url <- baseUrl
  .checkBaseUrl(baseUrl)
  require(httr)
  require(jsonlite)
  
  if (http_type(httr::GET(url)) != "application/json") {
    stop(paste0(url, " API for did not return json"), call. = FALSE)
  } else {
    
    native <- httr::content(httr::GET(url), as = 'text', type = "application/json", encoding = 'UTF-8' )
    parsed <- jsonlite::fromJSON(txt = native, simplifyVector = TRUE, simplifyDataFrame = TRUE)
    
    result <- list(
      native = native,
      parsed = parsed
    )
    result
  }
}