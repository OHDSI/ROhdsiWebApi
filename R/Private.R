# @file Private
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


.numericCharacters <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(x)))) & is.character(x)
}

.logicalCharacters <- function(x) {
  !any(is.na(suppressWarnings(as.logical(x)))) & is.character(x)
}

.integerCharacters <- function(x) {
  !any(is.na(suppressWarnings(as.integer(x)))) & is.character(x)
}

.getStandardCategories <- function(){
  dplyr::tibble(categoryStandard =  c("conceptSet","cohort","incidenceRate",
                               "estimation","prediction","characterization",
                               "pathway")
  ) %>% 
    dplyr::mutate(categoryFirstUpper = paste0(toupper(substr(.data$categoryStandard, 1, 1)), substr(.data$categoryStandard, 2, nchar(.data$categoryStandard)))) %>% 
    dplyr::mutate(categoryAsUsedInWebApi = dplyr::case_when(categoryStandard == 'incidenceRate' ~ 'ir',
                                                            categoryStandard == 'conceptSet' ~ 'conceptset',
                                                            categoryStandard == 'cohort' ~'cohortdefinition',
                                                            categoryStandard == 'characterization' ~ 'cohort-characterization',
                                                            categoryStandard == 'pathway' ~'pathway-analysis',
                                                            TRUE ~ categoryStandard
    )
    # ) %>% 
    # dplyr::mutate(categoryUrlExtension = dplyr::case_when(categoryStandard == 'characterization' ~ '',
    #                                                       TRUE ~ categoryStandard
    # )
    ) %>%  
    dplyr::mutate(categoryUrlGeneration = dplyr::case_when(categoryStandard == 'cohort' ~ 'info',
                                                           categoryStandard == 'characterization' ~ 'generation',
                                                           categoryStandard == 'pathway' ~ 'generation',
                                                           categoryStandard == 'incidenceRate' ~ 'info',
                                                           TRUE ~ '')) %>% 
    return()
}


# recursively flattens tree based structure.
.flattenTree <- function(node, accumulated) {
  if (is.null(node$children)) {
    accumulated$name <- c(accumulated$name, node$name);
    accumulated$size <- c(accumulated$size, node$size);
    return(accumulated)
  } else {
    for (child in node$children) {
      accumulated <- .flattenTree(child, accumulated)
    }
    return(accumulated)
  }
}

# converts time in integer/milliseconds to date-time with timezone.
# assumption is that the system timezone = time zone of the local server running WebApi.
.millisecondsToDate <- function(milliseconds) {
  sec <- milliseconds/1000
  return(as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone()))
}