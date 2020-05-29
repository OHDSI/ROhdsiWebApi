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
  tibble(categoryStandard =  c("conceptSet","cohort","incidenceRate",
                               "estimation","prediction","characterization",
                               "pathway")
  ) %>% 
    dplyr::mutate(categoryFirstUpper = paste0(toupper(substr(categoryStandard, 1, 1)), substr(categoryStandard, 2, nchar(categoryStandard)))) %>% 
    dplyr::mutate(categoryAsUsedInWebApi = dplyr::case_when(categoryStandard == 'incidenceRate' ~ 'ir',
                                                            categoryStandard == 'conceptSet' ~ 'conceptset',
                                                            categoryStandard == 'cohort' ~'cohortdefinition',
                                                            categoryStandard == 'characterization' ~ 'cohort-characterization',
                                                            categoryStandard == 'pathway' ~'pathway-analysis',
                                                            TRUE ~ categoryStandard
    )
    ) %>% 
    dplyr::mutate(categoryUrlExtension = dplyr::case_when(categoryStandard == 'characterization' ~ '',
                                                          TRUE ~ categoryStandard
    )
    ) %>%  
    return()
}