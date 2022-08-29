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

#' Get user information 
#' 
#' This function returns all public users in the security enabled webapi, and their
#' public permissions. This function only works in security enabled webapi.
#'
#' @template baseUrl 
#'
#' @return A tibble with user ID, user name, and permission information when available.
#' @export
#'
#' @examples
#' \dontrun{
#' getUserInformations(baseUrl = "http://server.org:80/WebAPI")
#' }
getUserInformation <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  if (!securityEnabled(baseUrl)) {
    message("Security is not enabled so there are no permissions to return.")
    return(invisible(NULL))
  }
  r <- .GET(paste0(baseUrl, "/user"))
  
  userInformation <- dplyr::tibble(user = httr::content(r)) %>%
    tidyr::unnest_wider(col = "user")
  
  return(userInformation)
}


#' Get my permissions
#' 
#' This function returns the authorized users permission for
#' definitions like cohort, incidence rate, characterization..
#'
#' @template baseUrl 
#'
#' @return A tibble with user ID, user name, and permission information when available.
#' @export
#'
#' @examples
#' \dontrun{
#' getUserInformations(baseUrl = "http://server.org:80/WebAPI")
#' }
getMyPermissions <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  if (!securityEnabled(baseUrl)) {
    message("Security is not enabled so there are no permissions to return.")
    return(invisible(NULL))
  }
  myPermissions <- .GET(paste0(baseUrl, "/user/me")) %>%
    httr::content()
  
  myPermissions <- myPermissions$permissions
  
  permissions <- c()
  for (i in (1:length(myPermissions))) {
    data <- myPermissions[[i]] %>% purrr::flatten() %>% as.data.frame() %>% dplyr::tibble()
    permissions <- dplyr::bind_rows(permissions,
                                    data)
  }
  
  return(permissions %>% 
           dplyr::arrange(.data$id,
                          .data$permission))
}

#' Is WebAPI using authentication/authorization
#'
#' @template baseUrl 
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' securityEnabled(baseUrl = "http://server.org:80/WebAPI")
#' }
securityEnabled <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  url <- paste0(baseUrl, "/info")
  response <- httr::GET(url)
  httr::content(response)$configuration$security$enabled
}

