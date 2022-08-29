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


#' Obtains users for a specific WebAPI user role. Requires security enabled WebAPI
#' \lifecycle{experimental}
#' @details
#' For a security enabled WebAPI, obtains the user names for a specified Atlas user role. Role Id is
#' set in WebAPI sec_role table
#'
#' @template BaseUrl
#' @param roleId   The role id as defined in WebAPI sec_role table
#' @return
#' A data frame of user information for the WebAPI role.
#'
#' @examples
#' \dontrun{
#' getUsersFromRole(baseUrl = "http://server.org:80/WebAPI", roleId = 10)
#' }
#' @export
getUsersFromRole <- function(baseUrl, roleId) {
  
  .checkBaseUrl(baseUrl)
  url <- sprintf("%s/role/%s/users", baseUrl, roleId)
  request <- .GET(url)
  httr::stop_for_status(request)
  
  json <- .GET(url)
  users <- httr::content(json)
  
  result <- lapply(users, function(u) {
    data.frame(id = u$id, login = u$login, name = u$name)
  })
  return(do.call(rbind, result) %>% 
           dplyr::tibble())
}


#' Assign cohort definition permissions to user. \lifecycle{experimental}
#'
#' @details
#' The function will call the webservice to assign permissions to a cohort definition for the given userId.
#'
#' @template baseUrl
#' @param cohortId    The ID of the cohort definition
#' @param userId      The id to grant permission. You can find out the id from userName 
#'                    as getUserIdFromUserName(baseUrl = baseUrl, userName = "myId")
#' @param permission  The permission type to assign.  Can be "READ" or "WRITE". Defaults to "WRITE".
#'
#' @examples
#' \dontrun{
#' setCohortPermission(baseUrl, cohortId, userId = getUserIdFromUserName(baseUrl = baseUrl, userName = "myId"), permission = "READ")
#' }
#' @export
setCohortPermission <-
  function(baseUrl, cohortId, userId, permission = "READ") {
    .checkBaseUrl(baseUrl)
    if (!securityEnabled(baseUrl)) {
      message("Security is not enabled so permissions cannot be set.")
      return(invisible(NULL))
    }
    
    if (getWebApiVersion(baseUrl) < "2.10") {
      message("setCohortPermission can only be used with WebAPI version 2.10 and later")
      return(invisible(NULL))
    }
    
    if (!permission %in% c("READ", "WRITE")) {
      stop(paste0(
        "Invalid Permission Type:",
        permission,
        ".  Valid types are READ or WRITE."
      ))
    }
    
    url <-
      paste0(baseUrl,
             "/permission/access/COHORT_DEFINITION/",
             cohortId,
             "/role/",
             userId)
    response <-
      .postJson(url, paste0('{"accessType":"', permission, '"}'))
    
    if (response$status_code != 204) {
      stop(paste0("Error: Failed to assign permissions"))
    }
    invisible(response)
  }


#' Get the user id for a given user name. \lifecycle{stable}
#'
#' @details
#' The function will call the WebApi. to assign permissions to a cohort definition for the given userId.
#'
#' @template baseUrl
#' @param userName  A user name that is used to authenticate with WebApi.
#'
#' @examples
#' \dontrun{
#' getUserIdFromUserName(baseUrl, userName)
#' }
#' @export
getUserIdFromUserName <- function(baseUrl, userName) {
  .checkBaseUrl(baseUrl)
  if (!securityEnabled(baseUrl)) {
    message("Security is not enabled so permissions cannot be set.")
    return(invisible(NULL))
  }
  
  userInformation <- getUserInformation(baseUrl = baseUrl) %>%
    dplyr::filter(.data$login %in% c(userName)) %>% 
    dplyr::pull(.data$id)
  
  return(userInformation)
}
