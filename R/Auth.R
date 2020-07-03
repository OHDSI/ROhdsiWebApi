#' Authenticate user to an instance of WebAPI
#' 
#' Authenticate user using an auth menthod supported by WebAPI and store the
#' auth token in an environment variable which can be used throughout the user's
#' R session to interacte with WebAPI.
#'
#' @template baseUrl 
#' @param authMethod The method used for authentication to WebAPI. Options are
#' \itemize{
#'  \item{"db"}{Database authentication using Atlas/WebAPI built in auth}
#'  \item{"ad"}{Active Directory}
#' }
#' By default the correct authentication method will be guessed.
#' 
#' @template WebApiUsername
#' @template WebApiPassword
#'
#' @return A WebApi connection object
#' @import getPass getPass
#' @export
authenticateWebApi <- function(baseUrl, 
                               authMethod,
                               webApiUsername = NULL,
                               webApiPassword = getPass("WebApi Password")) { #browser()
  # check input
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(baseUrl, len = 1, min.chars = 1, add = errorMessage)
  checkmate::assertChoice(authMethod, choices = c("db", "ad"), add = errorMessage)
  checkmate::assert(checkmate::checkCharacter(webApiUsername), checkmate::checkNull(webApiUsername), add = errorMessage)
  checkmate::assert(authMethod == "none", checkmate::checkCharacter(webApiPassword), checkmate::checkNull(webApiPassword), add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  # remove trailing forward slashes in url
  baseUrl <- stringr::str_remove(baseUrl, "/+$")
  .checkBaseUrl(baseUrl)
  
  # run appropriate auth. Each auth method must return a header to be added to WebAPI calls.
  authHeader <- switch(authMethod,
                       "db" = .authDb(baseUrl, webApiUsername, webApiPassword),
                       "ad" = .authDb(baseUrl, webApiUsername, webApiPassword)
  )
  
  Sys.setenv(WEBAPI_AUTHHEADER = authHeader)
  
  invisible()
}

#' Authenticate using Atlas database authentication
#'
#' @template BaseUrl
#' @template WebApiUsername
#' @template WebApiPassword
#'
#' @return a bearer token to be used in http headers in calls to WebAPI
.authDb <- function(baseUrl, webApiUsername, webApiPassword) {
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)
  
  authUrl <- paste0(baseUrl, "/user/login/db")
  login <- list(login = webApiUsername, password = webApiPassword)
  r <- httr::POST(authUrl, body = login, encode = "form")
  if (length(httr::headers(r)$bearer) < 1) stop("db authentication failed.")
  authHeader <- paste0("Bearer ", httr::headers(r)$bearer)
  authHeader
}
