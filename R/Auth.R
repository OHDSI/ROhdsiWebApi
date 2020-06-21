#' Create a connection to an instance of WebAPI
#' 
#' This function creates a WebAPI connection object that can be used by other
#' functions in this package to access WebAPI. This function will test 
#' the connection as well as trigger authentication and store the auth
#' session token.
#'
#' @template baseUrl 
#' @param authMethod The method used for authentication to WebAPI. Options are
#' \itemize{
#'  \item{"none"}{No authentication. WebAPI security must be disabled.}
#'  \item{"db"}{Database authentication using Atlas/WebAPI built in auth}
#'  \item{"AD"}{Active Directory}
#'  \item{"windows"}{Windows}
#'  \item{oauth}{OAuth}
#'  \item{IAP}{Google Cloud Platform IAP}
#'  
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
                               webApiPassword = NULL) { 
  # check input
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(baseUrl, len = 1, min.chars = 1, add = errorMessage)
  checkmate::assertChoice(authMethod, choices = c("db", "AD", "windows", "none"), add = errorMessage)
  checkmate::assert(checkmate::checkCharacter(webApiUsername), checkmate::checkNull(webApiUsername), add = errorMessage)
  checkmate::assert(checkmate::checkCharacter(webApiPassword), checkmate::checkNull(webPassword), add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  # remove trailing forward slashes in url
  baseUrl <- stringr::str_remove(baseUrl, "/+$")
  .checkBaseUrl(baseUrl)
  
  # run appropriate auth
  authHeader <- switch(authMethod,
                       "none" = "",
                       "db" = .authDb(baseUrl, webApiUsername, webApiPassword),
                       "AD" = .authAD(baseUrl, webApiUsername, webApiPassword),
                       "windows" = .authWindows(baseUrl, webApiUsername, webApiPassword),
                       "oauth" = .authOauth(baseUrl, webApiUsername, webApiPassword),
                       "IAP" = .authOauth(baseUrl, webApiUsername, webApiPassword))
  
  Sys.setenv(WEBAPI_AUTHHEADER = authHeader)
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
  bearer <- paste0("Bearer ", httr::headers(r)$bearer)
  bearer
}
