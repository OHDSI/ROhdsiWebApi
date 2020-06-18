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
#'  \item{"db"}{Database authentication using Atlas/WebAPI built in auth}
#'  \item{"AD"}{Active Directory}
#'  \item{"windows"}{Windows}
#'  \item{"none"}{No authentication. WebAPI security must be disabled.}
#' }
#' By default the correct authentication method will be guessed.
#' @param keyringServiceName (Optional) Keyring stores passwords securely. 
#'                           For some authentication methods it may be convenient 
#'                           to pass keyring service name instead of entering a password.
#'
#' @return A WebApi connection object
#' @export
connectWebApi <- function(baseUrl, 
                          authMethod = guessAuthMethod(),
                          atlasUsername = NULL,
                          keyringServiceName = NULL) {
  .checkBaseUrl(baseUrl)
  
  # run appropriate auth
  if (authMethod == "db") {
    authHeader <- .authDb(baseUrl, atlasUsername, keyringServiceName)
  } else if(authMethod == "none") {
    authHeader <- ""
  } else if (authMethod == "AD") {
    authHeader <- .authAD(baseUrl, atlasUsername, keyringServiceName)
  } else {
    stop(paste("authMethod must be one of: db, AD, windows, none.
          You passed in", authMethod))
  }
 
  structure(list(baseUrl = baseUrl, 
                 authMethod = authMethod,
                 authHeader = authHeader),
            class = "WebApiConnection")
}

guessAuthMethod <- function() {
  stop("guessAuthMethod is not implemented yet. Is this even possible??")
}

#' Authenticate using Atlas database authentication
#'
#' @param keyringUsername A Keyring service name to be used to pull a password from keyring
#'
#' @return a bearer token to be used in http headers in calls to WebAPI
.authDb <- function(baseUrl, atlasUsername, keyringServiceName) {
  if (!is.character(atlasUsername)) stop("db authentication requires a username. Please supply one.")
  
  if (!is.null(keyringServiceName)) {
    password <- keyring::key_get(keyringServiceName)
  } else {
    password <- getPass::getPass(paste("Please Enter Atlas Password for", atlasUsername))
  }
  
  authUrl <- paste0(baseUrl, "/user/login/db")
  login <- list(login = atlasUsername, password = password)
  r <- httr::POST(authUrl, body = login, encode = "form")
  if (length(httr::headers(r)$bearer) < 1) stop("db authentication failed.")
  bearer <- paste0("Bearer ", httr::headers(r)$bearer)
  bearer
}

.authAD <- function(baseUrl, atlasUsername, keyringServiceName) {
  stop("AD authentication has not been implemented yet")
}
