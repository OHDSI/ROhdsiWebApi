#' Authorize ROhdsiWebApi to access a protected instance of WebAPI Authorize the ROhdsiWebApi package
#' to access WebApi on behalf of the user. This can be done with any of the auth methods described
#' below. authorizeWebApi will use attempt to retrieve, cache, and update a token which will grant
#' access to webAPI by all subsequent requests made by the package.
#'
#' @template baseUrl
#' @param authMethod       The method used for authentication to WebAPI. Options are
#'                         \itemize{
#'                           \item {"db"}{Database authentication using Atlas/WebAPI built in auth}
#'                           \item {"ad"}{Active Directory}
#'                           \item {"windows"}{Windows NT authentication}
#'                         }
#'                         The auth method must be enabled in the instance of WebAPI pointed to by
#'                         baseUrl.
#'
#' @param webApiUsername   A character string containing the WebApi username passed on to
#'                         authentication methods
#' @param webApiPassword   An character string containing a WebApi password passed on to authentication
#'                         methods. By default the user will be prompted for their password when
#'                         needed.
#'
#' @export
authorizeWebApi <- function(baseUrl, authMethod, webApiUsername = NULL, webApiPassword = NULL) {

  # check input
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(baseUrl, len = 1, min.chars = 1, add = errorMessage)
  checkmate::assertChoice(authMethod, choices = c("db", "ad", "windows"), add = errorMessage)

  # With windows type we can try NT user authentication
  if (authMethod == "windows" & is.null(webApiUsername) & is.null(webApiPassword) & .Platform$OS.type ==
    "windows") {
    webApiUsername <- ":"
    webApiPassword <- ":"
  }

  checkmate::assert(checkmate::checkCharacter(webApiUsername),
                    checkmate::checkNull(webApiUsername),
                    add = errorMessage)
  checkmate::assert(checkmate::checkCharacter(webApiPassword),
                    checkmate::checkNull(webApiPassword),
                    add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  .checkBaseUrl(baseUrl)

  # run appropriate auth. Each auth method must return a header to be added to WebAPI calls.
  authHeader <- switch(authMethod,
                       db = .authDb(baseUrl, webApiUsername, webApiPassword),
                       ad = .authAd(baseUrl, webApiUsername, webApiPassword),
                       windows = .authWindows(baseUrl, webApiUsername, webApiPassword))

  # store token in package environment
  setAuthHeader(baseUrl, authHeader)

  invisible()
}

.authDb <- function(baseUrl, webApiUsername, webApiPassword) {
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/db")
  login <- list(login = webApiUsername, password = webApiPassword)
  r <- httr::POST(authUrl, body = login, encode = "form")
  if (length(httr::headers(r)$bearer) < 1)
    stop("Authentication failed.")
  authHeader <- paste0("Bearer ", httr::headers(r)$bearer)
  authHeader
}

.authAd <- function(baseUrl, webApiUsername, webApiPassword) {
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/ad")
  login <- list(login = webApiUsername, password = webApiPassword)
  r <- httr::POST(authUrl, body = login, encode = "form")
  if (length(httr::headers(r)$bearer) < 1)
    stop("Authentication failed.")
  authHeader <- paste0("Bearer ", httr::headers(r)$bearer)
  authHeader
}

.authWindows <- function(baseUrl, webApiUsername, webApiPassword) {
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/windows")
  r <- httr::GET(authUrl, httr::authenticate(webApiUsername, webApiPassword, type = "ntlm"))
  if (length(httr::headers(r)$bearer) < 1)
    stop("Authentication failed.")
  authHeader <- paste0("Bearer ", httr::headers(r)$bearer)
  authHeader
}

#' Manually set the authorization http header for a WebAPI baseUrl In some cases the user may want to
#' manually set the authorization header. An authHeader is associated with a particular baseUrl and
#' added to to the header of all http requests sent to that url by ROhdsiWebApi.
#'
#' @template baseUrl
#' @param authHeader   A character string containing a Bearer token that will be added to the header of
#'                     all http requests sent to baseUrl. (e.g. "Bearer
#'                     lxd9n2nsdsd2329km23mexjop02m23m23mmmsioxiis0")
#'
#' @export
setAuthHeader <- function(baseUrl, authHeader) {
  checkmate::assertCharacter(baseUrl, min.chars = 1, len = 1)
  checkmate::assertCharacter(authHeader, min.chars = 1, len = 1)
  if (is.null(ROWebApiEnv[[baseUrl]]))
    ROWebApiEnv[[baseUrl]] <- list()
  ROWebApiEnv[[baseUrl]]$authHeader <- authHeader
}
