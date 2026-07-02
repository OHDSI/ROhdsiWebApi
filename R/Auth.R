#' Authorize access to a WebApi instance
#'
#' @description
#' Certain WebAPI endpoints require user authentication. The user must authorize
#' access WebApi on behalf of the user. This can be done with any of the auth methods described
#' below. authorizeWebApi will use attempt to retrieve, cache, and update a token which will grant
#' access to webAPI by all subsequent requests made by the package.
#'
#' @template BaseUrl
#' @param authMethod       The method used for authentication to WebAPI. Options are
#'                         \itemize{
#'                           \item {"db"}{Database authentication using Atlas/WebAPI built in auth}
#'                           \item {"ad"}{Active Directory}
#'                           \item {"ldap"}{LDAP protocol}
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
  checkmate::assertChoice(authMethod, choices = c("db", "ad", "ldap", "windows"), add = errorMessage)

  # With windows type we can try NT user authentication
  if (authMethod == "windows" && is.null(webApiUsername) && is.null(webApiPassword) && .Platform$OS.type ==
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
                       ldap = .authLdap(baseUrl, webApiUsername, webApiPassword),
                       windows = .authWindows(baseUrl, webApiUsername, webApiPassword))

  # store token in package environment
  setAuthHeader(baseUrl, authHeader)

  invisible()
}

#' Parse LoginService.Result response from WebAPI authentication endpoints
#'
#' @param response An httr response object from an authentication endpoint
#' @return A character string containing the Bearer token header value
#' @keywords internal
.parseLoginResult <- function(response) {
  # Check HTTP status

  if (httr::http_error(response)) {
    # Try to parse error message from response body
    tryCatch({
      content <- httr::content(response, as = "parsed", type = "application/json")
      if (!is.null(content$message)) {
        stop(sprintf("Authentication failed (HTTP %d): %s", 
                     httr::status_code(response), 
                     content$message))
      }
    }, error = function(e) {
      if (grepl("Authentication failed", e$message)) {
        stop(e$message)
      }
    })
    stop(sprintf("Authentication failed with HTTP status %d", httr::status_code(response)))
  }
  
  # Parse JSON response body
  content <- httr::content(response, as = "parsed", type = "application/json")
  
  # Validate JWT is present

  if (is.null(content$jwt) || nchar(content$jwt) == 0) {
    errorMsg <- if (!is.null(content$message)) content$message else "No JWT token in response"
    stop(sprintf("Authentication failed: %s", errorMsg))
  }
  
  # Return Bearer token header value
  paste0("Bearer ", content$jwt)
}

.authDb <- function(baseUrl, webApiUsername, webApiPassword) {
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/db")
  login <- list(login = webApiUsername, password = webApiPassword)
  r <- httr::POST(authUrl, body = login, encode = "form")
  .parseLoginResult(r)
}

.authAd <- function(baseUrl, webApiUsername, webApiPassword) {
  # Note: AD authentication endpoint requires future WebAPI implementation
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/ad")
  login <- list(login = webApiUsername, password = webApiPassword)
  r <- httr::POST(authUrl, body = login, encode = "form")
  .parseLoginResult(r)
}

.authLdap <- function(baseUrl, webApiUsername, webApiPassword) {
  # Note: LDAP authentication endpoint requires future WebAPI implementation
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/ldap")
  login <- list(login = webApiUsername, password = webApiPassword)
  r <- httr::POST(authUrl, body = login, encode = "form")
  .parseLoginResult(r)
}

.authWindows <- function(baseUrl, webApiUsername, webApiPassword) {
  checkmate::assertCharacter(webApiUsername, min.chars = 1, len = 1)
  checkmate::assertCharacter(webApiPassword, min.chars = 1, len = 1)

  authUrl <- paste0(baseUrl, "/user/login/windows")
  r <- httr::GET(authUrl, httr::authenticate(webApiUsername, webApiPassword, type = "ntlm"))
  .parseLoginResult(r)
}

#' Manually set the authorization http header for a WebAPI baseUrl In some cases the user may want to
#' manually set the authorization header. An authHeader is associated with a particular baseUrl and
#' added to to the header of all http requests sent to that url by ROhdsiWebApi.
#'
#' @template BaseUrl
#' @param authHeader   A character string containing a Bearer token that will be added to the header of
#'                     all http requests sent to baseUrl. (e.g. "Bearer
#'                     lxd9n2nsdsd2329km23mexjop02m23m23mmmsioxiis0")
#'
#' @details
#' An alternative to calling this function is to set an environment variable containing a raw API
#' token (without the \code{"Bearer "} prefix). By default ROhdsiWebApi reads the \code{WEBAPI_TOKEN}
#' environment variable, but this name can be overridden by setting the R option
#' \code{ROhdsiWebApi.tokenEnvVar}. For example, to use a variable named
#' \code{MY_INSTANCE_TOKEN}:
#'
#' \preformatted{
#' options(ROhdsiWebApi.tokenEnvVar = "MY_INSTANCE_TOKEN")
#' Sys.setenv(MY_INSTANCE_TOKEN = "eyJ...")
#' }
#'
#' When the env var is set, it takes precedence over any token stored via
#' \code{setAuthHeader()} or \code{authorizeWebApi()} and is applied globally to all WebAPI
#' base URLs. This is useful for service accounts or non-interactive environments where the
#' token is provisioned externally. Store it in \code{.Renviron}
#' (e.g. \code{WEBAPI_TOKEN=eyJ...}) or set it at runtime via \code{Sys.setenv()}.
#'
#' @seealso \code{\link{authorizeWebApi}} for interactive JWT-based authentication.
#'
#' @export
setAuthHeader <- function(baseUrl, authHeader) {
  checkmate::assertCharacter(baseUrl, min.chars = 1, len = 1)
  checkmate::assertCharacter(authHeader, min.chars = 1, len = 1)
  if (is.null(ROWebApiEnv[[baseUrl]]))
    ROWebApiEnv[[baseUrl]] <- list()
  ROWebApiEnv[[baseUrl]]$authHeader <- authHeader
}

#' Create a personal API key in WebAPI
#'
#' Calls \code{POST /user/apikeys} to generate a new long-lived API key for the
#' authenticated user. The \code{rawKey} value is printed to the console and
#' returned invisibly — it is returned by WebAPI \strong{exactly once} and
#' cannot be retrieved again. Store it securely immediately (e.g. in
#' \code{.Renviron} as \code{WEBAPI_TOKEN=<rawKey>}).
#'
#' @template BaseUrl
#' @param name          A short human-readable label for the key (required).
#' @param description   An optional longer description (default \code{NULL}).
#' @param expiresInDays Integer number of days until the key expires. Use
#'   \code{NULL} or \code{0} for a non-expiring key (default \code{NULL}).
#'
#' @return Invisibly returns a list with elements \code{rawKey},
#'   \code{keyIdentifier}, \code{name}, \code{createdAt}, and \code{expiresAt}.
#'
#' @details
#' The caller must already be authenticated — either via
#' \code{\link{authorizeWebApi}}, \code{\link{setAuthHeader}}, or the
#' \code{WEBAPI_TOKEN} environment variable — before calling this function.
#'
#' The key is authenticated using the \code{X-API-KEY} request header on
#' subsequent calls. See \code{\link{setAuthHeader}} for details on the
#' \code{WEBAPI_TOKEN} env var approach.
#'
#' @seealso \code{\link{authorizeWebApi}}, \code{\link{setAuthHeader}}
#'
#' @export
createApiKey <- function(baseUrl, name, description = NULL, expiresInDays = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(baseUrl, len = 1, min.chars = 1, add = errorMessage)
  checkmate::assertCharacter(name, len = 1, min.chars = 1, add = errorMessage)
  checkmate::assert(checkmate::checkCharacter(description, len = 1),
                    checkmate::checkNull(description),
                    add = errorMessage)
  checkmate::assert(checkmate::checkInt(expiresInDays, lower = 0),
                    checkmate::checkNull(expiresInDays),
                    add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  .checkBaseUrl(baseUrl)

  body <- list(name = name)
  if (!is.null(description)) body$description <- description
  if (!is.null(expiresInDays) && expiresInDays > 0) body$expiresInDays <- expiresInDays

  url <- paste0(baseUrl, "/user/apikeys")
  response <- .POST(url, body = body, encode = "json")

  if (httr::status_code(response) != 201) {
    httr::stop_for_status(response)
  }

  result <- httr::content(response, as = "parsed", type = "application/json")

  message("API key created successfully.")
  message("Key name:       ", result$name)
  message("Key identifier: ", result$keyIdentifier)
  message("Created at:     ", format(as.POSIXct(result$createdAt, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC"))
  message("Expires at:     ", if (is.null(result$expiresAt)) "never" else format(as.POSIXct(result$expiresAt, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC"))
  message("")
  message("Raw key (store this now — it will not be shown again):")
  message(result$rawKey)

  invisible(result)
}
