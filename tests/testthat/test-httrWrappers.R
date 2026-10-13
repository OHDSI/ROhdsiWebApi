# Tests for .request() auth header resolution in httrWrappers.R

# Replaces httr::GET and returns the auth headers .request() attached to the call.
captureAuthHeaders <- function(url = "http://example.com/WebAPI/info") {
  headers <- NULL
  testthat::local_mocked_bindings(
    GET = function(url, config, handle, body, encode, ...) {
      headers <<- unlist(lapply(list(...), function(x) x$headers))
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr",
    .env = parent.frame()
  )
  .GET(url)
  headers
}

clearAuthCache <- function() {
  rm(list = ls(ROhdsiWebApi:::ROWebApiEnv), envir = ROhdsiWebApi:::ROWebApiEnv)
}

test_that("WEBAPI_TOKEN env var is sent as X-Api-Key when set", {
  withr::local_envvar(WEBAPI_TOKEN = "testtoken123")
  headers <- captureAuthHeaders()
  expect_equal(headers[["X-Api-Key"]], "testtoken123")
  expect_false("Authorization" %in% names(headers))
})

test_that("WEBAPI_TOKEN takes precedence over cached JWT", {
  setAuthHeader("http://example.com/WebAPI", "Bearer cached-jwt-token")
  withr::defer(clearAuthCache())
  withr::local_envvar(WEBAPI_TOKEN = "envtoken456")
  headers <- captureAuthHeaders()
  expect_equal(headers[["X-Api-Key"]], "envtoken456")
  expect_false("Authorization" %in% names(headers))
})

test_that("Falls back to cached JWT when WEBAPI_TOKEN is not set", {
  setAuthHeader("http://example.com/WebAPI", "Bearer cached-jwt-token")
  withr::defer(clearAuthCache())
  withr::local_envvar(WEBAPI_TOKEN = "")
  headers <- captureAuthHeaders()
  expect_equal(headers[["Authorization"]], "Bearer cached-jwt-token")
  expect_false("X-Api-Key" %in% names(headers))
})

test_that("No auth header sent when WEBAPI_TOKEN unset and no JWT cached", {
  clearAuthCache()
  withr::local_envvar(WEBAPI_TOKEN = "")
  headers <- captureAuthHeaders()
  expect_false(any(c("X-Api-Key", "Authorization") %in% names(headers)))
})

test_that("ROhdsiWebApi.tokenEnvVar option overrides default WEBAPI_TOKEN env var name", {
  withr::local_options(ROhdsiWebApi.tokenEnvVar = "MY_CUSTOM_TOKEN")
  withr::local_envvar(MY_CUSTOM_TOKEN = "customtoken789", WEBAPI_TOKEN = "")
  headers <- captureAuthHeaders()
  expect_equal(headers[["X-Api-Key"]], "customtoken789")
})
