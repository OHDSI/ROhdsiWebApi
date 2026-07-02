# Tests for .request() auth header resolution in httrWrappers.R

# Ensure any previously set env var doesn't bleed into these tests
withr::defer(Sys.unsetenv("WEBAPI_TOKEN"), envir = parent.env(environment()))

test_that("WEBAPI_TOKEN env var is used as Bearer token when set", {
  withr::with_envvar(
    c(WEBAPI_TOKEN = "testtoken123"),
    with_mock_api({
      req <- tryCatch(
        .GET("http://example.com/WebAPI/info"),
        error = function(e) e
      )
      expect_true(
        any(grepl("Bearer testtoken123",
                  as.character(httptest::last_request()$headers$Authorization),
                  fixed = TRUE))
      )
    })
  )
})

test_that("WEBAPI_TOKEN takes precedence over cached JWT", {
  setAuthHeader("http://example.com/WebAPI", "Bearer cached-jwt-token")
  on.exit(rm(list = ls(ROhdsiWebApi:::ROWebApiEnv),
             envir = ROhdsiWebApi:::ROWebApiEnv))

  withr::with_envvar(
    c(WEBAPI_TOKEN = "envtoken456"),
    with_mock_api({
      tryCatch(
        .GET("http://example.com/WebAPI/info"),
        error = function(e) e
      )
      expect_true(
        any(grepl("Bearer envtoken456",
                  as.character(httptest::last_request()$headers$Authorization),
                  fixed = TRUE))
      )
    })
  )
})

test_that("Falls back to cached JWT when WEBAPI_TOKEN is not set", {
  withr::with_envvar(
    c(WEBAPI_TOKEN = ""),
    {
      setAuthHeader("http://example.com/WebAPI", "Bearer cached-jwt-token")
      on.exit(rm(list = ls(ROhdsiWebApi:::ROWebApiEnv),
                 envir = ROhdsiWebApi:::ROWebApiEnv))

      with_mock_api({
        tryCatch(
          .GET("http://example.com/WebAPI/info"),
          error = function(e) e
        )
        expect_true(
          any(grepl("Bearer cached-jwt-token",
                    as.character(httptest::last_request()$headers$Authorization),
                    fixed = TRUE))
        )
      })
    }
  )
})

test_that("No Authorization header sent when WEBAPI_TOKEN unset and no JWT cached", {
  withr::with_envvar(
    c(WEBAPI_TOKEN = ""),
    with_mock_api({
      tryCatch(
        .GET("http://example.com/WebAPI/info"),
        error = function(e) e
      )
      auth <- httptest::last_request()$headers$Authorization
      expect_true(is.null(auth) || nchar(auth) == 0)
    })
  )
})

test_that("ROhdsiWebApi.tokenEnvVar option overrides default WEBAPI_TOKEN env var name", {
  withr::with_options(
    list(ROhdsiWebApi.tokenEnvVar = "MY_CUSTOM_TOKEN"),
    withr::with_envvar(
      c(MY_CUSTOM_TOKEN = "customtoken789", WEBAPI_TOKEN = ""),
      with_mock_api({
        tryCatch(
          .GET("http://example.com/WebAPI/info"),
          error = function(e) e
        )
        expect_true(
          any(grepl("Bearer customtoken789",
                    as.character(httptest::last_request()$headers$Authorization),
                    fixed = TRUE))
        )
      })
    )
  )
})
