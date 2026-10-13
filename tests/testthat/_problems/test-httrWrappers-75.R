# Extracted from test-httrWrappers.R:75

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ROhdsiWebApi", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
withr::defer(Sys.unsetenv("WEBAPI_TOKEN"), envir = parent.env(environment()))

# test -------------------------------------------------------------------------
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
