# Extracted from test-httrWrappers.R:18

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ROhdsiWebApi", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
withr::defer(Sys.unsetenv("WEBAPI_TOKEN"), envir = parent.env(environment()))

# test -------------------------------------------------------------------------
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
