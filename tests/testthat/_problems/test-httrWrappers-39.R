# Extracted from test-httrWrappers.R:39

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ROhdsiWebApi", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
withr::defer(Sys.unsetenv("WEBAPI_TOKEN"), envir = parent.env(environment()))

# test -------------------------------------------------------------------------
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
