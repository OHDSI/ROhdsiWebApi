# Extracted from test-httrWrappers.R:95

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ROhdsiWebApi", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
withr::defer(Sys.unsetenv("WEBAPI_TOKEN"), envir = parent.env(environment()))

# test -------------------------------------------------------------------------
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
