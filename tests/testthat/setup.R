authType <- getOption("ohdsiWebApiAuthType")
if (authType == "windows") {
  testBaseUrl <- Sys.getenv("TEST_OHDSI_WINDOWS_WEB_API_URL")
  testOhdsiUser <- Sys.getenv("TEST_OHDSI_WINDOWS_WEB_API_USER")
  testOhdsiPassword <- Sys.getenv("TEST_OHDSI_WINDOWS_WEB_API_PASSWORD")
}

if (authType == "ad") {
  testBaseUrl <- Sys.getenv("TEST_OHDSI_AD_WEB_API_URL")
  testOhdsiUser <- Sys.getenv("TEST_OHDSI_AD_WEB_API_USER")
  testOhdsiPassword <- Sys.getenv("TEST_OHDSI_AD_WEB_API_PASSWORD")
}

if (authType == "db") {
  testBaseUrl <- Sys.getenv("TEST_OHDSI_DB_WEB_API_URL")
  testOhdsiUser <- Sys.getenv("TEST_OHDSI_DB_WEB_API_USER")
  testOhdsiPassword <- Sys.getenv("TEST_OHDSI_DB_WEB_API_PASSWORD")
}

if (is.null(testBaseUrl) | is.null(testOhdsiUser) | is.null(testOhdsiPassword)) {
  authSet <- FALSE
} else {
  authSet <- TRUE
}
