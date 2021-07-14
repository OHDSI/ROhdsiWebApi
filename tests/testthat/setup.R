authType <- getOption("ohdsiWebApiAuthType")
if (authType == "windows") {
  testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
}

if (authType == "ad") {
  testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
}

if (authType == "db") {
  testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
}

authSet <- !(testBaseUrl == "" | testOhdsiUser == "" | testOhdsiPassword == "")
