authType <- getOption("ohdsiWebApiAuthType")
# if (authType == "windows") {
#   testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
#   testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
#   testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
# }
# 
# if (authType == "ad") {
#   testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
#   testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
#   testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
# }

if (authType == "db") {
  testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
}

if (authType == "nonsecure") {
  baseUrl <- Sys.getenv("WEBAPI_TEST_WEBAPI_URL")
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
  
  #Setup test ids 
  sourceKeyVariable <- 'SYNPUF5PCT' #source db 
  idCohort <- 2
  idConceptSet <- 2
  idCharacterization <- 1
  idIncidenceRate <- 2
  idPathway <- 1
}

authSet <- !(testBaseUrl == "" | testOhdsiUser == "" | testOhdsiPassword == "")
