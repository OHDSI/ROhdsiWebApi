authType <- getOption("ohdsiWebApiAuthType")

# Default - unsecured environment
baseUrl <- Sys.getenv("WEBAPI_TEST_WEBAPI_URL")
testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")

# if (authType == "db") {
#   baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
#   testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
#   testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
# }
# if (authType == "windows") {
#   baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
#   testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
#   testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
# }
# 
# if (authType == "ad") {
#   baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL")
#   testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
#   testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
# }

#Setup test ids 
sourceKeyVariable <- 'SYNPUF5PCT' #source db 
idCohort <- 2
idConceptSet <- 2
idCharacterization <- 1
idIncidenceRate <- 2
idPathway <- 1

authSet <- !(baseUrl == "" | testOhdsiUser == "" | testOhdsiPassword == "")
