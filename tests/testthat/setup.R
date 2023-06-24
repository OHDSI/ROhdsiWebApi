## Setup file for ROhdsiWebApi tests -----------------
library(httptest)


## set up Environmental variables
baseUrl <- Sys.getenv("WEBAPI_TEST_WEBAPI_URL") #nonsecure test environment
sourceKeyVariable <- 'SYNPUF5PCT' #source db 
# testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") #db test url
# testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME") #db test user
# testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD") # db test password

# preload ids for tests (careful this is fragile) --------------------
# these are the ids for test artifacts in the unsecured webapi instance

# Generatable categories
idCohort <- 2
idIncidenceRate <- 2
idPathway <- 1
idCharacterization <- 2

# Non-generatable categories
idConceptSet <- 2
# estimation
# prediction


