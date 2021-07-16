## Setup file for ROhdsiWebApi tests -----------------
library(httptest)

## set up Environmental variables
baseUrl <- Sys.getenv("WEBAPI_TEST_WEBAPI_URL") #nonsecure test environment
sourceKeyVariable <- 'SYNPUF5PCT' #source db 
# testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") #db test url
# testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME") #db test user
# testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD") # db test password

# preload ids for tests (careful this is fragile) --------------------
idCohort <- 2
idConceptSet <- 2
idCharacterization <- 1
idIncidenceRate <- 2
idPathway <- 1


## Set scratch directory to test writes ----------------------
#tear it down after test complete
# removing this because these functions are leaving the package

#set up scratch space for ROhdsiWebApi
# Sys.setenv("scratch_package" = tempdir("scratch_package"))


# withr::defer({
#   #remove temp dir for Capr Save space used for testing
#   unlink(Sys.getenv("scratch_package"), recursive = TRUE, force = TRUE)
# }, testthat::teardown_env())
