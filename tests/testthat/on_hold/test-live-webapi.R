## Old structure for authentication type ------------------
# test_that("Auth with real URL works", {
#   skip_if(!authSet)
#   authorizeWebApi(testBaseUrl, authType, testOhdsiUser, testOhdsiPassword)
# 
#   # Won't know what this token is but we expect there to be one and for it to be long
#   expect_true(substr(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader, 1, 6) == "Bearer")
#   expect_true(nchar(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader) > 50)
# })

## New Structure per authtype ----------------------

test_that("db Auth with real URL works", {
  skip_if(!authSet)
  authorizeWebApi(testBaseUrl, "db", testOhdsiUser, testOhdsiPassword)
  
  # Won't know what this token is but we expect there to be one and for it to be long
  expect_true(substr(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader, 1, 6) == "Bearer")
  expect_true(nchar(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader) > 50)
})

##Commented out until authentcation types are set up

# test_that("ad Auth with real URL works", {
#   skip_if(!authSet)
#   authorizeWebApi(testBaseUrl, "ad", testOhdsiUser, testOhdsiPassword)
#   
#   # Won't know what this token is but we expect there to be one and for it to be long
#   expect_true(substr(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader, 1, 6) == "Bearer")
#   expect_true(nchar(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader) > 50)
# })
# 
# test_that("windows Auth with real URL works", {
#   skip_if(!authSet)
#   authorizeWebApi(testBaseUrl, "windows", testOhdsiUser, testOhdsiPassword)
#   
#   # Won't know what this token is but we expect there to be one and for it to be long
#   expect_true(substr(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader, 1, 6) == "Bearer")
#   expect_true(nchar(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader) > 50)
# })

## Getting configuration info  -------------------------
#no authtype

test_that("get configuration information on WebApi", {
  skip_if(!authSet)
  cdmSources <- getCdmSources(baseUrl = testBaseUrl)
  expect_is(cdmSources, "data.frame")
  webApiVersion <- getWebApiVersion(baseUrl = testBaseUrl)
  expect_is(webApiVersion, "character")
  priorityVocabulary <- getPriorityVocabularyKey(baseUrl = testBaseUrl)
  expect_is(priorityVocabulary, "character")
  definitionsMetadata <- getDefinitionsMetadata(baseUrl = testBaseUrl, category = "cohort")
  expect_is(definitionsMetadata, "data.frame")

})
