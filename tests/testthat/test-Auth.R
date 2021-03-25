library(httr)
library(testthat)

test_that("Test getWebApiVersion", {
  skip_if_not(simulationIsRunning())
  expect_equal(getWebApiVersion(simulationBaseUrl), "2.7.6")
})

test_that("Test authorizeWebApi", {
  skip_if_not(simulationIsRunning())
  
  fakeUrl <- "http://blah.blah"
  setAuthHeader(fakeUrl, "Bearer 0000")
  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[fakeUrl]]$authHeader, "Bearer 0000")
  
  authorizeWebApi(simulationBaseUrl, "db", "testUser", "testPassword")
  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[simulationBaseUrl]]$authHeader, "Bearer 0000")
  
  expect_error(authorizeWebApi(simulationBaseUrl, "db", "testUser", "wrongPassword"), "failed")

  # Should be supported by github actions
  if (.Platform$OS == "windows") {
    authorizeWebApi(simulationBaseUrl, "windows")
  } else {
    authorizeWebApi(simulationBaseUrl, "windows", ":", ":")
  }

  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[simulationBaseUrl]]$authHeader, "Bearer 0000")
})

test_that("Test getCdmSources", {
  skip_if_not(simulationIsRunning())
  expectedCdmSources <- tibble::tribble(
    ~sourceId, ~sourceName, ~sourceKey, ~sourceDialect, ~cdmDatabaseSchema, ~vocabDatabaseSchema, ~resultsDatabaseSchema,
     1,        "synthea",   "synthea",  "postgresql",   "synthea",          "synthea",            "synthea_results",      
     2,        "synpuf",    "synpuf",   "postgresql",   "synpuf",           "synpuf",             "synpuf_results")
  
  expect_equal(getCdmSources(simulationBaseUrl), expectedCdmSources)
})


test_that("Test getPriorityVocabularyKey", {
  skip_if_not(simulationIsRunning())
  expect_equal(getPriorityVocabularyKey(simulationBaseUrl), "synthea")
})


test_that("http error codes are handled", {
  skip_if_not(simulationIsRunning())
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 401))), "401")
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 403))), "403")
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 404))), "404")
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 500))), "500")
})
