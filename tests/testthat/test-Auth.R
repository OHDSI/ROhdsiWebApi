library(httr)
library(testthat)

# start the api simulation in a background R process
plumberPath <- system.file("WebApiSimulation", "plumber.R", package = "ROhdsiWebApi", mustWork = T)
# plumberPath <- pkgload:::shim_system.file("WebApiSimulation", "plumber.R", package = "ROhdsiWebApi", mustWork = T)

webApiSimulation <- callr::r_bg(function(file, port) { plumber::plumb(file)$run(port = port) }, args = list(file = plumberPath, port = 8000))

# end the WebApi simulation at teardown, regardless of test status
withr::defer({
  webApiSimulation$kill()
}, testthat::teardown_env())

baseUrl <- "http://localhost:8000"

test_that("Test getWebApiVersion", {
  skip_if_not(webApiSimulation$get_status() == "running")
  expect_equal(getWebApiVersion(baseUrl), "2.7.6")
})

test_that("Test authorizeWebApi", {
  skip_if_not(webApiSimulation$get_status() == "running")
  
  fakeUrl <- "http://blah.blah"
  setAuthHeader(fakeUrl, "Bearer 0000")
  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[fakeUrl]]$authHeader, "Bearer 0000")
  
  authorizeWebApi(baseUrl, "db", "testUser", "testPassword")
  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[baseUrl]]$authHeader, "Bearer 0000")
  
  expect_error(authorizeWebApi(baseUrl, "db", "testUser", "wrongPassword"), "failed")

  # Should be supported by github actions
  if (.Platfor$OS == "windows") {
    authorizeWebApi(baseUrl, "windows")
  } else {
    authorizeWebApi(baseUrl, "windows", ":", ":")
  }

  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[baseUrl]]$authHeader, "Bearer 0000")
})

test_that("Test getCdmSources", {
  expectedCdmSources <- tibble::tribble(
    ~sourceId, ~sourceName, ~sourceKey, ~sourceDialect, ~cdmDatabaseSchema, ~vocabDatabaseSchema, ~resultsDatabaseSchema,
     1,        "synthea",   "synthea",  "postgresql",   "synthea",          "synthea",            "synthea_results",      
     2,        "synpuf",    "synpuf",   "postgresql",   "synpuf",           "synpuf",             "synpuf_results")
  
  expect_equal(getCdmSources(baseUrl), expectedCdmSources)
})


test_that("Test getPriorityVocabularyKey", {
  expect_equal(getPriorityVocabularyKey(baseUrl), "synthea")
})


test_that("http error codes are handled", {
  expect_error(stop_for_status(.GET(baseUrl, path = "echoStatus", query = list(status = 401))), "401")
  expect_error(stop_for_status(.GET(baseUrl, path = "echoStatus", query = list(status = 403))), "403")
  expect_error(stop_for_status(.GET(baseUrl, path = "echoStatus", query = list(status = 404))), "404")
  expect_error(stop_for_status(.GET(baseUrl, path = "echoStatus", query = list(status = 500))), "500")
})