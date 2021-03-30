library(httr)
library(testthat)

# start the api simulation in a background R process
port <- 8000
plumberPath <- system.file("WebApiSimulation", "plumber.R", package = "ROhdsiWebApi", mustWork = T)

serverStart <- function(file, port) {
  pr <- plumber::plumb(file = file)
  pr$run(port = port, swagger = FALSE)
  return(pr)
}
webApiSimulation <- callr::r_bg(serverStart, args = list(file = plumberPath, port = port))

simulationBaseUrl <- paste0("http://localhost:", port)

test_that("Test getWebApiVersion", {
  skip_if_not(webApiSimulation$is_alive())
  expect_equal(getWebApiVersion(simulationBaseUrl), "2.7.6")
})

test_that("Test authorizeWebApi", {
  skip_if_not(webApiSimulation$is_alive())
  
  fakeUrl <- "http://blah.blah"
  setAuthHeader(fakeUrl, "Bearer 0000")
  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[fakeUrl]]$authHeader, "Bearer 0000")
  
  authorizeWebApi(simulationBaseUrl, "db", "testUser", "testPassword")
  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[simulationBaseUrl]]$authHeader, "Bearer 0000")
  
  expect_error(authorizeWebApi(simulationBaseUrl, "db", "testUser", "wrongPassword"), "failed")

  expect_equal(ROhdsiWebApi:::ROWebApiEnv[[simulationBaseUrl]]$authHeader, "Bearer 0000")
})

test_that("Test getCdmSources", {
  skip_if_not(webApiSimulation$is_alive())
  expectedCdmSources <- tibble::tribble(
    ~sourceId, ~sourceName, ~sourceKey, ~sourceDialect, ~cdmDatabaseSchema, ~vocabDatabaseSchema, ~resultsDatabaseSchema,
     1,        "synthea",   "synthea",  "postgresql",   "synthea",          "synthea",            "synthea_results",      
     2,        "synpuf",    "synpuf",   "postgresql",   "synpuf",           "synpuf",             "synpuf_results")
  
  expect_equal(getCdmSources(simulationBaseUrl), expectedCdmSources)
})


test_that("Test getPriorityVocabularyKey", {
  skip_if_not(webApiSimulation$is_alive())
  expect_equal(getPriorityVocabularyKey(simulationBaseUrl), "synthea")
})


test_that("http error codes are handled", {
  skip_if_not(webApiSimulation$is_alive())
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 401))), "401")
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 403))), "403")
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 404))), "404")
  expect_error(stop_for_status(.GET(simulationBaseUrl, path = "echoStatus", query = list(status = 500))), "500")
})
