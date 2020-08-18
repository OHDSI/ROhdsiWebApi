library(httr)
library(testthat)

# start the api simulation in a background R process
webApiSimulation <- callr::r_bg(function(file, port) { plumber::plumb(file)$run(port = port) }, 
                                args = list(file = system.file("inst", "WebApiSimulation", "plumber.R", package = "ROhdsiWebApi"), port = 8000)) 

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
})

# end the WebApi simulation
webApiSimulation$kill()

