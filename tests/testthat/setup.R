authType <- getOption("ohdsiWebApiAuthType")
if (authType == "windows") {
  testBaseUrl <- Sys.getenv("TEST_OHDSI_WINDOWS_WEB_API_URL")
  testOhdsiUser <- Sys.getenv("TEST_OHDSI_WINDOWS_WEB_API_USER")
  testOhdsiPassword <- Sys.getenv("TEST_OHDSI_WINDOWS_WEB_API_PASSWORD")
}

if (authType == "ad") {
  testBaseUrl <- Sys.getenv("TEST_OHDSI_AD_WEB_API_URL")
  testOhdsiUser <- Sys.getenv("TEST_OHDSI_AD_WEB_API_USER")
  testOhdsiPassword <- Sys.getenv("TEST_OHDSI_AD_WEB_API_PASSWORD")
}

if (authType == "db") {
  testBaseUrl <- Sys.getenv("TEST_OHDSI_DB_WEB_API_URL")
  testOhdsiUser <- Sys.getenv("TEST_OHDSI_DB_WEB_API_USER")
  testOhdsiPassword <- Sys.getenv("TEST_OHDSI_DB_WEB_API_PASSWORD")
}

if (is.null(testBaseUrl) | is.null(testOhdsiUser) | is.null(testOhdsiPassword)) {
  authSet <- FALSE
} else {
  authSet <- TRUE
}

# start the api simulation in a background R process
plumberPath <- system.file("WebApiSimulation", "plumber.R", package = "ROhdsiWebApi", mustWork = T)
# plumberPath <- pkgload:::shim_system.file("WebApiSimulation", "plumber.R", package = "ROhdsiWebApi", mustWork = T)
port <- 64374
.webserverFunc <- function(file, port) {
  plumber::plumb(file)$run(port = port)
}

webApiSimulation <- callr::r_bg(.webserverFunc, args = list(file = plumberPath, port = port), supervise = TRUE)
simulationBaseUrl <- paste0("http://localhost:", port)

# end the WebApi simulation at teardown, regardless of test status
withr::defer({
  webApiSimulation$kill()
}, testthat::teardown_env())


simulationIsRunning <- function() {
  tryCatch({
    return(webApiSimulation$get_status() == "running")
  }, error = function(...) {})
  return(FALSE)
}
