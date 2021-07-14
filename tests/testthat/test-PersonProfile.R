#TODO: Would be better if this was not in each test case file.
baseUrl <- Sys.getenv('WEBAPI_TEST_WEBAPI_URL')
sourceKey <- "SYNPUF5PCT"
personId <- 50740
indexCohortId <- 2

test_that("Test getPersonProfile without indexCohortId", {
  result <- ROhdsiWebApi::getPersonProfile(baseUrl = baseUrl, sourceKey = sourceKey, personId = personId, indexCohortId = NULL)
  expect_equal(result$cohorts$personId[1], personId)
})

test_that("Test getPersonProfile with indexCohortId", {
  result <- ROhdsiWebApi::getPersonProfile(baseUrl = baseUrl, 
                                           sourceKey = sourceKey, 
                                           personId = personId, 
                                           indexCohortId = indexCohortId)
  expect_equal(result$cohorts$personId[1], personId)
  expect_equal(result$person$indexCohortId[1], indexCohortId)
})
