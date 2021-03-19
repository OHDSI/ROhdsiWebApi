library(dplyr)

test_that("Auth with real URL works", {

  if (is.null(testBaseUrl) | is.null(testOhdsiUser) | is.null(testOhdsiPassword)) {
    skip("Test user settings are not defined")
  }

  authorizeWebApi(testBaseUrl, authType, testOhdsiUser, testOhdsiPassword)

  # Won't know what this token is but we expect there to be one and for it to be long
  expect_true(substr(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader, 1, 6) == "Bearer")
  expect_true(nchar(ROhdsiWebApi:::ROWebApiEnv[[testBaseUrl]]$authHeader) > 50)
})

test_that("get configuration information on WebApi", {
  if (!authSet) {
    skip("Test user settings are not defined")
  }
  cdmSources <- getCdmSources(baseUrl = testBaseUrl)
  expect_is(cdmSources, "data.frame")
  webApiVersion <- getWebApiVersion(baseUrl = testBaseUrl)
  expect_is(webApiVersion, "character")
  priorityVocabulary <- getPriorityVocabularyKey(baseUrl = testBaseUrl)
  expect_is(priorityVocabulary, "character")
  definitionsMetadata <- getDefinitionsMetadata(baseUrl = testBaseUrl, category = "cohort")
  expect_is(definitionsMetadata, "data.frame")

})
