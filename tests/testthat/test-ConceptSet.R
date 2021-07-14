# TODO: Remove this baseUrl or figure out a way to set this for the test run
baseUrl <- Sys.getenv('WEBAPI_TEST_WEBAPI_URL')

test_that("Test resolveConceptSet with conceptSetDefinition missing the expression attribute", {
  conceptSetJson <- jsonlite::read_json("resources/concept-set.json")
  testData <- conceptSetJson
  result <- ROhdsiWebApi::resolveConceptSet(conceptSetDefinition = testData, baseUrl = baseUrl)
  expect_equal(result, c(1503297))
})

test_that("Test resolveConceptSet with malformed conceptSetDefinition", {
  testData <- "{}"
  expect_error(ROhdsiWebApi::resolveConceptSet(conceptSetDefinition = testData, baseUrl = baseUrl))
})

test_that("Test convertConceptSetDefinitionToTable has expression attribute", {
  conceptSetJson <- jsonlite::read_json("resources/concept-set.json")
  testData <- list(expression=conceptSetJson)
  result <- ROhdsiWebApi::convertConceptSetDefinitionToTable(testData)
  expect_equal(result$conceptId[1], 1503297)
})

#TODO: 1) Need to identify the concept sets or create them from scratch
#      2) How do we check the output? For now just checking the file exists
#         and that the file size > 0
test_that("Test createConceptSetWorkbook", {
  testFile <- tempfile("concept_set_workbook.xlsx")
  ROhdsiWebApi::createConceptSetWorkbook(conceptSetIds = c(2),
                                         fileName = testFile,
                                         baseUrl = baseUrl,
                                         included = TRUE,
                                         mapped = TRUE)
  # Verify the file exists and has data
  expect_true(file.exists(testFile))
  expect_gt(file.size(testFile), 0)
  on.exit(unlink(testFile))
})