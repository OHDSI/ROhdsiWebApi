
# baseUrl <- Sys.getenv('WEBAPI_TEST_WEBAPI_URL')

# "cohort",
# "characterization",
# "pathway",
# "incidenceRate"


test_that("invokeGeneration works for cohorts", {
  
  # Make sure that cohort #2 exists
  cohorts <- getCohortDefinitionsMetaData(baseUrl)
  expect_equal(nrow(dplyr::filter(cohorts, .data$id == 2)), 1)
  msg <- testthat::capture_output({
    generationInfo <- invokeGeneration(id = 2, 
                                       baseUrl = baseUrl, 
                                       sourceKey = "SYNPUF5PCT", 
                                       category = "cohort")
  })
  
  expect_true(grepl("invoked", msg))
  expect_s3_class(generationInfo, "data.frame")
  expect_equal(nrow(generationInfo), 1)
  
})

test_that("invokeGeneration works for characterizations", {
  
  # Make sure that characterization #1 exists
  characterizations <- getCharacterizationDefinitionsMetaData(baseUrl)
  expect_equal(nrow(dplyr::filter(characterizations, .data$id == 1)), 1)
  
  generationInfo <- invokeGeneration(id = 1, 
                                     baseUrl = baseUrl, 
                                     sourceKey = "SYNPUF5PCT", 
                                     category = "characterization")
  
  expect_s3_class(generationInfo, "data.frame")
  expect_equal(nrow(generationInfo), 1)
  
})

test_that("invokeGeneration handles non-existent cohorts", {
  
  #TODO: make this an informative error like "Cohort 1e9 does not exist in Atlas"
  expect_error(invokeGeneration(id = 1e9, 
                                baseUrl = baseUrl, 
                                sourceKey = "SYNPUF5PCT", 
                                category = "cohort"))
})


