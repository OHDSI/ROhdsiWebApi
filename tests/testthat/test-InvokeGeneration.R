
# baseUrl <- Sys.getenv('WEBAPI_TEST_WEBAPI_URL')

# "cohort",
# "characterization",
# "pathway",
# "incidenceRate"


test_that("invokeGeneration works for cohorts", {
  
  # Make sure that cohort #2 exists
  cohorts <- getCohortDefinitionsMetaData(baseUrl)
  expect_equal(nrow(dplyr::filter(cohorts, .data$id == 2)), 1)
  
  generationInfo <- invokeGeneration(id = 2, 
                                     baseUrl = baseUrl, 
                                     sourceKey = "SYNPUF5PCT", 
                                     category = "cohort")
  
  expect_s3_class(generationInfo, "data.frame")
  expect_equal(nrow(generationInfo), 1)
  
})

test_that("invokeGeneration works for cohorts", {
  
  # Make sure that cohort #2 exists
  characterizations <- getCharacterizationDefinitionsMetaData(baseUrl)
  expect_equal(nrow(dplyr::filter(characterizations, .data$id == 1)), 1)
  
  generationInfo <- invokeGeneration(id = 1, 
                                     baseUrl = baseUrl, 
                                     sourceKey = "SYNPUF5PCT", 
                                     category = "characterization")
  
  expect_s3_class(generationInfo, "data.frame")
  expect_equal(nrow(generationInfo), 1)
  
})



