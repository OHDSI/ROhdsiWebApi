## Tests for exists%category%Name -------------------

test_that("Test existsPathwayName",{
  definitionsMetaData <- getPathwayDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsPathwayName(pathwayName = nm, 
                                 baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)

})



test_that("Test existsConceptSetName",{
  definitionsMetaData <- getConceptSetDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsConceptSetName(conceptSetName = nm, 
                                 baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})



test_that("Test existsCohortName",{
  definitionsMetaData <- getCohortDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsCohortName(cohortName = nm, 
                                 baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})




test_that("Test existsIncidenceRateName",{
  definitionsMetaData <- getIncidenceRateDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsIncidenceRateName(incidenceRateName = nm, 
                             baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


test_that("Test existsEstimationName",{
  definitionsMetaData <- getEstimationDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsEstimationName(estimationName = nm, 
                                    baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


test_that("Test existsPredictionName",{
  definitionsMetaData <- getPredictionDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsPredictionName(predictionName = nm, 
                                 baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


test_that("Test existsCharacterizationName",{
  definitionsMetaData <- getCharacterizationDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  pathtb <- existsCharacterizationName(characterizationName = nm, 
                                 baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})

## Tests for detect%category%Name -------------------
test_that("Test detectCharacterizationsByName",{
  definitionsMetaData <- getCharacterizationDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  nmPattern <- stringr::word(nm, 1)
  pathtb <- detectCharacterizationsByName(pattern = nmPattern, 
                                         negate = FALSE,
                                         baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


test_that("Test detectCohortsByName",{
  definitionsMetaData <- getCohortDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  nmPattern <- stringr::word(nm, 1)
  pathtb <- detectCohortsByName(pattern = nmPattern, 
                                          negate = FALSE,
                                          baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


test_that("Test detectEstimationsByName",{
  definitionsMetaData <- getEstimationDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  nmPattern <- stringr::word(nm, 1)
  pathtb <- detectEstimationsByName(pattern = nmPattern, 
                                negate = FALSE,
                                baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


test_that("Test detectPredictionsByName",{
  definitionsMetaData <- getPredictionDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  nmPattern <- stringr::word(nm, 1)
  pathtb <- detectPredictionsByName(pattern = nmPattern, 
                                negate = FALSE,
                                baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})

test_that("Test detectPathwaysByName",{
  definitionsMetaData <- getPathwayDefinitionsMetaData(baseUrl = baseUrl)[1,]
  nm <- definitionsMetaData$name
  nmPattern <- stringr::word(nm, 1)
  pathtb <- detectPathwaysByName(pattern = nmPattern, 
                                negate = FALSE,
                                baseUrl = baseUrl)
  expect_equal(pathtb$name, nm)
})


