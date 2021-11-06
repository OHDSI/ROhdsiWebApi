# Tests for the Generation.R file

# Generatable categories are: cohort, incidenceRate, pathway, and characterization

with_mock_dir("mocks/Generation", {

  test_that("Cohort generation functions", {
    df <- invokeCohortGeneration(idCohort, baseUrl, sourceKeyVariable)
    expect_s3_class(df, "data.frame")

    # df2 <- getGenerationInformation(idCohort, "cohort", baseUrl) #TODO argument order should be consistent.
    df2 <- getCohortGenerationInformation(idCohort, baseUrl)
    expect_s3_class(df2, "data.frame")

    # TODO Why does cancel generation produce a warning?
    # expect_warning(cancelGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort"), "requested to be stopped")
    expect_warning(cancelCohortGeneration(idCohort, baseUrl, sourceKeyVariable), "requested to be stopped")

    # check generation information again after cancellation
    df3 <- getGenerationInformation(idCohort, "cohort", baseUrl)
    expect_s3_class(df3, "data.frame")
  })
  
  test_that("incidenceRate generation functions", {
    df <- invokeIncidenceRateGeneration(idIncidenceRate, baseUrl, sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
    
    df2 <- getIncidenceRateGenerationInformation(idIncidenceRate, baseUrl) 
    expect_s3_class(df2$executionInfo, "data.frame")
    
    expect_warning(cancelIncidenceRateGeneration(idIncidenceRate, baseUrl, sourceKeyVariable), "requested to be stopped")
    
    df3 <- getGenerationInformation(idIncidenceRate, "incidenceRate", baseUrl)
    expect_s3_class(df3$executionInfo, "data.frame")
  })  
  
  # TODO get pathway generation functions working.
  # test_that("pathway generation functions", {
    # df <- invokePathwayGeneration(idPathway, baseUrl, sourceKeyVariable)
    # expect_s3_class(df, "data.frame")
    
    # df2 <- getPathwayGenerationInformation(idPathway, baseUrl) 
    # expect_s3_class(df2$executionInfo, "data.frame")
    
    # expect_warning(cancelGeneration(idPathway, baseUrl, sourceKeyVariable), "requested to be stopped")
    
  # })
  
  test_that("characterization generation functions", {
    df <- invokeCharacterizationGeneration(idCharacterization, baseUrl, sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    
    df2 <- getCharacterizationGenerationInformation(idCharacterization, baseUrl)
    expect_s3_class(df2, "data.frame")
    
    expect_warning(cancelCharacterizationGeneration(idCharacterization, baseUrl, sourceKeyVariable), "requested to be stopped")
    
    df3 <- getGenerationInformation(idCharacterization, "characterization", baseUrl)
    expect_s3_class(df3, "data.frame")
  })
  

  
  ## Negative tests ------------------------------------------------
  test_that("Test cancelGeneration pathway (expect error)", {
    id <- 999999 #fake id
    category <- "pathway"
    expect_error(
      ROhdsiWebApi::cancelGeneration(id = id, baseUrl = baseUrl,
                                     sourceKey = sourceKeyVariable,
                                     category = category)
    )
  })
  
  
  #fix this to test the cancel cohort generation call
  #returns a warning instead of an error
  testthat::test_that("Test cancelGeneration cohort (expect error)", {
    id <- 999999
    category <- "cohort"
    wrn <- testthat::expect_warning(
      ROhdsiWebApi::cancelGeneration(id = id, baseUrl = baseUrl,
                                     sourceKey = sourceKeyVariable,
                                     category = category)
    )

    expect_true(grepl(id, wrn))
  })
  
  
  test_that("invokeGeneration works for cohorts", {
    # Make sure that cohort #2 exists
    cohorts <- getCohortDefinitionsMetaData(baseUrl)
    expect_equal(nrow(dplyr::filter(cohorts, .data$id == idCohort)), 1)
    msg <- testthat::capture_output({
      generationInfo <- invokeGeneration(id = idCohort,
                                         baseUrl = baseUrl,
                                         sourceKey = "SYNPUF5PCT",
                                         category = "cohort")
    })

    expect_true(grepl("invoked", msg))
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


})

