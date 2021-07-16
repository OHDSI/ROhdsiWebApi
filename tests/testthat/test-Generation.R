# Tests for the Generation.R file

with_mock_dir("mocks/Generation", {

  # temporarily use the secured version. Unsecured version is backed up.
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME") 
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
  # For the testathon we need to use auth to manually refresh the cache but not when using the cache.
  # authorizeWebApi(baseUrl, "db", testOhdsiUser, testOhdsiPassword)
  
  idCohort <- 3
  idIncidenceRate <- 2
  

  test_that("Generation functions work for cohorts", {
    # df <- invokeGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort")
    df <- invokeCohortGeneration(idCohort, baseUrl, sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)

    # df2 <- getGenerationInformation(idCohort, "cohort", baseUrl) #TODO argument order should be consistent.
    df2 <- getCohortGenerationInformation(idCohort, baseUrl)
    expect_s3_class(df2, "data.frame")
    expect_equal(nrow(df2), 1)

    # Why does cancel generation produce a warning?
    # expect_warning(cancelGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort"), "requested to be stopped")
    expect_warning(cancelCohortGeneration(idCohort, baseUrl, sourceKeyVariable), "requested to be stopped")

    # check generation information again after cancelation
    df3 <- getGenerationInformation(idCohort, "cohort", baseUrl)
    expect_s3_class(df3, "data.frame")
    expect_equal(nrow(df3), 1)
  })
  
  test_that("Generation functions work for incidence rates", {
    # df <- invokeGeneration(idIncidenceRate, baseUrl, sourceKeyVariable, "incidenceRate")
    df <- invokeIncidenceRateGeneration(idIncidenceRate, baseUrl, sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
    
    #TODO argument order should be consistent.
    # df2 <- getGenerationInformation(idIncidenceRate, "incidenceRate", baseUrl) 
    df2 <- getIncidenceRateGenerationInformation(idIncidenceRate, baseUrl) 
    expect_s3_class(df2$executionInfo, "data.frame")
    
    # Why does cancel generation produce a warning?
    expect_warning(cancelIncidenceRateGeneration(idIncidenceRate, baseUrl, sourceKeyVariable), "requested to be stopped")
    
    # check generation information again after cancelation
    df3 <- getGenerationInformation(idIncidenceRate, "incidenceRate", baseUrl)
    expect_s3_class(df3$executionInfo, "data.frame")
  })
  
  #TODO create a similar pattern for testing all characterization generation functions
  test_that("invokeGeneration works for characterizations", {
    # Make sure that characterization #7 exists
    characterizations <- getCharacterizationDefinitionsMetaData(baseUrl)
    expect_equal(nrow(dplyr::filter(characterizations, .data$id == 7)), 1)

    generationInfo <- invokeGeneration(id = 7,
                                       baseUrl = baseUrl,
                                       sourceKey = "SYNPUF5PCT",
                                       category = "characterization")

    expect_s3_class(generationInfo, "data.frame")
    expect_equal(nrow(generationInfo), 1)

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

