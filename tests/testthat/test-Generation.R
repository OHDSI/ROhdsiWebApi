# Tests for the Generation.R file

with_mock_dir("mocks/Generation", {

  # temporarily use the secured version. Unsecured version is backed up.
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME") 
  testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
  # authorizeWebApi(baseUrl, "db", testOhdsiUser, testOhdsiPassword)
  
  idCohort <- 3
  idIncidenceRate <- 2
  

  test_that("Generation functions work for cohorts", {
    df <- invokeGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)

    df2 <- getGenerationInformation(idCohort, "cohort", baseUrl) #TODO argument order should be consistent.
    expect_s3_class(df2, "data.frame")
    expect_equal(nrow(df2), 1)

    # Why does cancel generation produce a warning?
    expect_warning(cancelGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort"), "requested to be stopped")

    # check generation information again after cancelation
    df3 <- getGenerationInformation(idCohort, "cohort", baseUrl)
    expect_s3_class(df3, "data.frame")
    expect_equal(nrow(df3), 1)
  })
  
  test_that("Generation functions work for incidence rates", {
    df <- invokeGeneration(idIncidenceRate, baseUrl, sourceKeyVariable, "incidenceRate")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
    
    df2 <- getGenerationInformation(idIncidenceRate, "incidenceRate", baseUrl) #TODO argument order should be consistent.
    expect_s3_class(df2$executionInfo, "data.frame")
    
    # Why does cancel generation produce a warning?
    expect_warning(cancelGeneration(idIncidenceRate, baseUrl, sourceKeyVariable, "incidenceRate"), "requested to be stopped")
    
    # check generation information again after cancelation
    df3 <- getGenerationInformation(idIncidenceRate, "incidenceRate", baseUrl)
    expect_s3_class(df3$executionInfo, "data.frame")
  })

})

