# Tests for StudyPackage.R 

with_mock_dir("mocks/StudyPackage", {

  test_that("Test insertCohortDefinitionInPackage", {
    
    scratch <- tempdir()
    jsonFolder <- file.path(scratch, "inst/cohorts")
    sqlFolder <- file.path(scratch, "inst/sql/sql_server")
    
    expect_output(
      insertCohortDefinitionInPackage(cohortId = idCohort, 
                                      baseUrl = baseUrl, 
                                      jsonFolder = jsonFolder,
                                      sqlFolder = sqlFolder), 
      "Created JSON file:"
    )

    jsonFile <- readr::read_file(list.files(jsonFolder, full.names = TRUE)[[1]])
    expect_true(nchar(jsonFile) > 4) 
    
    sqlFile <- readr::read_file(list.files(sqlFolder, full.names = TRUE)[[1]])
    expect_true(nchar(jsonFile) > 4) 
    
    unlink(scratch, recursive = TRUE, force = TRUE)
  })

  test_that("test insertCohortDefinitionSetInPackage", {
    
    # create the temp package structure
    scratch <- tempdir()
    dir.create(file.path(scratch, "inst", "settings"), recursive = TRUE)
    jsonFolder <- file.path(scratch, "inst", "cohorts")
    dir.create(jsonFolder, recursive = TRUE)
    sqlFolder <- file.path(scratch, "inst", "sql", "sql_server")
    dir.create(sqlFolder, recursive = TRUE)
    
    # insert CohortsToCreate.csv file
    df <- data.frame(atlasId = idCohort, cohortId = idCohort, name = "blah")
    settingsPath <- file.path(scratch, "inst", "settings", "CohortsToCreate.csv")
    write.csv(df, settingsPath, row.names = FALSE)

    expect_output(
      insertCohortDefinitionSetInPackage(fileName = settingsPath,
                                         baseUrl = baseUrl,
                                         jsonFolder = jsonFolder,
                                         sqlFolder = sqlFolder,
                                         insertCohortCreationR = FALSE,
                                         packageName = "fakePackage"),
      "Inserting cohort")
    
    jsonFile <- readr::read_file(list.files(jsonFolder, full.names = TRUE)[[1]])
    expect_true(nchar(jsonFile) > 4) 
    
    sqlFile <- readr::read_file(list.files(sqlFolder, full.names = TRUE)[[1]])
    expect_true(nchar(jsonFile) > 4) 
    
    unlink(scratch, recursive = TRUE, force = TRUE)
  })

})
