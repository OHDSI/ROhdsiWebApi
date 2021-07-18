# Tests for StudyPackage.R 

with_mock_dir("mocks/StudyPackage", {

  test_that("insertCohortDefinitionInPackage works", {
    
    scratch <- tempdir(check = TRUE)
    jsonFolder <- file.path(scratch, "inst/cohorts")
    sqlFolder <- file.path(scratch, "inst/sql/sql_server")
    
    expect_output(
      insertCohortDefinitionInPackage(cohortId = idCohort, 
                                      baseUrl = baseUrl, 
                                      jsonFolder = jsonFolder,
                                      sqlFolder = sqlFolder), 
      "Created JSON file:"
    )

    jsonFile <- readr::read_file(list.files(jsonFolder, full.names = TRUE)[1])
    expect_true(nchar(jsonFile) > 4) 
    
    sqlFile <- readr::read_file(list.files(sqlFolder, full.names = TRUE)[1])
    expect_true(nchar(jsonFile) > 4) 
    
    unlink(scratch, recursive = TRUE, force = TRUE)
  })

  test_that("insertCohortDefinitionSetInPackage works", {
    # create the temp package structure
    scratch <- tempdir(TRUE)
    # scratch <- here::here("tempPackage")
    dir.create(file.path(scratch, "inst", "settings"), recursive = TRUE)
    jsonFolder <- file.path(scratch, "inst", "cohorts")
    dir.create(jsonFolder, recursive = TRUE)
    sqlFolder <- file.path(scratch, "inst", "sql", "sql_server")
    dir.create(sqlFolder, recursive = TRUE)
    
    # insert CohortsToCreate.csv file
    df <- data.frame(atlasId = idCohort, cohortId = idCohort, name = c("thing1", "thing2"))
    settingsPath <- file.path(scratch, "inst", "settings", "CohortsToCreate.csv")
    write.csv(df, settingsPath, row.names = FALSE)
    
    dir.create(file.path(scratch, "R"))
    rFileName <- file.path(scratch, "R", "CreateCohorts.R")

    expect_output(
        insertCohortDefinitionSetInPackage(fileName = settingsPath,
                                           baseUrl = baseUrl,
                                           jsonFolder = jsonFolder,
                                           sqlFolder = sqlFolder,
                                           rFileName = rFileName,
                                           insertTableSql = FALSE,
                                           insertCohortCreationR = FALSE,
                                           generateStats = TRUE,
                                           packageName = "fakePackage"),
    "Inserting cohort")
    
    # Check that the files were created and are not empty
    jsonFile <- readr::read_file(list.files(jsonFolder, full.names = TRUE)[[1]])
    expect_gt(nchar(jsonFile), 4) 
    
    sqlFile <- readr::read_file(list.files(sqlFolder, full.names = TRUE)[[1]])
    expect_gt(nchar(sqlFile), 4) 
    
    # rFile <- readr::read_file(rFileName)
    # expect_gt(nchar(rFile), 4)
    
    unlink(scratch, recursive = TRUE, force = TRUE)
  })

})
