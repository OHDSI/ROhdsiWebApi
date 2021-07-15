test_that("Test insertCohortDefinitionInPackage", {
  testthat::skip_if(baseUrl == "")
  
  #pull scratch directory
  scratch <- Sys.getenv("scratch_package")
  
  
  #create inst on scratch and new sub directories
  dir.create(file.path(scratch, "inst/cohorts"), recursive = TRUE)
  dir.create(file.path(scratch, "inst/sql/sql_server"), recursive = TRUE)
  jsonFolder <- file.path(scratch, "inst/cohorts")
  sqlFolder <- file.path(scratch, "inst/sql/sql_server")
  
  #run function 
  insertCohortDefinitionInPackage(cohortId = idCohort,
                                        baseUrl = baseUrl,
                                        jsonFolder = jsonFolder,
                                        sqlFolder = sqlFolder)
  

  pathsForTestScenario <- purrr::map(c(jsonFolder, sqlFolder), 
                                     ~list.files(.x, full.names = TRUE))
  contentsOfTestScenario <- purrr::map(pathsForTestScenario, 
                                       ~readr::read_file(.x))
  
  #test that function writes files to directory
  expect_true(nchar(contentsOfTestScenario[[1]]) > 4) # test for json
  expect_true(nchar(contentsOfTestScenario[[2]]) > 4) # test for sql
  
  #exit test and destroy temp environment
  #unlink(scratch, recursive = TRUE, force = TRUE)
})

