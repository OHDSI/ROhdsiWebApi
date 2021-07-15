library(withr)
library(httptest)
library(httr)

context("CohortDefinition.R")


with_mock_dir("mocks", {
  test_that("Inserts cohort definition in package without specifying the name", {
    baseUrl <- "http://atlas-dev.ohdsi.org/WebAPI";
    workFolder <- paste0(tempdir(), "/CohortDefinition");
    insertCohortDefinitionInPackage(cohortId=2,
                                    baseUrl=baseUrl, 
                                    jsonFolder = paste0(workFolder, "/json"),
                                    sqlFolder = paste0(workFolder, "/sql"));
    expect_gt(file.info(paste0(workFolder, "/json/type 1 diabetes.json"))$size,0);
    expect_gt(file.info(paste0(workFolder, "/sql/type 1 diabetes.sql"))$size,0);
  });

  test_that("Inserts cohort definition in package with name: TestCohort", {
    baseUrl <- "http://atlas-dev.ohdsi.org/WebAPI";
    workFolder <- paste0(tempdir(), "/CohortDefinition");
    insertCohortDefinitionInPackage(cohortId=2,
                                    name="TestCohort", 
                                    baseUrl=baseUrl, 
                                    jsonFolder = paste0(workFolder, "/json"),
                                    sqlFolder = paste0(workFolder, "/sql"));
    expect_gt(file.info(paste0(workFolder, "/json/TestCohort.json"))$size,0);
    expect_gt(file.info(paste0(workFolder, "/sql/TestCohort.sql"))$size,0);
  });
})

