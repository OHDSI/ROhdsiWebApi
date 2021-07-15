library(httr)


with_mock_dir("mocks", {
  baseUrl <- "http://atlas-dev.ohdsi.org/WebAPI";
  ROhdsiWebApi::insertCohortDefinitionInPackage(cohortId = 2, 
                                                baseUrl = baseUrl,
                                                name="TestCohort",
                                                jsonFolder = paste0(tempdir(), "/mocks"),
                                                sqlFolder = paste0(tempdir(), "/mocks"));
})
