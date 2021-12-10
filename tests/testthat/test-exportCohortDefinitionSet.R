with_mock_dir("mocks/exportCohortDefinitionSet", {
  
  test_that("exportCohortDefinitionSet works", {
    cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = baseUrl,
                                                                   cohortIds = c(2,4,6),
                                                                   generateStats = TRUE)
    expect_s3_class(cohortDefinitionSet, "data.frame")
    expect_equal(nrow(cohortDefinitionSet), 3)
  })
})
