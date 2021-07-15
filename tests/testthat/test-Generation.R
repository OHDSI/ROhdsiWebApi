# Tests for the Generation.R file

# with_mock_dir("mocks/Generation", {
#   
#   test_that("Generation functions work for cohorts", {
#     df <- invokeGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort")
#     expect_s3_class(df, "data.frame")
#     expect_equal(nrow(df), 1)
#   
#     df2 <- getGenerationInformation(idCohort, baseUrl, sourceKeyVariable, "cohort")
#     expect_s3_class(df2, "data.frame")
#     expect_equal(nrow(df2), 1)
#   
#     expect_error(cancelGeneration(idCohort, baseUrl, sourceKeyVariable, "cohort"), NULL)
#     
#     # check generation information again after cancelation
#     df3 <- getGenerationInformation(idCohort, baseUrl, sourceKeyVariable, "cohort")
#     expect_s3_class(df3, "data.frame")
#     expect_equal(nrow(df3), 1)
#   })
#   
# })

