# Tests for GetResults.R file

with_mock_dir("mocks/GetResults", {

  test_that("getResults work for cohorts", {
    result <- getResults(idCohort, baseUrl, "cohort")
    expect_type(result, "list")
   
  })

})
