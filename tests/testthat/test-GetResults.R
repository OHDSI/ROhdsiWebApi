# Tests for GetResults.R file

with_mock_dir("mocks/GetResults", {

  test_that("getResults work for cohorts", {
    result <- getResults(idCohort, baseUrl, "cohort")
    expect_type(result, "list")
  })
  
  # # TODO fix this test. Seems that function is not working.
  # test_that("getResults work for incidenceRate", {
  #   result <- getResults(idIncidenceRate, baseUrl, "incidenceRate")
  #   expect_type(result, "list")
  # })
  
  # TODO: allow for downloading only the latest successful execution
  test_that("getResults work for pathway", {
    result <- getResults(2, baseUrl, "pathway")
    expect_type(result, "list")
  })
  
  test_that("getResults work for characterization", {
    result <- getResults(2, baseUrl, "characterization")
    expect_type(result, "list")
  })

})
