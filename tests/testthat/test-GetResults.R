# Tests for GetResults.R file

with_mock_dir("mocks/GetResults", {

  test_that("getResults work for cohorts", {
    result <- getResults(idCohort, baseUrl, "cohort")
    expect_type(result, "list")
  })
  
  # TODO fix this test. Seems that function is not working.
  # test_that("getResults work for incidenceRate", {
  #   result <- getResults(idIncidenceRate, baseUrl, "incidenceRate")
  #   expect_type(result, "list")
  # })  
  
  # TODO: why do these download so many different ids? Are they different executions?
  test_that("getResults work for pathway", {
    result <- getResults(idPathway, baseUrl, "pathway")
    expect_type(result, "list")
  })
  
  test_that("getResults work for characterization", {
    result <- getResults(idCharacterization, baseUrl, "characterization")
    expect_type(result, "list")
  })

})
