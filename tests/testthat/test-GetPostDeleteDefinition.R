# Tests for the GetPostDeleteDefinition.R file

with_mock_dir("mocks/GetPostDeleteDefinition", {

  test_that("Get Post and Delete functions work for cohorts", {
    
    # Get the test cohort definition
    def <- getDefinition(idCohort, baseUrl, "cohort")
    expect_type(def, "list")
    
    # Post the cohort definition
    response <- postDefinition(baseUrl, "temporary test cohort", "cohort", def)
    expect_s3_class(response, "data.frame")
    expect_equal(nrow(response), 1)
    expect_equal(response$name, "temporary test cohort")
    
    # Delete the cohort
    expect_output(deleteDefinition(response$id, baseUrl, "cohort"), "Success")
    
  })

})

