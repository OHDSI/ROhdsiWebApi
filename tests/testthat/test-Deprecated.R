# Tests for Deprecated.R

with_mock_dir("mocks/Deprecated", { 

  test_that("getCohortDefinitionExpression works and warns", {
    expect_warning(expr <- getCohortDefinitionExpression(idCohort, baseUrl))
    expect_type(expr, "character")
  })
  
  # test_that("getCohortDefinitionName works and warns", {
  # TODO: getCohortDefinitionExpression does not seem to work at all. 
  #   expect_error(nm <- getCohortDefinitionExpression(baseUrl, idCohort))
  # })
  
  test_that("getCohortDefinitionSql works and warns", {
    expect_warning(sql <- getCohortDefinitionSql(idCohort, baseUrl))
    expect_type(sql, "character")
  })
  
  test_that(".formatName works", {
    expect_equal(.formatName("blah_blah"), "blah blah")
  })
  
  test_that("getCohortInclusionRulesAndCounts works and warns", {
    expect_warning(df <- getCohortInclusionRulesAndCounts(baseUrl, idCohort, sourceKeyVariable))
    expect_s3_class(df, "data.frame")
  })

})
