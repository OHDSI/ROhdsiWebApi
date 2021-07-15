# Tests for the CohortDefinition.R file

with_mock_dir("mocks/CohortDefinition", {
  
  test_that("getCohortSql works", {
    def <- getDefinition(id = idCohort, baseUrl = baseUrl, category = 'cohort')
    sql <- getCohortSql(def, baseUrl, generateStats = FALSE)
    expect_type(sql, "character")
    expect_true(nchar(sql) > 1)
  }); 

})

