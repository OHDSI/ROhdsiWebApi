# Tests for the CohortDefinition.R file

with_mock_dir("mocks/CohortDefinition", {
  
  test_that("getCohortSql works", {
    #skip_if(baseUrl == "", message = "test skipped because of blank url.\n baseUrl not properly set as an environment variable for tests")
    def <- getDefinition(id = idCohort, baseUrl = baseUrl, category = 'cohort')
    sql <- getCohortSql(def, baseUrl, generateStats = FALSE)
    expect_type(sql, "character")
    expect_true(nchar(sql) > 1)
  }); 
  
  test_that("getCohortSql error tests", {
    def <- getDefinition(id = idCohort, baseUrl = baseUrl, category = 'cohort')
    
    #expect error from bad url
    expect_error(getCohortSql(def, baseUrl = gsub("a", "z", baseUrl),
                              generateStats = FALSE))
    
    #remove expression from definition
    def2 <- def
    def2$expression <- NULL
    expect_error(getCohortSql(def2, baseUrl, generateStats = FALSE))
    
    #error in cohort sql when using other object type
    def3 <- getDefinition(id = idPathway, 
                          baseUrl = baseUrl, 
                          category = 'pathway')
    expect_error(getCohortSql(def3, baseUrl))
  });
  

})


