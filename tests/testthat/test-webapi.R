# Tests for WebApi.R file

with_mock_dir("mocks/WebApi", {
  
  test_that("getPriorityVocabularyKey works", {
    key <- getPriorityVocabularyKey(baseUrl)
    expect_equal(key, sourceKeyVariable)
  })
  
  test_that("getWebApiVersion works", {
    version <- getWebApiVersion(baseUrl)
    expect_type(version, "character")
  })
  
  test_that("getCdmSources works", {
    sources <- getCdmSources(baseUrl)
    expect_s3_class(sources, "data.frame")
  })
  
  test_that("isValidId works for cohorts", {
    isValid <- isValidId(idCohort, baseUrl, "cohort")
    expect_true(isValid)
    
    isValid <- isValidId(1e9, baseUrl, "cohort")
    expect_false(isValid)
  }) 
  
  test_that("isValidSourceKey works for cohorts", {
    isValid <- isValidSourceKey(sourceKeyVariable, baseUrl)
    expect_true(isValid)
    
    isValid <- isValidSourceKey("blah", baseUrl)
    expect_false(isValid)
  })
  
})

