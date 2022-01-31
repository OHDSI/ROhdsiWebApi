# Tests for the Concept.R file
with_mock_dir("mocks/Concept", {
  
  test_that("getConcepts works", {
    df <- getConcepts(idConceptSet, baseUrl, vocabularySourceKey = sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
  })
  
  test_that("getConcepts with no vocabulary source key", {
    df <- getConcepts(1, baseUrl = baseUrl)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
  })
  
  test_that("getConcepts all upper names", {
    df <- getConcepts(1, baseUrl)
    expect_true(all(grepl("[[:upper:]]", strsplit(names(df), "_")[[1]])))
  })
  
  
  test_that("getSourceConcepts works", {
    df <- getSourceConcepts(idConceptSet, baseUrl, vocabularySourceKey = sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
  })
  
  
  test_that("getSourceConcepts with no vocabulary source key", {
    df <- getSourceConcepts(1, baseUrl = baseUrl)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
  })
  
  test_that("getConcepts all upper names", {
    df <- getConcepts(1, baseUrl)
    expect_true(all(grepl("[[:upper:]]", strsplit(names(df), "_")[[1]])))
  })
  

  
})




  