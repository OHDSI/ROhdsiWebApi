# Tests for the Concept.R file

with_mock_dir("mocks/Concept", {
  
  test_that("getConcepts works", {
    df <- getConcepts(idConceptSet, baseUrl, vocabularySourceKey = sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
  })
  
  test_that("getSourceConcepts works", {
    df <- getSourceConcepts(idConceptSet, baseUrl, vocabularySourceKey = sourceKeyVariable)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 1)
  })
  
})




  