with_mock_dir("mocks/DefinitionsMetadata", {
  test_that("getDefinitionsMetaData works",{
    df <- getDefinitionsMetadata(baseUrl = baseUrl,
                                 category = "cohort")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 10)
  })
  
  test_that("getDefinitionsMetadata errors", {
    #error for bad url
    misspelledBaseUrl <- gsub("t", "r",baseUrl)
    expect_error(getDefinition(baseUrl = misspelledBaseUrl,
                               category = 'cohort'))
  })

  test_that("getDefinitionsMetadata group2", {
    df <- getDefinitionsMetadata(baseUrl = baseUrl,
                                 category = "characterization")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 2)
  })
  
})
