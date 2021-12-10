with_mock_dir("mocks/getConceptSetDefinitionBySourceKey", {
  
  test_that("getConceptSetDefinitionBySourceKey works", {
    conceptSetDefByKey <- ROhdsiWebApi::getConceptSetDefinitionBySourceKey(baseUrl=baseUrl, conceptSetId=2)
    expect_equal(length(conceptSetDefByKey$expression$items), 1)
  })

  test_that("getConceptSetDefinitionBySourceKey works with vocabularySourceKey", {
    conceptSetDefByKey <- ROhdsiWebApi::getConceptSetDefinitionBySourceKey(baseUrl= baseUrl, conceptSetId=2, 
                                                                           vocabularySourceKey = "SYNPUF5PCT")
    expect_equal(length(conceptSetDefByKey$expression$items), 1)
  })
})
