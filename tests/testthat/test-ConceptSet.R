# Tests for ConceptSet.R file

with_mock_dir("mocks/ConceptSet", {
  
  test_that("resolveConceptSet works", {
    definition <- getConceptSetDefinition(idConceptSet, baseUrl)
    ids <- resolveConceptSet(definition, baseUrl)
    expect_type(ids, "integer")
  })

  test_that("convertConceptSetDefinitionToTable works", {
    definition <- getConceptSetDefinition(idConceptSet, baseUrl)
    df <- convertConceptSetDefinitionToTable(definition)
    expect_s3_class(df, "data.frame")
  })
  
  test_that("createConceptSetWorkbook works", {
    path <- tempfile(fileext = ".xlsx")
    createConceptSetWorkbook(idConceptSet, path, baseUrl)
    df <- openxlsx::read.xlsx(path, sheet = 1)
    expect_equal(colnames(df), c("conceptSetId", "conceptSetName"))
    expect_equal(nrow(df), 1)
  })
  
})
