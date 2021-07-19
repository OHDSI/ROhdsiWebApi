# Tests for ConceptSet.R file

with_mock_dir("mocks/ConceptSet", {
  
  test_that("resolveConceptSet works", {
    definition <- getConceptSetDefinition(idConceptSet, baseUrl)
    ids <- resolveConceptSet(definition, baseUrl)
    expect_type(ids, "integer")
  })
  
  test_that("resolveConceptSet error no expressions", {
    definition <- getConceptSetDefinition(idConceptSet, baseUrl)
    definition$expression <- NULL
    expect_error(resolveConceptSet(definition, baseUrl))
  })
  

  test_that("convertConceptSetDefinitionToTable works", {
    definition <- getConceptSetDefinition(idConceptSet, baseUrl)
    df <- convertConceptSetDefinitionToTable(definition)
    expect_s3_class(df, "data.frame")
  })
  
  test_that("resolveConcepSetDefinitionToTabl error no expressions", {
    definition <- getConceptSetDefinition(idConceptSet, baseUrl)
    definition$expression <- NULL
    expect_error(convertConceptSetDefinitionToTable(definition))
  })
  
  test_that("createConceptSetWorkbook works", {
    path <- tempfile(fileext = ".xlsx")
    createConceptSetWorkbook(idConceptSet, path, baseUrl)
    df <- openxlsx::read.xlsx(path, sheet = 1)
    expect_equal(colnames(df), c("conceptSetId", "conceptSetName"))
    expect_equal(nrow(df), 1)
    
    #check sheet 2
    df2 <- openxlsx::read.xlsx(path, sheet = 2)
    expect_equal(ncol(df2), 13)
    unlink(path)
  })
  
  test_that("createConceptSetWorkbook included/mapped", {
    path <- tempfile(fileext = ".xlsx")
    createConceptSetWorkbook(idConceptSet, path, baseUrl,
                             included = TRUE,
                             mapped = TRUE)
    #check included concepts sheet
    df3 <- openxlsx::read.xlsx(path, sheet = 3)
    expect_equal(ncol(df3), 10)
    expect_equal(df3$conceptId, 201254)
    
    #check mapped concepts sheet
    df4 <- openxlsx::read.xlsx(path, sheet = 4)
    expect_equal(ncol(df4), 10)
    expect_equal(nrow(df4), 55)
    
    unlink(path)
  })
  

  
})
