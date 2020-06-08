library(testthat)

baseUrl <- Sys.getenv("ohdsiBaseUrl")
testAuthMethod <- Sys.getenv("testAuthMethod")
keyringUsername <- Sys.getenv("keyringUsername")
bearerToken <- NULL

test_that("Test createBearerToken", {
  authMethods <- c("db")
  skip_if(!(testAuthMethod %in% authMethods) | baseUrl == "" | keyringUsername == "")
  bearerToken <<- createBearerToken(baseUrl, testAuthMethod, keyringUsername)
  expect_match(bearerToken, "^Bearer .+$")
})

test_that("Test .checkBaseUrl", {
  skip_if(baseUrl == "")
  value <- ROhdsiWebApi:::.checkBaseUrl(baseUrl = baseUrl)
  expect_true(value)
  expect_error(ROhdsiWebApi:::.checkBaseUrl(baseUrl = "http://black_hole.net"), regexp = "Could not reach WebApi")
})

test_that("Test getCdmSources", {
  skip_if(baseUrl == "")
  cdmSources <- getCdmSources(baseUrl = baseUrl, bearerToken = bearerToken)
  expect_s3_class(cdmSources, "data.frame")
  expect_gt(nrow(cdmSources), 0)
})

test_that("Test getWebApiVersion", {
  skip_if(baseUrl == "")
  version <- getWebApiVersion(baseUrl = baseUrl)
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})

test_that("Test getPriorityVocabularyKey", {
  skip_if(baseUrl == "")
  key <- getPriorityVocabularyKey(baseUrl = baseUrl)
  expect_type(key, "character")
  expect_gt(nchar(key), 0)
})

test_that("Test getConceptSetDefinition", {
  skip_if(baseUrl == "")
  conceptSet <- getConceptSetDefinition(1863526, baseUrl = baseUrl)
  expect_type(conceptSet, "list")
  expect_equal(conceptSet$id, 1863526)
  
  table <- convertConceptSetDefinitionToTable(conceptSet)
  expect_s3_class(table, "data.frame")
  expect_gt(nrow(table), 0)
})

test_that("Test resolveConceptSet", {
  skip_if(baseUrl == "")
  conceptSet <- getConceptSetDefinition(1863526, baseUrl = baseUrl)
  conceptIds <- resolveConceptSet(conceptSet, baseUrl = baseUrl)
  expect_type(conceptIds, "integer")
  expect_gt(length(conceptIds), 0)
})

test_that("Test getConcepts", {
  skip_if(baseUrl == "")
  concepts <- getConcepts(conceptIds = c(8507, 8532), baseUrl = baseUrl)
  expect_true("MALE" %in% concepts$conceptName)
  expect_true("FEMALE" %in% concepts$conceptName)
})

test_that("Test getSourceConcepts", {
  skip_if(baseUrl == "")
  concepts <- getSourceConcepts(conceptIds = c(8507, 8532), baseUrl = baseUrl)
  expect_true("MALE" %in% concepts$conceptName)
  expect_true("FEMALE" %in% concepts$conceptName)
})

test_that("Test createConceptSetWorkbook", {
  skip_if(baseUrl == "")
  file <- tempfile(fileext = ".xlsx")
  createConceptSetWorkbook(conceptSetIds = c(1863526),
                           fileName = file,
                           baseUrl = baseUrl,
                           included = TRUE,
                           mapped = TRUE)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("Test getCohortDefinition", {
  skip_if(baseUrl == "")
  cohort <- getCohortDefinition(1774139, baseUrl = baseUrl)
  expect_type(cohort, "list")
  expect_equal(cohort$id, 1774139)
})

test_that("Test getCohortGenerationInformation", {
  skip_if(baseUrl == "")
  info <- getCohortGenerationInformation(1774139, baseUrl = baseUrl)
  expect_s3_class(info, "data.frame")
  expect_gt(nrow(info), 0)
})

test_that("Test getCohortResults", {
  skip_if(baseUrl == "")
  results <- getCohortResults(1774139, baseUrl = baseUrl, sourceKey = "SYNPUF5PCT")
  expect_type(results, "list")
  expect_gt(length(results), 0)
})

test_that("Test getCohortDefinitionSql", {
  skip_if(baseUrl == "")
  sql <- getCohortDefinitionSql(1774139, baseUrl = baseUrl)
  expect_type(sql, "character")
})

test_that("Test getMetadataForAllSpecifications", {
  skip_if(baseUrl == "")
  metaData <- getMetadataForAllSpecifications( baseUrl = baseUrl)
  expect_s3_class(metaData, "data.frame")
})

# test_that("Test getCohortResults", {
#   skip_if(baseUrl == "")
#   info <- getCohortResults(1774139, baseUrl = baseUrl, sourceKey = "SYNPUF5PCT")
#   expect_s3_class(info, "data.frame")
#   expect_gt(nrow(info), 0)
# })

# TODO: add cohort characterization and incidence rates