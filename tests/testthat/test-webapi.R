library(testthat)

baseUrl <- Sys.getenv("ohdsiBaseUrl")

test_that("Test getCdmSources", {
  skip_if(baseUrl == "")
  cdmSources <- getCdmSources(baseUrl = baseUrl)
  expect_s3_class(cdmSources, "data.frame")
  expect_gt(nrow(cdmSources), 0)
})

test_that("Test getWebApiVersion", {
  skip_if(baseUrl == "")
  version <- getWebApiVersion(baseUrl = baseUrl)
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})

test_that("Test getPriorityVocabKey", {
  skip_if(baseUrl == "")
  key <- getPriorityVocabKey(baseUrl = baseUrl)
  expect_type(key, "character")
  expect_gt(nchar(key), 0)
})

