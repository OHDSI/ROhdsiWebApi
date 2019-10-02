library(testthat)

baseUrl <- paste0("http://", paste("api", "ohdsi", "org", sep = "."), ":80/WebAPI")

test_that("Test getCdmSources", {
  cdmSources <- getCdmSources(baseUrl = baseUrl)
  expect_gt(length(cdmSources), 0)
})

test_that("Test getWebApiVersion", {
  version <- getWebApiVersion(baseUrl = baseUrl)
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})

