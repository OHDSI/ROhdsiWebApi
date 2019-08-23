library(testthat)

test_that("Execute WebApi functions", {
  baseUrl <- paste0("http://", paste("api", "ohdsi", "org", sep = "."), ":80/WebAPI")
  
  cdmSources <- getCdmSources(baseUrl = baseUrl)
  expect_true(length(cdmSources) > 0)
})