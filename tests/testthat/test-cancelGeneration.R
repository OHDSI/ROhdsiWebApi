test_that("Test cancelGeneration pathway (expect error)", {
  id <- 999999 #fake id
  category <- "pathway"
  expect_error(
    ROhdsiWebApi::cancelGeneration(id = id, baseUrl = baseUrl, 
                                   sourceKey = sourceKeyVariable,
                                   category = category)
  )
})


#fix this to test the cancel cohort generation call
#returns a warning instead of an error
testthat::test_that("Test cancelGeneration cohort (expect error)", {
  id <- 999999
  category <- "cohort"
  wrn <- testthat::expect_warning(
    ROhdsiWebApi::cancelGeneration(id = id, baseUrl = baseUrl,
                                   sourceKey = sourceKeyVariable,
                                   category = category)
  )

  expect_true(grepl(id, wrn))
})
