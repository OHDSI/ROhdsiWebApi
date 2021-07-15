test_that("Test cancelGeneration pathway (expect error)", {
  id <- 999999 #fake id
  category <- "pathway"
  err <- testthat::expect_error(
    ROhdsiWebApi::cancelGeneration(id = id, baseUrl = baseUrl, 
                                   sourceKey = sourceKeyVariable,
                                   category = category)
  )
  
  expect_true(grepl("http error 500", err$message))
})



# testthat::test_that("Test cancelGeneration cohort (expect error)", {
#   id <- 999999
#   category <- "cohort"
#   err <- testthat::expect_error(
#     ROhdsiWebApi::cancelGeneration(id = id, baseUrl = baseUrl, 
#                                    sourceKey = sourceKeyVariable,
#                                    category = category)
#   )
#   
#   expect_true(grepl("http error 500", err$message))
# })
