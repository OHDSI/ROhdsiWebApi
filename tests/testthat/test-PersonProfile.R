# Tests for PersonProfile.R

with_mock_dir("mocks/PersonProfile", { 

  personId <- 50740
  indexCohortId <- 2
  
  test_that("Test getPersonProfile without indexCohortId", {
    result <- ROhdsiWebApi::getPersonProfile(baseUrl = baseUrl, 
                                             sourceKey = sourceKeyVariable, 
                                             personId = personId, 
                                             indexCohortId = NULL)
    expect_equal(result$cohorts$personId[1], personId)
  })
  
  test_that("Test getPersonProfile with indexCohortId", {
    result <- ROhdsiWebApi::getPersonProfile(baseUrl = baseUrl, 
                                             sourceKey = sourceKeyVariable, 
                                             personId = personId, 
                                             indexCohortId = indexCohortId)
    expect_equal(result$cohorts$personId[1], personId)
    expect_equal(result$person$indexCohortId[1], indexCohortId)
  })

})
