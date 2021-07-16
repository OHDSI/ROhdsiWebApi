# Tests for the Auth.R file

# I don't think Auth.R can be tested with mocks

testBaseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
testOhdsiUser <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME") 
testOhdsiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD") 

test_that("Database authorization works", {
  skip_if(testBaseUrl == "")
  setAuthHeader(testBaseUrl, "blah")
  expect_error(getPriorityVocabularyKey(testBaseUrl), "Unauthorized")
  authorizeWebApi(testBaseUrl, "db", testOhdsiUser, testOhdsiPassword)
  expect_type(getPriorityVocabularyKey(testBaseUrl), "character")
})

test_that("Active Directory Authorization", {
  skip_if(testBaseUrl == "")
  expect_error(authorizeWebApi(testBaseUrl, "ad", testOhdsiUser, testOhdsiPassword), "fail")
})
