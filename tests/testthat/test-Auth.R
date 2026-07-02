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
test_that("createApiKey posts correct body and prints rawKey", {
  skip_if(testBaseUrl == "")

  withr::with_envvar(c(WEBAPI_TEST_SECURE_WEBAPI_URL = testBaseUrl), {
    authorizeWebApi(testBaseUrl, "db", testOhdsiUser, testOhdsiPassword)

    result <- createApiKey(
      baseUrl       = testBaseUrl,
      name          = "r-test-key",
      description   = "Created by ROhdsiWebApi unit test",
      expiresInDays = 1L
    )

    expect_type(result, "list")
    expect_true(startsWith(result$rawKey, "wa_"))
    expect_match(result$keyIdentifier, "^[0-9a-f]{16}$")
    expect_equal(result$name, "r-test-key")
  })
})