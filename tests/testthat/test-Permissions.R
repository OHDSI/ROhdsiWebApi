
testthat("get and set permsissions works", {
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  
  authorizeWebApi(baseUrl = baseUrl,
                  authMethod = "db",
                  webApiUsername = Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME"),
                  webApiPassword = Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD"))
  
  df <- getUserPermission(baseUrl) 
  
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("id", "login", "name") %in% names(df)))
  
  # expect_error(setCohortPermission(baseUrl, cohortId = 3, userId = 1000, permission = "WRITE"), NA)
  # http error 403: Forbidden request.
})
