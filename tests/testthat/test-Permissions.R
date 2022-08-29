
test_that("get and set permsissions works", {
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  
  authorizeWebApi(baseUrl = baseUrl,
                  authMethod = "db",
                  webApiUsername = Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME"),
                  webApiPassword = Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD"))
  
  df <- getUserInformation(baseUrl) 
  
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("id", "login", "name") %in% names(df)))
  
})



test_that("get my permissions", {
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  
  authorizeWebApi(baseUrl = baseUrl,
                  authMethod = "db",
                  webApiUsername = Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME"),
                  webApiPassword = Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD"))
  
  df <- getMyPermissions(baseUrl) 
  
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("id", "permission", "description") %in% names(df)))
})
