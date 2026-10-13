
test_that("get and set permsissions works", {
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  webApiUsername <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  webApiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
  skip_if(!all(nzchar(c(baseUrl, webApiUsername, webApiPassword))))
  
  authorizeWebApi(baseUrl = baseUrl,
                  authMethod = "db",
                  webApiUsername = webApiUsername,
                  webApiPassword = webApiPassword)
  
  df <- getUserInformation(baseUrl) 
  
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("id", "login", "name") %in% names(df)))
  
})



test_that("get my permissions", {
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  webApiUsername <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  webApiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
  skip_if(!all(nzchar(c(baseUrl, webApiUsername, webApiPassword))))
  
  authorizeWebApi(baseUrl = baseUrl,
                  authMethod = "db",
                  webApiUsername = webApiUsername,
                  webApiPassword = webApiPassword)
  
  df <- getMyPermissions(baseUrl) 
  
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("id", "permission", "description") %in% names(df)))
})

test_that("get Users From Role", {
  baseUrl <- Sys.getenv("WEBAPI_TEST_SECURE_WEBAPI_URL") 
  webApiUsername <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_NAME")
  webApiPassword <- Sys.getenv("WEBAPI_TEST_ADMIN_USER_PASSWORD")
  skip_if(!all(nzchar(c(baseUrl, webApiUsername, webApiPassword))))
  
  authorizeWebApi(baseUrl = baseUrl,
                  authMethod = "db",
                  webApiUsername = webApiUsername,
                  webApiPassword = webApiPassword)
  
  df <- getUsersFromRole(baseUrl = baseUrl, roleId = 10) 
  
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("id", "login", "name") %in% names(df)))
})
