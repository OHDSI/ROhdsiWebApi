# Tests for WebApi.R file

with_mock_dir("mocks/WebApi", {
  
  test_that("getPriorityVocabularyKey works", {
    key <- getPriorityVocabularyKey(baseUrl)
    expect_equal(key, sourceKeyVariable)
  })
  
  test_that("getWebApiVersion works", {
    version <- getWebApiVersion(baseUrl)
    expect_type(version, "character")
  })
  
  test_that("whoAmI returns login and name", {
    me <- whoAmI(baseUrl)
    expect_equal(me, list(login = "na\\testuser", name = "NA\\testuser"))
  })
  
  test_that("getMyPermissions returns sorted permissions", {
    permissions <- getMyPermissions(baseUrl)
    expect_s3_class(permissions, "tbl_df")
    expect_named(permissions, "permission")
    expect_equal(permissions$permission,
                 c("create:cohort-definition", "create:conceptset", "list",
                   "read:cohort-definition", "read:conceptset"))
  })
  
  test_that("getCdmSources works", {
    sources <- getCdmSources(baseUrl)
    expect_s3_class(sources, "data.frame")
  })
  
  test_that("isValidId works for cohorts", {
    isValid <- isValidId(idCohort, baseUrl, "cohort")
    expect_true(isValid)
    
    isValid <- isValidId(1e9, baseUrl, "cohort")
    expect_false(isValid)
  }) 
  
  test_that("isValidSourceKey works for cohorts", {
    isValid <- isValidSourceKey(sourceKeyVariable, baseUrl)
    expect_true(isValid)
    
    isValid <- isValidSourceKey("blah", baseUrl)
    expect_false(isValid)
  })
  
})

