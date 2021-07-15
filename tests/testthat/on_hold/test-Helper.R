test_that("Test checkInputFileEncoding expected pass", {
  testData <- data.frame(id=c(1), name=c("test"))
  testFile <- tempfile("test_data.csv")
  readr::write_csv(x = testData, file = testFile)
  expect_invisible(ROhdsiWebApi::checkInputFileEncoding(testFile))
  on.exit(unlink(testFile))
})

test_that("Test checkInputFileEncoding expected fail", {
  expect_error(ROhdsiWebApi::checkInputFileEncoding("resources/bad-file-encoding.csv"))
})