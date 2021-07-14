test_that("Test checkInputFileEncoding expected pass", {
  testdata <- data.frame(id=c(1), name=c("test"))
  testfile <- tempfile("test_data.csv")
  readr::write_csv(x = testdata, file = testfile)
  expect_invisible(ROhdsiWebApi::checkInputFileEncoding(testfile))
  on.exit(unlink(testfile))
})

test_that("Test checkInputFileEncoding expected fail", {
  expect_error(ROhdsiWebApi::checkInputFileEncoding("resources/bad-file-encoding.csv"))
})