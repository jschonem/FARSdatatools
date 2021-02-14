test_that("proper filename is generated", {
  filename <- make_filename("2013")
  expect_match(filename, "^accident_")
  expect_match(filename, "\\.csv\\.bz2$")
  expect_match(filename, "\\d{4}")
})
