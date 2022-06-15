test_that("null file_path throws error", {
  expect_warning(score(), )
  
  scores_monthly <- lottr("Readings.csv", monthly = TRUE)

})