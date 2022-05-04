test_that("null file_path throws error", {
  expect_warning(score(), )
  
  scores_monthly <- score("all_vehicles.csv", monthly = TRUE)

})