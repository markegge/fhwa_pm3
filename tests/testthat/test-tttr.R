test_that("null file_path throws error", {
  expect_warning(score(), )

})

test_that("tttr runs correctly", {
  expect_equal({
    tttr_scores <- tttr(test_path("testdata", "Readings.csv"))
    mean(tttr_scores$max_tttr)
  }, 
  1.638
  )
})