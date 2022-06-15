test_that("hpms runs correctly", {
  expect_equal({
      lottr_scores <- lottr("Readings.csv", verbose = TRUE)
      tttr_scores <- tttr("Readings.csv", verbose = TRUE)
      phed_scores <- phed(travel_time_readings = "Readings.csv",
                          tmc_identification = "TMC_Identification.csv",
                          speed_limits = fread("speed_limits.csv"),
                          urban_code = 56139,
                          population = 50000)
      hpms("TMC_Identification.csv", lottr_scores, tttr_scores, phed_scores)
    },
    TRUE
  )
  
})
