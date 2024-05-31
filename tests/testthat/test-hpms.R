test_that("hpms runs correctly", {
  expect_equal({
      lottr_scores <- lottr(test_path("testdata", "Readings.csv"), verbose = TRUE)
      tttr_scores <- tttr(test_path("testdata", "Readings.csv"), verbose = TRUE)
      phed_scores <- phed(travel_time_readings = test_path("testdata", "Readings.csv"),
                          tmc_identification = test_path("testdata", "TMC_Identification.csv"),
                          speed_limits = fread(test_path("testdata", "speed_limits.csv")),
                          urban_code = 56139,
                          population = 50000)
      hpms(paste0(tempdir(), "/hpms_test.txt"), test_path("testdata", "TMC_Identification.csv"), lottr_scores, tttr_scores, phed_scores)
    },
    NULL
  )
  
})
