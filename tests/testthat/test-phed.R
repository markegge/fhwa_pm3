test_that("phed runs correctly", {
  
  # missing months
  moy_factor_test <- read.table(header = TRUE, sep = ",", text = "
      month,freeway,non_freeway
      1,0.94,0.94
      2,0.88,0.88
      3,1.01,1.04
      4,1.01,1.01
      5,1.05,1.05
      6,1.04,1.04
      7,1.05,1.05
      8,1.08,1.08
      9,0.99,0.99
      12,0.97,0.97")
  
  expect_warning(phed(travel_time_readings = test_path("testdata", "Readings.csv"),
                      tmc_identification = test_path("testdata", "TMC_Identification.csv"),
                      speed_limits = fread(test_path("testdata", "speed_limits.csv")),
                      urban_code = 56139,
                      population = 52898,
                      moy_factor = moy_factor_test))
  
  # missing days
  dow_factor_test <- read.table(header = TRUE, sep = ",", text = "
      day,freeway,non_freeway
      1,1.05,1.05
      2,1.05,1.05
      3,1.05,1.05
      4,1.05,1.05
      5,1.1,1.1")
  
  expect_warning(phed(travel_time_readings = test_path("testdata", "Readings.csv"),
                      tmc_identification = test_path("testdata", "TMC_Identification.csv"),
                      speed_limits = fread(test_path("testdata", "speed_limits.csv")),
                      urban_code = 56139,
                      population = 52898,
                      dow_factor = dow_factor_test))
  
  # out of range
  dow_factor_test <- read.table(header = TRUE, sep = ",", text = "
      day,freeway,non_freeway
      2,0.05,0.05
      3,0.05,0.05
      4,0.05,0.05
      5,0.05,0.05
      6,0.1,0.1")
  
  expect_warning(phed(travel_time_readings = test_path("testdata", "Readings.csv"),
                      tmc_identification = test_path("testdata", "TMC_Identification.csv"),
                      speed_limits = fread(test_path("testdata", "speed_limits.csv")),
                      urban_code = 56139,
                      population = 52898,
                      dow_factor = dow_factor_test))
  
  # out of range
  hod_profile_test <- read.table(header = TRUE, sep = ",", text = "
      hour,freeway,non_freeway
      6,1.063,1.046
      7,0.071,0.064
      8,0.0615,0.056
      9,0.0525,0.052
      15,0.0725,0.0735
      16,0.0785,0.0795
      17,0.07,0.07
      18,0.0555,0.0575
      19,0.042,0.047")
  
  expect_warning(phed(travel_time_readings = test_path("testdata", "Readings.csv"),
                      tmc_identification = test_path("testdata", "TMC_Identification.csv"),
                      speed_limits = fread(test_path("testdata", "speed_limits.csv")),
                      urban_code = 56139,
                      population = 52898,
                      hod_profile = hod_profile_test))
  
  expect_equal({
    phed_scores <- phed(urban_code = 56139,
       population = 52898,
       travel_time_readings = test_path("testdata", "Readings.csv"),
       tmc_identification = test_path("testdata", "TMC_Identification.csv"),
       speed_limits = fread(test_path("testdata", "speed_limits.csv"))
       )
    round(phed_scores[, sum(delay, na.rm = TRUE) / 52898], 2)
    }, 
       0.18,
       tolerance = 1
  )
  
})
