test_that("phed runs correctly", {
  expect_equal(phed(urban_code = 56139,
       population = 52898,
       travel_time_readings = "Readings.csv",
       tmc_identification = "TMC_Identification.csv",
       speed_limits = fread("speed_limits.csv")), 
       0.33335481,
       tolerance = 0.1
  )
  
})
