library(data.table)

tttr_test <- score_pm3("test_data/Readings.csv", metric = "TTTR", verbose = TRUE)
tttr_monthly_test <- score_pm3("test_data/Readings.csv", metric = "TTTR", period = "monthly", verbose = TRUE)
tttr_annual_test <- score_pm3("test_data/Readings.csv", metric = "TTTR", period = "annual", verbose = TRUE)
score_pm3("test_data/Readings.csv", metric = "TTTR", period = "typo", verbose = TRUE)


lottr_monthly_test <- score_pm3("test_data/Readings.csv", metric = "LOTTR", period = "monthly", verbose = TRUE)
