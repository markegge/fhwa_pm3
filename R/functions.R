#' Calculate LOTTR Metric Score
#'
#' Calculate LOTTR given a RITIS NPMRDS export of travel time data.
#' Data is passed in as the path to the csv containing the travel time readings.\
#' Travel time units must be seconds and averaging should be 15 minutes. 
#'
#' @param travel_time_readings path to RITIS export CSV with 15-minute average readings for all vehicles
#' @param monthly TRUE or FALSE specifies if the results should be
#'   aggregated by month. If FALSE scores will be computed by TMC for all
#'   data in the input file.
#' @param verbose Provide diagnostic output and return all calculated values (necessary for \code{\link{hpms}} function)
#' @return A data.table of LOTTR scores by TMC
#' 
#' @examples
#' \dontrun{
#' lottr("data/All_Vehicles/Readings.csv")
#' lottr("data/All_Vehicles/Readings.csv", monthly = TRUE)
#' }
#' 
#' @export
lottr <- function(travel_time_readings = NULL, monthly = FALSE, verbose = FALSE) {
  scores <- score(travel_time_readings, metric = "LOTTR", monthly, verbose)

  if(verbose == TRUE) {
    scores[, max_lottr := pmax(score_weekday_am, score_weekday_mid, score_weekday_pm, score_weekend, na.rm = TRUE)]
  } else {
    scores[, max_lottr := pmax(weekday_am, weekday_mid, weekday_pm, weekend, na.rm = TRUE)]
  }
  scores[, reliable := max_lottr < 1.5]
  
  return(scores)
}


#' Calculate TTTR Metric Score
#'
#' Calculate TTTR given a RITIS NPMRDS export of travel time data.
#' Data is passed in as the path to the csv containing the travel time readings.\
#' Travel time units must be seconds and averaging should be 15 minutes. 
#'
#' @param travel_time_readings path to RITIS export CSV with 15-minute average readings for trucks
#' @param monthly TRUE or FALSE specifies if the results should be
#'   aggregated by month. If FALSE scores will be computed by TMC for all
#'   data in the input file.
#' @param verbose Provide diagnostic output and return all calculated values (necessary for \code{\link{hpms}} function)
#' @return A data.table of TTTR scores by TMC
#' 
#' @examples
#' \dontrun{
#' tttr("data/Trucks/Readings.csv")
#' tttr("data/Trucks/Readings.csv", monthly = TRUE)
#' }
#' 
#' @export
tttr <- function(travel_time_readings = NULL, monthly = FALSE, verbose = FALSE) {
  scores <- score(travel_time_readings, metric = "TTTR", monthly, verbose)
  
  if(verbose == TRUE) {
    scores[, max_tttr := pmax(score_weekday_am, score_weekday_mid, score_weekday_pm, score_weekend, score_overnight, na.rm = TRUE)]
  } else {
    scores[, max_tttr := pmax(weekday_am, weekday_mid, weekday_pm, weekend, overnight, na.rm = TRUE)]
  }

  return(scores)
}


#' Internal function to Calculate LOTTR or TTTR Metric Score
#'
#' Calculate LOTTR / TTTR given a RITIS NPMRDS export of travel time data.
#' Data is passed in as the path to the csv containing the travel time
#' readings.
#' input file must have header and format: 
#' tmc_code,measurement_tstamp,travel_time_seconds
#'  e.g. 
#'  > tmc_code,measurement_tstamp,travel_time_seconds
#'  > 116-04379,2019-01-01 00:00:00,44.78
#'  > 116-04379,2019-01-01 00:15:00,46.69
#'
#' @param input_file Path to file containing travel time readings
#' @param metric "LOTTR" or "TTTR"
#' @param monthly TRUE or FALSE specifies if the results should be
#'   aggregated by month. If FALSE scores will be computed by TMC for all
#'   data in the input file.
#' @param verbose Provide diagnostic output and return all calculated values
#' @return A data.table of LOTTR/TTTR scores by TMC
#' 
#' @examples
#' \dontrun{
#' score("data/All_Vehicles/Readings.csv", metric = "LOTTR")
#' score("data/Trucks/Readings.csv", metric = "TTTR", monthly = TRUE)
#' }

score <- function(input_file = NULL, metric, monthly = FALSE, verbose = FALSE) {
  if (!is.null(input_file)) {
    DT <- fread(input_file)
  } else {
    warning("Please provide a valid path to travel time readings, e.g. input_file = 'path/to/readings.csv'.")
    return()
  }
  
  if(!all(c("tmc_code", "measurement_tstamp", "travel_time_seconds") %in% colnames(DT))) {
    warning("Invalid readings file format. Please ensure you selected Seconds as your travel time unit.")
  }
  
  cat("Read ", nrow(DT), " records. Estimated processing time: ", round(nrow(DT) / 1E8, 2), " minutes.\n")

  if(uniqueN(DT$measurement_tstamp) < 480)
    warning("Provided travel time readings file contains less than one week of data. Is this what you intended?")

  if(!("POSIXct" %in% class(DT$measurement_tstamp))) {
    warning("measurement_tstamp not POSIXct - consider updating to the latest versinon of data.table")
    DT[, measurement_tstamp := fasttime::fastPOSIXct(measurement_tstamp, tz = "GMT")]  
  }
  
  DT[, `:=`(dow = wday(measurement_tstamp),
            hod = hour(measurement_tstamp))]
  
  if(monthly == TRUE) {
    DT[, period := format(measurement_tstamp, "%Y-%m")]
  } else {
    DT[, period := year(measurement_tstamp)]
  }
  
  if(uniqueN(year(DT$measurement_tstamp)) > 1) {
    warning("More than one year detected. Due to annual TMC network changes,
            please process no more than one year at a time.")
    return()
  }
  
  if(verbose)
    cat("Assigning NHPP Periods...\n")
  
  DT[dow %in% c(2, 3, 4, 5, 6),
     nhpp_period := cut(c(hod),
                        breaks = c(0, 5, 9, 15, 19, 23),
                        labels = c(NA, "weekday_am", "weekday_mid",
                                   "weekday_pm", NA), include.lowest = TRUE)]
  DT[dow %in% c(1, 7) & hod >= 6 & hod < 20, nhpp_period := "weekend"]
  
  # TTTR has a fifth, overnight period 8 pm - 6 am
  if (metric == "TTTR") {
    DT[hod < 6 | hod >= 20, nhpp_period := "overnight"]
  }
  
  DT[, nhpp_period := as.character(nhpp_period)] # Goodbye, factor, ruining my day
  
  if(verbose)
    cat("Calculating Scores...\n")
  
  group <- quote(.(tmc_code, nhpp_period, period))
  
  scores <- DT[!is.na(nhpp_period),
               .(Observations = .N,
                 denominator = quantile(travel_time_seconds, 
                                        probs = c(0.5), 
                                        type = 1),
                 numerator = quantile(travel_time_seconds, 
                                      probs = c(ifelse(metric == "LOTTR", 0.8, 0.95)),
                                      type = 1)),
               by = eval(group)]
  
  rm(DT) # R doesn't seem to garbage collect
  
  scores[, score := round(numerator / denominator, 2)]
  
  vv <- "score"
  if(verbose == TRUE)
    vv = c("denominator", "numerator", vv)

  scores <- data.table::dcast(scores, tmc_code + period ~ nhpp_period, value.var = vv)
  
  return(scores)
}


# default factor tables / volume profiles
# based on FHWA guidance

moy_factor_default <- read.table(header = TRUE, sep = ",", text = "
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
      10,1.04,1.04
      11,0.95,0.95
      12,0.97,0.97")

dow_factor_default <- read.table(header = TRUE, sep = ",", text = "
      day,freeway,non_freeway
      1,0.8,0.8
      2,1.05,1.05
      3,1.05,1.05
      4,1.05,1.05
      5,1.05,1.05
      6,1.1,1.1
      7,0.9,0.9")

hod_profile_default <- read.table(header = TRUE, sep = ",", text = "
      hour,freeway,non_freeway
      6,0.063,0.046
      7,0.071,0.064
      8,0.0615,0.056
      9,0.0525,0.052
      15,0.0725,0.0735
      16,0.0785,0.0795
      17,0.07,0.07
      18,0.0555,0.0575
      19,0.042,0.047")

#' Calculate PHED Metric
#'
#' Calculate the CMAQ Traffic Congestion Measure in accordance with
#' \href{https://www.fhwa.dot.gov/tpm/guidance/hif18040.pdf}{FHWA General Guidance and Step-by-Step Metric Calculation Procedures for National Performance Measures for Congestion, Reliability, and Freight, and CMAQ Traffic Congestion}
#' Requires the speed limits for all TMC segments. Other parameters are optional
#' and (with defaults supplied from  FHWA's guidance). Uses the same travel time readings
#' file as \code{score(..., metric = "LOTTR")}. Outputs annual hours of delay.
#' 
#' @param travel_time_readings path to readings CSV with 15-minute travel time observations for all vehicles exported from RITIS.
#' @param tmc_identification Path to TMC_Identification.csv file provided by RITIS with travel time download.
#' @param speed_limits a data.frame-like object with speed limits for all TMCs with format tmc,speed_limit
#' @param urban_code optional vector of one (or more) urban_code values. if specified, filters TMCs to the relevant urban_code
#' @param pm_peak 3 or 4. Peak Period is defined as weekdays from 6 am to 10 am and either 3 pm to 7 pm (3) or 4 pm to 8 pm (4)
#' @param avo_cars Average vehicle occupancy for passenger vehicles
#' @param avo_trucks Average vehicle occupancy for freight trucks
#' @param avo_buses Average vehicle occupancy for buses
#' @param moy_factor Month of year traffic adjustment factors for freeways and non-freeway facilities in format \preformatted{month,freeway,non_freeway
#' 1, 0.99, 0.98
#' ... 
#' 12, 1.01, 1.00}
#' @param dow_factor Day of week adjustment factors. Monday (2) through Friday (6). Format: \preformatted{day,freeway,non_freeway
#' 2, 1.05, 1.05
#' ...
#' 6, 1.1, 1.1}
#' @param hod_profile Hourly traffic percentages for peak hours. Non-directional. Format: \preformatted{hour,freeway,non_freeway
#' 6,0.045,0.050
#' ...
#' 18,0.052,0.048}
#' @param population Optional population value. If provided, function will print PHED value
#' 
#' @return Annual hours of peak hour excessive delay per capita
#' 
#' @examples
#' \dontrun{
#' phed(travel_time_readings = "npmrds/all_vehicles_2021/Readings.csv",
#'      tmc_identification = "npmrds/all_vehicles_2021/TMC_Identification.csv", 
#'      speed_limits = fread("birmingham_tmc_speed_limits.csv"), 
#'      urban_code = 7786,
#'      pm_peak = 3, 
#'      output_file_name = "phed_2021.csv",
#'      avo_cars = 1.62, avo_trucks = 1.0, avo_buses = 5.1,
#'      moy_factor = fread("birmingham_moy_factors.csv"), 
#'      dow_factor = fread("birmingham_dow_factors.csv"), 
#'      hod_profile = fread("birmingham_hod_profile.csv"),
#'      population = 752898)
#' }
#' 
#' @export
phed <- function(travel_time_readings, tmc_identification, 
                 speed_limits, urban_code = NA, pm_peak = 3,
                 avo_cars = 1.7, avo_trucks = 1.0, avo_buses = 10.7,
                 moy_factor = moy_factor_default, 
                 dow_factor = dow_factor_default,
                 hod_profile = hod_profile_default,
                 population = NA) {
  
  if(as.integer(pm_peak) == 3)  {
    hours <- c(6, 7, 8, 9, 15, 16, 17, 18) # 3 - 7 pm
  } else if (as.integer(pm_peak) == 4) {
    hours <- c(6, 7, 8, 9, 16, 17, 18, 19) # 4 - 8 pm
  } else {
    warning("Invalid PM peak period. Use '3' for 3 - 7 pm or '4' for 4 - 8 pm")
  }
  
  setDT(moy_factor); setDT(dow_factor); setDT(hod_profile)
  hod_profile <- hod_profile[hour %in% hours]
  dow_factor <- dow_factor[day %in% c(2, 3, 4, 5, 6)]
  
  if(!nrow(hod_profile) == 8)
    warning("Invalid number of hours in hour of day profile.")
  
  if(!nrow(moy_factor) == 12)
    warning("Invalid number of months in month of year factors.")
  
  if(min(c(moy_factor$freeway, moy_factor$non_freeway)) < 0.25 | 
     max(c(moy_factor$freeway, moy_factor$non_freeway)) > 1.75)
    warning("Invalid invalid month of year adjustment factor. Expected value between 0.5 and 1.5.")
  
  if(min(c(dow_factor$freeway, dow_factor$non_freeway)) < 0.25 | 
     max(c(dow_factor$freeway, dow_factor$non_freeway)) > 1.75)
    warning("Invalid invalid day of week adjustment factor. Expected value between 0.5 and 1.5.")
  
  if(!nrow(dow_factor) == 5)
    warning("Invalid day of week factors. Expected values for day: 2, 3, 4, 5, 6 (Monday through Friday).")
  
  if(max(c(hod_profile$freeway, hod_profile$non_freeway) > 0.5))
    warning("Invalid hourly traffic profile. Expected values generally between 0.01 and 0.2.")
  
  # set up factor tables for joining later
  factors <- list("moy" = moy_factor, "dow" = dow_factor, "hod" = hod_profile)
  for(factor in names(factors)) {
    factors[[factor]] <- melt(factors[[factor]], 
                              measure.vars = c("freeway", "non_freeway"), 
                              variable.name = "road_class", variable.factor = FALSE,
                              value.name = paste0(factor, "_factor"))
  }
  
  # tmcs ----
  tmcs <- fread(tmc_identification)
  
  # filter to just selected urban area
  if(!is.na(urban_code)) {
    uc <- urban_code # to resolve variable scope issue vs. tmcs$urban_code
    tmcs <- tmcs[urban_code %in% uc]
  }

  tmcs <- tmcs[faciltype %in% c(1, 2, 6) & nhs >= 1]
  
  tmcs[, road_class := ifelse(f_system == 1, "freeway", "non_freeway")] # for volume factors
  
  stopifnot(nrow(tmcs) > 1)
  
  #
  # calculate volume (persons) per 15 mins
  #
  
  # annualized average daily people
  tmcs[, aadt_cars := aadt - (aadt_singl + aadt_combi)]
  tmcs[, aadp := avo_cars * aadt_cars + avo_buses * aadt_singl + avo_trucks * aadt_combi]
  tmcs[, aadp := aadp * nhs_pct * 0.01 * ifelse(faciltype == 1, 1.0, 0.5)]
  
  # people per period
  periods <- expand.grid(month = 1:12, day = 2:6, hour = 0:23, road_class = c("freeway", "non_freeway"))
  setDT(periods)
  
  period_factors <- merge(periods, factors[["moy"]], by = c("road_class", "month"))
  period_factors <- merge(period_factors, factors[["dow"]], by = c("road_class", "day"))
  period_factors <- merge(period_factors, factors[["hod"]], by = c("road_class", "hour"))  
  period_factors[, factor := moy_factor * dow_factor * hod_factor]
  
  #
  # calculate delay ----
  #
  
  # join speed limits
  stopifnot(c("tmc", "speed_limit") %in% colnames(speed_limits))
  tmcs <- merge(tmcs, speed_limits[, c("tmc", "speed_limit")], by = "tmc", all.x = TRUE)
  
  if(any(is.na(tmcs$speed_limit))) {
    warning(paste("Speed limits missing for ", tmcs[is.na(speed_limit)]$tmc))
  }
  
  # calculate threshold travel times
  tmcs[, threshold_speed := pmax(20, speed_limit * 0.6)]
  tmcs[, threshold_travel_time := (miles / threshold_speed) * 3600]
  
  # read in 15 minute travel time readings
  travel_time <- fread(travel_time_readings, 
                       select = c("tmc_code", "measurement_tstamp", "travel_time_seconds"))
  
  # filter to peak hours and weekdays only
  travel_time[, hour := data.table::hour(measurement_tstamp)]
  travel_time <- travel_time[hour %in% hours]
  
  travel_time[, day := data.table::wday(measurement_tstamp)]
  travel_time <- travel_time[day %in% c(2, 3, 4, 5, 6)]
  
  travel_time[, month := data.table::month(measurement_tstamp)]
  
  year <- unique(year(travel_time$measurement_tstamp))
  
  if(length(year) > 1) {
    warning("Error: data for more than one year supplied")
  }
  
  setnames(travel_time, "tmc_code", "tmc")
  
  # join in threshold travel times with TMCs
  travel_time <- merge(travel_time, tmcs[, .(tmc, road_class, aadp, threshold_travel_time)], by = "tmc")
  
  # 0 seconds min, 900 seconds max delay per FHWA's rule:
  travel_time[, delay_seconds := pmax(0, pmin(travel_time_seconds - threshold_travel_time, 900))]
  
  # calculate person hours
  travel_time <- merge(travel_time, period_factors, by = c("road_class", "month", "day", "hour"))
  
  # multiply by 0.25 because each observation is a quarter of an hour
  travel_time[, delay_person_hours := (delay_seconds / 3600) * aadp * factor * 0.25]
  
  tmc_delay <- travel_time[, .(delay = round(sum(delay_person_hours), 3)), by = tmc]
  
  # join TMC back to TMCs to capture any TMCs without travel time info
  tmc_delay <- merge(tmcs, tmc_delay, by = "tmc", all.x = TRUE)
  
  rm(travel_time); # R doesn't seem to garbage collect
  # Results ----
  
  # PHED per Capita:
  if(!is.na(population))
    cat(paste("Peak Hour Excess Delay per Capita for ", year, ":", 
              round(tmc_delay[, sum(delay) / population], 2), 
              "hours"))

  return(tmc_delay)
}


#' Generate an HPMS Submission File
#'
#' Generate an HPMS submission file in accordance with
#' \href{https://www.fhwa.dot.gov/tpm/guidance/pm3_hpms.pdf}{HPMS Field Manual Supplemental Guidance}
#' Requires the scores from score() to be run with verbose = TRUE
#' The reporting year is based on the TMC_Identification year (e.g. use 2021 TMC network for 2021 reporting in 2022)
#' Writes the resulting file to hpms_year.txt
#' 
#' @param tmc_identification Path to TMC_Identification.csv file provided by RITIS with travel time download
#' @param lottr_scores A data.table of LOTTR scores produced using \code{score(..., metric == "LOTTR")}
#' @param tttr_scores A data.table of TTTR scores produced using \code{score(..., metric == "TTTR")}
#' @param phed_scores A data.table of of PHED scores produced using \code{phed()}
#' @param occ_fac Occupancy factor. Default = 1.7
#' 
#' @examples
#' \dontrun{
#' lottr_scores <- lottr("data/All_Vehicles/al_tt_seconds.csv", verbose = TRUE)
#' tttr_scores <- tttr("data/Trucks/aldot_2019_trucks.csv", verbose = TRUE)
#' phed_scores <- phed("Readings.csv", "TMC_Identification.csv",
#'                     speed_limits = fread("speed_limits.csv"),
#'                     urban_code = 56139, pm_peak = 3, population = 52898)
#' hpms("TMC_Identification.csv", lottr_scores, tttr_scores, phed_scores)
#' }
#' 
#' @export
hpms <- function(tmc_identification, lottr_scores, tttr_scores, phed_scores = NULL, occ_fac = 1.7) {
  DT <- fread(tmc_identification)

  yr <- first(year(DT$active_start_date))
  
  if(!("denominator_weekday_am" %in% colnames(lottr_scores))) {
    cat("Error, numerator and denominator fields missing. Try rerunning score() with verbose = TRUE")
    return()
  }
  
  state <- unique(toupper(DT$state))
  stopifnot(length(state) == 1)
  state_fips <- tpm:::fips_lookup[Postal_Code == state]$FIPS_Code
  
  # Set NHS Value appropriately - no zeros allowed by FHWA!
  DT[isprimary == "0", nhs := -1]
  DT[nhs == 0, nhs := -1]
  
  DT <- DT[, .(Year_Record = yr,
               State_Code = state_fips,
               Travel_Time_Code = tmc,
               F_System = f_system,
               Urban_Code = urban_code,
               Facility_Type = faciltype,
               NHS = nhs,
               Segment_Length = round(miles * nhs_pct * 0.01, 3),
               Directionality = sapply(direction, function(x) switch(EXPR = x, "N" = 1, "S" = 2, "E" = 3, "W" = 4, 5)),
               DIR_AADT = as.integer(aadt * ifelse(faciltype == 1, 1.0, 0.5)),
               OCC_FAC = occ_fac,
               METRIC_SOURCE = 1,
               Comments = "")]
  
  # merge PHED data, if provided, else set to 0
  if(!is.null(phed_scores)) {
    DT <- merge(DT, 
                phed_scores[, .(Travel_Time_Code = tmc,PHED = delay)], 
                all.x = TRUE, by = "Travel_Time_Code")
  } else {
    DT[, PHED := 0]
  }
  
  lottr_merge <- lottr_scores[, .(Travel_Time_Code = tmc_code,
                                 LOTTR_AMP = score_weekday_am, 
                                 TT_AMP50PCT = denominator_weekday_am, 
                                 TT_AMP80PCT = numerator_weekday_am, 
                                 LOTTR_MIDD = score_weekday_mid, 
                                 TT_MIDD50PCT = denominator_weekday_mid, 
                                 TT_MIDD80PCT = numerator_weekday_mid, 
                                 LOTTR_PMP = score_weekday_pm, 
                                 TT_PMP50PCT = denominator_weekday_pm, 
                                 TT_PMP80PCT = numerator_weekday_pm, 
                                 LOTTR_WE = score_weekend, 
                                 TT_WE50PCT = denominator_weekend, 
                                 TT_WE80PCT = numerator_weekend)]
  
  tttr_merge <- tttr_scores[, .(Travel_Time_Code = tmc_code,
                                 TTTR_AMP = score_weekday_am, 
                                 TTT_AMP50PCT = denominator_weekday_am, 
                                 TTT_AMP95PCT = numerator_weekday_am, 
                                 TTTR_MIDD = score_weekday_mid, 
                                 TTT_MIDD50PCT = denominator_weekday_mid, 
                                 TTT_MIDD95PCT = numerator_weekday_mid, 
                                 TTTR_PMP = score_weekday_pm, 
                                 TTT_PMP50PCT = denominator_weekday_pm, 
                                 TTT_PMP95PCT = numerator_weekday_pm, 
                                 TTTR_WE = score_weekend, 
                                 TTT_WE50PCT = denominator_weekend, 
                                 TTT_WE95PCT = numerator_weekend, 
                                 TTTR_OVN = score_overnight, 
                                 TTT_OVN50PCT = denominator_overnight,
                                 TTT_OVN95PCT = numerator_overnight)]
  
  DT <- merge(DT, lottr_merge, by = "Travel_Time_Code", all.x = TRUE)
  DT <- merge(DT, tttr_merge, by = "Travel_Time_Code",  all.x = TRUE)
  
  
  DT <- DT[, c("Year_Record", "State_Code", "Travel_Time_Code", "F_System", "Urban_Code", 
               "Facility_Type", "NHS", "Segment_Length", "Directionality", "DIR_AADT", 
               "LOTTR_AMP", "TT_AMP50PCT", "TT_AMP80PCT", "LOTTR_MIDD", "TT_MIDD50PCT", 
               "TT_MIDD80PCT", "LOTTR_PMP", "TT_PMP50PCT", "TT_PMP80PCT", "LOTTR_WE", 
               "TT_WE50PCT", "TT_WE80PCT", "TTTR_AMP", "TTT_AMP50PCT", "TTT_AMP95PCT", 
               "TTTR_MIDD", "TTT_MIDD50PCT", "TTT_MIDD95PCT", "TTTR_PMP", "TTT_PMP50PCT", 
               "TTT_PMP95PCT", "TTTR_WE", "TTT_WE50PCT", "TTT_WE95PCT", "TTTR_OVN", "TTT_OVN50PCT", 
               "TTT_OVN95PCT", "PHED", "OCC_FAC", "METRIC_SOURCE", "Comments"), with = FALSE]
  file_name <- paste0("hpms_", yr, ".txt")
  cat("Writing output to", file_name) 
  write.table(DT, quote = FALSE, sep = "|", na = "", row.names = FALSE, file = file_name)
  return(TRUE)
}