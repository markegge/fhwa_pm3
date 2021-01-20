#' Generate a list of PM3 TMCs from a shapefile
#'
#' This function creates a comma separated list of TMCs from a given
#' network shapefile. This list of TMCs can be used with the RITIS
#' Massive Data Downloader to download the correct set of data for a given year.
#' Requires either "shp" or "infile"
#' 
#' @param shp A sf object with the corresponding shapefile
#' @param infile Path to the input shapefile file
#' @param tmcs "all" or "primary" or "interstate" - Filter TMCs
#' @param outfile Optional, path to write resulting list to a .txt file
#' @return Character string TMCs to paste into RITIS
#' 
#' @examples
#' \dontrun{
#' shp <- sf::sf_read("shp/Alabama.shp")
#' tmc_list(shp, tmcs = "interstate", outfile = "out/interstate_tmcs.txt")
#' }
#' 
#' @export
tmc_list <- function(shp = NULL, infile = "", tmcs = "all", outfile = "") {
  if(!is.null(shp) & "sf" %in% class(shp)) {
    SF <- shp
  } else if (infile != "") {
    SF <- sf::st_read(infile, stringsAsFactors = FALSE)
  } else {
    cat("Please provide either a shapefile or shapefile path as an argument.")
    return()
  }
  
  # Some TMCs have missing attributes. Omit these TMCs
  SF <- SF[!is.na(SF$F_System), ]
  
  if (tmcs == "primary")
    SF <- SF[SF$IsPrimary == 1, ]

  if (tmcs == "interstate")
    SF <- SF[SF$F_System == 1, ]
  
  tmcs <- paste(na.omit(SF$Tmc), collapse = ", ")
  
  if (outfile != "") {
    fileConn <- file(outfile)
    writeLines(tmcs, fileConn)
    close(fileConn)
    cat("TMC list written to", outfile)
  } else {
    return(tmcs)
  }
  
}


#' Calculate LOTTR Metric Score
#'
#' A convenience wrapper for score(metric = "LOTTR")
#'
#' @param input_file Path to file containing travel time readings
#' @param monthly Calculate scores by month?
#' @return A data.table of LOTTR scores by TMC
#' 
#' @examples
#' \dontrun{
#' lottr("data/All_Vehicles/Readings.csv", monthly = TRUE)
#' }
#' 
#' @export
lottr <- function(input_file, monthly = FALSE) {
  score(input_file = input_file, metric = "LOTTR",
        period = ifelse(monthly == TRUE, "monthly", "none"))
}

#' Calculate TTTR Metric Score
#'
#' A convenience wrapper for score(metric = "TTTR")
#'
#' @param input_file Path to file containing travel time readings
#' @param monthly Calculate scores by month?
#' @return A data.table of TTTR scores by TMC
#' 
#' @examples
#' \dontrun{
#' tttr("data/All_Vehicles/Readings.csv", monthly = TRUE)
#' }
#' 
#' @export
tttr <- function(input_file, monthly = FALSE) {
  score(input_file = input_file, metric = "TTTR",
        period = ifelse(monthly == TRUE, "monthly", "none"))
}


#' Calculate LOTTR or TTTR Metric Score
#'
#' Calculate LOTTR / TTTR given a RITIS NPMRDS export of travel time data.
#' Data can be passed in as the path to the csv containing the travel time
#' readings or as a data.table of travel time readings.
#' input file must have header and format: 
#' tmc_code,measurement_tstamp,travel_time_seconds
#'  e.g. 
#'  > tmc_code,measurement_tstamp,travel_time_seconds
#'  > 116-04379,2019-01-01 00:00:00,44.78
#'  > 116-04379,2019-01-01 00:15:00,46.69
#'
#' @param input_file Path to file containing travel time readings
#' @param DT A data.table of travel time readings (if input_file not supplied)
#' @param metric "LOTTR" or "TTTR"
#' @param period "none", "monthly", or "annual" specifies if the results should be
#'   aggregated by time period. If "none" scores will be computed by TMC for all
#'   data in the input file. "annual" only used if an input readings file contains
#'   data for multiple years (discouraged due to annual shapefile updates)
#' @param verbose Provide diagnostic output and return all calculated values
#' @return A data.table of LOTTR/TTTR scores by TMC
#' 
#' @examples
#' \dontrun{
#' score("data/All_Vehicles/Readings.csv", metric = "LOTTR")
#' score("data/Trucks/Readings.csv", metric = "TTTR", period = "monthly")
#' }
#' 
#' @export
score <- function(input_file = NULL, DT = NULL, metric = "LOTTR", period = "none", verbose = FALSE) {
  if(!is.null(DT) & is.data.table(DT)) {
    # so far, so good
  } else if (!is.null(input_file)) {
    DT <- fread(input_file)
  } else {
    cat("Please provide either a data.table of travel time readings or 
         path to csv of travel time readings via input_file = 'path/to/readings.csv'.")
    return()
  }
  
  
  stopifnot(all(c("tmc_code", "measurement_tstamp", "travel_time_seconds") %in% colnames(DT)))
    
  cat("Read ", nrow(DT), " records. Estimated processing time: ", nrow(DT) / 1E7, " minutes.\n")
  
  DT[, `:=`(date = as.IDate(measurement_tstamp, format = "%Y-%m-%d %H:%M:%S"),
            time = as.ITime(measurement_tstamp, format = "%Y-%m-%d %H:%M:%S"))]
  
  DT[, `:=`(dow = wday(date),
            hod = hour(time))]
  
  # check if we have a complete dataset
  if(uniqueN(DT$date) < 28) {
    warning("Dataset contains less than 28 days. Is this expected?")
  }
  
  if(period == "monthly")
    DT[, period := format(date, "%Y-%m")]
  
  if(period == "annual")
    DT[, period := year(date)]
  
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
  
  if(period == "none") {
    group <- quote(.(tmc_code, nhpp_period))
  } else if (period == "monthly" | period == "annual") {
    group <- quote(.(tmc_code, nhpp_period, period))
  } else {
    cat("Invalid parameter value for period: ", period, ".")
    return(FALSE)
  }
  
  scores <- DT[!is.na(nhpp_period),
               .(Observations = .N,
                 denominator = quantile(travel_time_seconds, probs = c(0.5)),
                 numerator = quantile(travel_time_seconds, probs = c(ifelse(metric == "LOTTR", 0.8, 0.95)))),
               by = eval(group)]
  
  scores[, score := round(numerator / denominator, 2)]
  
  vv <- "score"
  if(verbose == TRUE)
    vv = c("denominator", "numerator", vv)
    
  if(period == "none") {
    scores <- data.table::dcast(scores, tmc_code ~ nhpp_period, value.var = vv)
  } else if (period == "monthly" | period == "annual") {
    scores <- data.table::dcast(scores, tmc_code + period ~ nhpp_period, value.var = vv)
  }
  
  if(metric == "LOTTR") {
    if(verbose == TRUE) {
      scores[, max_lottr := pmax(score_weekday_am, score_weekday_mid, score_weekday_pm, score_weekend, na.rm = TRUE)]
    } else {
      scores[, max_lottr := pmax(weekday_am, weekday_mid, weekday_pm, weekend, na.rm = TRUE)]
    }
    scores[, reliable := max_lottr < 1.5]
  } else if (metric == "TTTR") {
    if(verbose == TRUE) {
      scores[, max_tttr := pmax(score_weekday_am, score_weekday_mid, score_weekday_pm, score_weekend, score_overnight, na.rm = TRUE)]
    } else {
      scores[, max_tttr := pmax(weekday_am, weekday_mid, weekday_pm, weekend, overnight, na.rm = TRUE)]
    }
  }
  
  return(scores)
}


#' Generate an HPMS Submission File
#'
#' Generate an HPMS submission file in accordance with
#' https://www.fhwa.dot.gov/tpm/guidance/pm3_hpms.pdf
#' Requires the scores from score() to be run with verbose = TRUE
#' Writes the resulting file to out/hpms_year.txt
#' 
#' @param shp The sf shapefile containing the corresponding shapefile
#' @param lottr A data.table of LOTTR scores produced using \code{score(..., metric == "LOTTR")}
#' @param tttr A data.table of TTTR scores produced using \code{score(..., metric == "TTTR")}
#' @param year Year for which the file is being generated. Defaults to last year
#' 
#' @examples
#' \dontrun{
#' shp <- st_read("shp/Wyoming_2019/Wyoming.shp", stringsAsFactors = FALSE)
#' lottr <- score("data/All_Vehicles/al_tt_seconds.csv", metric = "LOTTR", verbose = TRUE)
#' tttr <- score("data/Trucks/aldot_2019_trucks.csv", metric = "TTTR", verbose = TRUE)
#' hpms(shp, lottr, tttr)
#' }
#' 
#' @export
hpms <- function(shp, lottr, tttr, year = NA) {
  if(is.na(year))
    year <- year(Sys.Date()) - 1
  
  if(!("denominator_weekday_am" %in% colnames(lottr))) {
    cat("Error, numerator and denominator fields missing. Try rerunning score() with verbose = TRUE")
    return()
  }
  
  df <- as.data.table(st_drop_geometry(shp))
  
  state <- unique(toupper(df$State))
  stopifnot(length(state) == 1)
  state_fips <- pm3:::fips_lookup[STATE_NAME == state]$FIPS_Code
  
  DT <- df[, .(Year_Record = year,
               State_Code = state_fips,
               Travel_Time_Code = Tmc,
               F_System,
               Urban_Code,
               Facility_Type = FacilType,
               NHS = ifelse(IsPrimary == "0", -1, NHS),
               Segment_Length = round(Miles * NHS_Pct * 0.01, 3),
               Directionality = sapply(Direction, function(x) switch(x, N = 1, S = 2, E = 3, W = 4, 5)),
               DIR_AADT = as.integer(AADT * ifelse(FacilType == 1, 1.0, 0.5)),
               PHED = "",
               OCC_FAC = 1.7,
               METRIC_SOURCE = 1,
               Comments = "")]
  
  lottr_merge <- lottr[, .(Travel_Time_Code = tmc_code,
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
  
  tttr_merge <- tttr[, .(Travel_Time_Code = tmc_code,
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
  file_name <- paste0("out/hpms_", year, ".txt")
  cat("Writing output to: ", file_name) 
  write.table(DT, quote = FALSE, sep = "|", na = "", row.names = FALSE, file = file_name)
}