#' Generate a list of TMCs from a shapefile
#'
#' This function creates a comma separated list of TMCs from a given
#' network shapefile. This list of TMCs can be used with the RITIS
#' Massive Data Downloader to download the correct set of data for a given year.
#'
#' @param infile Path to the input shapefile file
#' @param interstate Should the list of TMCs be limited to Interstate NHS TMCs only?
#' @param outfile Optional, path to write resulting list to a .txt file
#' @return Character string TMCs to paste into RITIS
#' 
#' @export
tmc_list <- function(infile, interstate = FALSE, outfile = "") {
  SF <- sf::st_read(infile, stringsAsFactors = FALSE)
  
  if (interstate == TRUE)
    SF <- SF[SF$F_System == 1, ]
  
  tmcs <- paste(SF$Tmc, collapse = ", ")
  
  if (outfile != "") {
    fileConn <- file(outfile)
    writeLines(tmcs, fileConn)
    close(fileConn)
    cat("TMC list written to", outfile)
  } else {
    return(tmcs)
  }
  
}


#' Calculate LOTTR or TTTR Metric Score
#'
#' Calculate LOTTR / TTTR given a RITIS export of travel time data
#' input file must have header and format: 
#' tmc_code,measurement_tstamp,travel_time_seconds
#'  e.g. 
#'  > tmc_code,measurement_tstamp,travel_time_seconds
#'  > 116-04379,2019-01-01 00:00:00,44.78
#'  > 116-04379,2019-01-01 00:15:00,46.69
#'
#' @param infile Path to the input file with travel time readings
#' @param metric "LOTTR" or "TTTR"
#' @param verbose Provide diagnostic output
#' @return A data.table of TMC scores
#' 
#' @export
score_pm3 <- function(infile, metric = "LOTTR", verbose = FALSE) {
  DT <- fread(infile)
  
  if(verbose)
    cat("Extracting dates and times from ", nrow(DT), " records...\n")
  
  DT[, `:=`(date = as.IDate(measurement_tstamp, format = "%Y-%m-%d %H:%M:%S"),
            time = as.ITime(measurement_tstamp, format = "%Y-%m-%d %H:%M:%S"))]
  
  DT[, `:=`(dow = wday(date),
            hod = hour(time))]
  
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
  
  scores <- DT[!is.na(nhpp_period),
               .(Observations = .N,
                 denominator = quantile(travel_time_seconds, probs = c(0.5)),
                 numerator = quantile(travel_time_seconds, probs = c(ifelse(metric == "LOTTR", 0.8, 0.95)))),
               by = .(tmc_code, nhpp_period)]
  
  scores[, score := round(numerator / denominator, 2)]
  
  scores <- data.table::dcast(scores, tmc_code ~ nhpp_period, value.var = "score")
  if(metric == "LOTTR") {
    scores[, max_lottr := pmax(weekday_am, weekday_mid, weekday_pm, weekend, na.rm = TRUE)]
    scores[, reliable := max_lottr < 1.5]
  } else if (metric == "TTTR") {
    scores[, max_tttr := pmax(weekday_am, weekday_mid, weekday_pm, weekend, overnight, na.rm = TRUE)]
  }
  
  return(scores)
}
