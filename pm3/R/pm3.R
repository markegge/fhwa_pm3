#' PM3 Tools: A package for calculating PM3 Travel Time Reliability Scores from NPMRDS Data
#'
#' This package will provides functions needed to calculate PM3 System Reliability
#' and Freight Federal TPM Performance measures. 
#' 
#' Note: if your state has a large amount of data, you may encounter
#' a "Error: vector memory exhausted (limit reached?)" error. See this
#' StackOverflow post on resolving: 
#' https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
#' 
#' @section PM3 functions:
#' The functions
#' \code{tmc_list} generates a list of TMC segments in RITIS format for data downloads
#' \code{score_pm3} calculates segment-level LOTTR and TTTR metric scores for segments,
#'   including (optionally) by month
#' 
#'
#' @docType package
#' @name pm3
#' 
#' @import data.table
#' @importFrom stats quantile time
NULL