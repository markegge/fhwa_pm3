#' TPM Tools: A package for calculating TPM PM3 Travel Time Reliability Scores from NPMRDS Data
#'
#' This package will provides functions needed to calculate PM3 System Reliability
#' and Freight and CMAQ Congestion Federal TPM Performance measures 
#' 
#' Note: if your state has a large amount of data, you may encounter
#' a "Error: vector memory exhausted (limit reached?)" error. See this
#' StackOverflow post on resolving: 
#' https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
#' 
#' @section PM3 functions:
#' The functions
#' \code{\link{lottr}} calculated LOTTR metric scores for TMC segments
#' \code{\link{tttr}} calculates TTTR metric scores for TMC segments
#' \code{\link{phed}} calculates PHED metric scores for TMC segments
#' \code{\link{hpms}} generates an HPMS submission file in pipe delimited format
#' 
#'
#' @docType package
#' @name tpm
#' 
#' @import data.table
#' @importFrom stats quantile time
#' @importFrom utils write.table
NULL