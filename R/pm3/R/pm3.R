#' PM3 Tools: A package for calculating PM3 Travel Time Reliability Scores from NPMRDS Data
#'
#' This package will help download the correct subset of NPMRDS data, calculate
#' segment-level LOTTR and TTTR metric scores for segments, and aggregate 
#' segment level scores to statewide measures.
#' 
#' Note: if your state has a large amount of data, you may encounter
#' a "Error: vector memory exhausted (limit reached?)" error. See this
#' StackOverflow post on resolving: 
#' https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
#' 
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name pm3
#' 
#' @import data.table
#' @importFrom stats quantile time
NULL