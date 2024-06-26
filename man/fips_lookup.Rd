% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fips_lookup}
\alias{fips_lookup}
\title{FIPS Codes}
\format{
A data frame with 56rows and 4 variables:
\describe{
  \item{State_Name}{state name (e.g. 'Alabama')}
  \item{Postal_Code}{two character state postal code (e.g. 'AL')}
  \item{FIPS_Code}{Census Bureau FIPS Code (e.g. 1)}
  \item{STATE_NAME}{capitalized state name (e.g. 'ALAMABA')}
}
}
\usage{
fips_lookup
}
\description{
This dataset provides a crosswalk between state names, postal codes, and
 FIPS codes. Used by the hpms() function. The variables are as follows:
}
\keyword{datasets}
