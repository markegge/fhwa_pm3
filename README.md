# NPMRDS Processing Tools for Assessing System Performance, Freight Movement, and CMAQ Improvement Program

This repository provides some scripts and tools written in R for effectively working with voluminous NPMRDS data for calculating the FHWA Transportation Performance Management (TPM) PM3 System Reliability, Freight, and CMAQ Congestion Performance  Performance Measures. For use with NPMRDS (2016 – Present) downloaded from https://npmrds.ritis.org/

Use this package to: 

* Calculate TMC-segment Level of Travel Time Reliability (LOTTR) and Truck Travel Time Reliability (TTTR) metric scores
* Calculate Interstate / Non-Interstate NHS Percent of Person Miles Reliable and TTTR Index performance measures
* Calculate Annual Hours of Peak Hour Excessive Delay per Capita performance measure
* Generate an HPMS Submittal File based on the [HPMS Field Manual Supplemental Guidance](https://www.fhwa.dot.gov/tpm/guidance/pm3_hpms.pdf)


## Installation

```r
library(devtools)
devtools::install_github("markegge/fhwa_pm3")
library(pm3)
```

## Calculating LOTTR and TTTR Metric Scores

To calculate LOTTR or TTTR Metric scores:

1. Log in to RITIS [https://npmrds.ritis.org/analytics/](https://npmrds.ritis.org/analytics/)
2. Go to Massive Data Downloader
3. Choose the appropriate "TMC segments from" value (e.g. "NPMRDS INRIX 2019")
4. Choose your region (e.g. Wyoming) click Add
5. Specify appropriate date range, e.g 01/01/2019 – 12/31/2019
![Massive Data Downloader Region and Dates](man/mdd_1.png)
6. Select data sources and measures: 
    * "NPMRDS form INRIX (Trucks and Passenger Vehicles): Travel Time" for LOTTR Measure (uncheck Speed, Historic average speed, Reference speed, and Data Density)
    * "NPMRDS from Inrix (Trucks): Travel Time" for TTTR Measure (uncheck Speed, Historic average speed, Reference speed, and Data Density)
9. Set averaging to 15 minutes (per PM3 Final Rule) and Submit
![Massive Data Downloader Data Sources and Units](man/mdd_2.png)
10. Download and extract the resulting dataset
11. Calculate scores using `score` 


### A Minimal Example

_To run the example below, copy `Readings.csv` and `TMC_Identification.csv` from `tests/testthat` into your working directory._

```R
library(data.table)
library(pm3)

# Caclulate segment-level LOTTR and TTTR scores
lottr_scores <- score("Readings.csv", metric = "LOTTR")
tttr_scores <- score("Readings.csv", metric = "TTTR")

# Read in TMC attributes from RITIS export and calculate attributes
tmcs <- fread("TMC_Identification.csv")

tmcs[, nhs_miles := miles * nhs_pct * 0.01]
tmcs[, vmt := ifelse(faciltype == 1, 1.0, 0.5) * aadt * nhs_miles]
tmcs[, system := ifelse(f_system == 1, "Interstate", "Non-Interstate NHS")]

# Merge the tmc_scores table to the tmcs attribute table
tmcs <- merge(tmcs, lottr_scores, by.x = "tmc", by.y= "tmc_code")
tmcs <- merge(tmcs, tttr_scores, by.x = "tmc", by.y= "tmc_code", all.x = TRUE)

# Calculate LOTTR scores
tmcs[!is.na(vmt), .(pct_reliable = sum(vmt * reliable) / sum(vmt)), by = system]
#>                system pct_reliable
#> 1: Non-Interstate NHS    0.7545984
#> 2:         Interstate    1.0000000

# Calculate TTTR score
tmcs[f_system == 1, .(tttr_index = sum(max_tttr * nhs_miles) / sum(nhs_miles))]
#>    tttr_index
#> 1:       1.08

# Calculating Peak Hour Excess Delay
phed(urban_code = 56139,
       population = 52898,
       travel_time_readings = "Readings.csv",
       tmc_identification = "TMC_Identification.csv",
       speed_limits = fread("speed_limits.csv"))
#> Peak Hour Excess Delay per Capita for 2020: 0.13 hours
```

## Creating an HPMS Submittal File

The `hpms()` function outputs a .txt file in the appropriate format for HPMS submission. See the (HPMS Supplemental Guidance for PM3)[https://www.fhwa.dot.gov/tpm/guidance/pm3_hpms.pdf]

Note, the input scores *must* be generated with verbose = TRUE.

```R
shp <- st_read("shp/Wyoming_2019/Wyoming.shp", stringsAsFactors = FALSE)
lottr <- score("data/All_Vehicles/Readings.csv", metric = "LOTTR", verbose = TRUE)
tttr <- score("data/Trucks/Readings.csv", metric = "TTTR", verbose = TRUE)
hpms(shp, lottr, tttr)
```

** Note: the `hpms()` function does not support the ability to join PHED scores at this time. **

## PM3 Guidance

The PM3 Performance measures are best described in [FHWA's June 1, 2017 PM3 Webinar Presentation](https://www.fhwa.dot.gov/tpm/rule/170601pm3.pdf)

FWHA has also provided two sets of guidance on calculating the LOTTR and TTTR performance metrics:

* [General Guidance and Step-by-Step Metric Calculation Procedures for National Performance Measures for Congestion, Reliability, and Freight, and CMAQ Traffic Congestion](https://www.fhwa.dot.gov/tpm/guidance/hif18040.pdf) 
* [FHWA Computation Procedure for Travel Time Based and Percent Non-Single Occupancy Vehicle (non-SOV) Travel Performance Measures](https://www.fhwa.dot.gov/tpm/guidance/hif18024.pdf)

You can also reference the definitive [Federal Register PM3 Final Rule](https://www.federalregister.gov/documents/2018/05/31/2018-11652/national-performance-management-measures-assessing-performance-of-the-national-highway-system).

## TMC Network and Traffic Volumes

Each year the TMC network is updated. This reflects improvements in TTI's conflation methodolgies, corrections submitted by users, as well as changes in the underlying attribute data.

The NPMRDS TMC network is a set of INRIX/HERE TMC segments to which selected HPMS attribtues have been conflated. There is a *two year lag* between the TMC network and the HPMS data. For example, the 2019 NPMRDS TMC network reflects AADT values based on 2017 HPMS. Since FHWA only requires traffic counts once every three years, some AADT values reported in the 2019 NPRMDS TMC network may be based on 2014 traffic counts. The NPMRDS reported AADT values should be calculating PM3 Performance Measures, but for other analysis it is better to use more recent sources of traffic counts. 

For a method for estimating order-of-magnitude traffic volumes using NPMRDS data itself, see [this repository](https://bitbucket.org/high-street/npmrds_probe_counts/).

## Attribution

Package author: Mark Egge, High Street (egge@highstreetconsulting.com)

<<<<<<< HEAD
## What's New

June 10, 2022: Added PHED function to calculate PHED given a travel time readings file and speed limits
=======
License: Mozilla Public License Version 2.0
>>>>>>> f240ec482c97d0198d57f75dbb712d47264636f0
