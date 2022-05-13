# NPMRDS Travel Time Reliability Processing Tools

This repository provides some scripts and tools written in R for effectively working with voluminous NPMRDS data for calculating the FHWA PM3 System Reliability and Freight Performance TPM Performance Measures. For use with NPMRDS (2016 – Present) downloaded from https://npmrds.ritis.org/

Use this package to: 

* Calculate TMC-segment Level of Travel Time Reliability (LOTTR) and Truck Travel Time Reliability (TTTR) metric scores
* Calculate Interstate / Non-Interstate NHS Percent of Person Miles Reliable and TTTR Index performance measures
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

```R
library(data.table)
library(pm3)

# Read in TMC attributes from RITIS export 
tmcs <- fread("all_vehicles/TMC_Identification.csv")

# Caclulate segment-level LOTTR scores
lottr_scores <- score("all_vehicles/Readings.csv", metric = "LOTTR")

# Calculate segment-level TTTR scores
tttr_scores <- score("trucks/Readings.csv", metric = "TTTR")

# Merge the tmc_scores table to the tmcs attribute table
tmcs <- merge(tmcs, lottr_scores[, .(tmc = tmc_code, max_lottr, reliable)], by = "tmc")
tmcs <- merge(tmcs, tttr_scores[, .(tmc = tmc_code, max_tttr)], by = "tmc", all.x = TRUE)

# Calculate TMC attributes used for scoring
tmcs[, nhs_miles := miles * nhs_pct * 0.01]
tmcs[, vmt := ifelse(faciltype == 1, 1.0, 0.5) * aadt * nhs_miles]
tmcs[, system := ifelse(f_system == 1, "Interstate", "Non-Interstate NHS")]

# Calculate Interstate / Non-Intercent Percent of Reliable Person Miles
tmcs[!is.na(vmt), sum(vmt * reliable) / sum(vmt), by = system]

# Calcuate TTTR Index
tmcs[f_system == 1, .(tttr_index = sum(max_tttr * nhs_miles) / sum(nhs_miles))]
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

License: Mozilla Public License Version 2.0
