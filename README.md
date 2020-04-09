# FHWA PM3 NPMRDS Travel Time Reliability Processing Tools

NPMRDS Travel Time data is genearlly too voluminous to be effectively managed or manipulated using "common" desktop tools such as Microsoft Excel or Tableau. This repository provides some scripts and tools for effectively working with voluminous NPMRDS data.

## Installation

```r
library(devtools)
devtools::install_bitbucket("high-street/pm3")
```


## Instructions for Use

To calculate LOTTR or TTTR Metric scores:

1. Download the shapefiles for the desired year(s) from (https://npmrds.ritis.org/analytics/shapefiles)[https://npmrds.ritis.org/analytics/shapefiles]
2. Generate TMC List with the `tmc_list` function, 

    ```R
    # Export list of Interstate TMCs for TTTR calculations
    tmc_list(infile = "shp/Utah/Utah.shp", tmcs = "interstate", outfile = "out/tmcs.txt")
    ```

3. Log in to RITIS [https://npmrds.ritis.org/analytics/](https://npmrds.ritis.org/analytics/)
4. Go to Massive Data Downloader
5. Choose the appropriate "TMC segments from" value (e.g. "NPMRDS INRIX 2019")
6. Paste in list of TMCs Segment Codes generated by `tmc_list` and click Add
7. Specify appropriate date range, e.g 01/01/2019 – 12/31/2019
8. Select data sources and measures: 

* "NPMRDS form INRIX (Trucks and Passenger Vehicles): Travel Time" for LOTTR Measure
* "NPMRDS from Inrix (Trucks): Travel Time" for TTTR Measure

9. Set averaging to 15 minutes (per PM3 Final Rule)
10. Download and extract the resulting dataset
11. Calculate scores using `score_pm3` 

    ```R
    # Calculate monthly TTTR metric scores
    tttr_scores <- score_pm3("data/Trucks/Readings.csv", 
                             metric = "TTTR", 
                             period = "monthly", 
                             verbose = TRUE)
    ```


### A Full Example

```
library(data.table)
library(pm3)
library(sf)

shp_2017 <- st_read("shp/Maryland_2017/Maryland.shp", stringsAsFactors = FALSE)
shp_2018 <- st_read("shp/Maryland_2018/Maryland.shp", stringsAsFactors = FALSE)
shp_2019 <- st_read("shp/Maryland_2019/Maryland.shp", stringsAsFactors = FALSE)

shp_2017$year <- 2017
shp_2018$year <- 2018
shp_2019$year <- 2019

# Generate TMC Lists for RITIS Download --------------------------------------
tmc_list("shp/Maryland_2017/Maryland.shp", outfile = "out/tmcs_2017.txt")
tmc_list("shp/Maryland_2018/Maryland.shp", outfile = "out/tmcs_2018.txt")
tmc_list("shp/Maryland_2019/Maryland.shp", outfile = "out/tmcs_2019.txt")

# Score Segments -------------------------------------------------------------
# Extract RITIS export and update paths below appropriately
lottr_2017 <- score("data/md_2017/md_2017.csv", metric = "LOTTR")
lottr_2018 <- score("data/md_2018/md_2018.csv", metric = "LOTTR")
lottr_2019 <- score("data/md_2019/md_2019.csv", metric = "LOTTR")

# LOTTR ------------------
lottr_2017 <- merge(st_drop_geometry(shp_2017), lottr_2017, by.x = "Tmc", by.y = "tmc_code")
lottr_2018 <- merge(st_drop_geometry(shp_2018), lottr_2018, by.x = "Tmc", by.y = "tmc_code")
lottr_2019 <- merge(st_drop_geometry(shp_2019), lottr_2019, by.x = "Tmc", by.y = "tmc_code")

lottr <- rbindlist(list(lottr_2017, lottr_2018, lottr_2019))

setDT(lottr)

lottr[, nhs_miles := Miles * NHS_Pct * 0.01]
lottr[, vmt := AADT * ifelse(FacilType == 1, 1.0, 0.5) * nhs_miles]
lottr[, interstate := ifelse(F_System == 1, "Interstate", "Non-Interstate NHS")]

# LOTTR Score 
lottr[, 
      .(pct_reliable = sum(vmt * reliable, na.rm = TRUE) / sum(vmt)),
      by = .(interstate, year)]


# TTTR -------------------------------------
tmc_list("shp/Maryland_2017/Maryland.shp", tmcs = "interstate", outfile = "out/tmcs_interstate_2017.txt")
tmc_list("shp/Maryland_2018/Maryland.shp", tmcs = "interstate", outfile = "out/tmcs_interstate_2018.txt")
tmc_list("shp/Maryland_2019/Maryland.shp", tmcs = "interstate", outfile = "out/tmcs_interstate_2019.txt")

# Download annual datasets from RITIS

tttr_2017 <- score("data/md_trucks_2018/md_int_2017.csv", metric = "TTTR")
tttr_2018 <- score("data/md_trucks_2018/md_int_2018.csv", metric = "TTTR")
tttr_2019 <- score("data/md_trucks_2019/md_int_2019.csv", metric = "TTTR")

# TTTR ------------------
tttr_2017 <- merge(st_drop_geometry(shp_2017), tttr_2017, by.x = "Tmc", by.y = "tmc_code")
tttr_2018 <- merge(st_drop_geometry(shp_2018), tttr_2018, by.x = "Tmc", by.y = "tmc_code")
tttr_2019 <- merge(st_drop_geometry(shp_2019), tttr_2019, by.x = "Tmc", by.y = "tmc_code")

tttr <- rbindlist(list(tttr_2017, tttr_2018, tttr_2019))

setDT(tttr)

tttr <- tttr[F_System == 1]

tttr[, nhs_miles := Miles * NHS_Pct * 0.01]

# TTTR Score
tttr[, .(tttr_index = sum(max_tttr * nhs_miles) / sum(nhs_miles)), by = year]

```

## PM3 Guidance

The PM3 Performance measures are best described in [FHWA's June 1, 2017 PM3 Webinar Presentation](https://www.fhwa.dot.gov/tpm/rule/170601pm3.pdf)

FWHA has also provided both [General Guidance and Step-by-Step Metric
Calculation Procedures for National Performance Measures for
Congestion, Reliability, and Freight, and CMAQ Traffic Congestion](https://www.fhwa.dot.gov/tpm/guidance/hif18040.pdf] and [FHWA Computation Procedure for Travel Time Based and Percent Non-Single Occupancy Vehicle (non-SOV) Travel Performance Measures](https://www.fhwa.dot.gov/tpm/guidance/hif18024.pdf). To be clear, these are two different 40+ page guides, published by FHWA within a month of each other—one created by a consultant, the other created by FHWA. 

If the two competing FHWA guides contradict themselves, reference the definitive [Federal Register PM3 Final Rule](https://www.federalregister.gov/documents/2018/05/31/2018-11652/national-performance-management-measures-assessing-performance-of-the-national-highway-system).

## TMC Network

Each year the TMC network is updated. This reflects improvements in TTI's conflation methodolgies, corrections submitted by users, as well as changes in the underlying attribute data.

The NPMRDS TMC network is a set of INRIX/HERE TMC segments to which selected HPMS attribtues have been conflated. There is a *two year lag* between the TMC network and the HPMS data. For example, the 2019 NPMRDS TMC network reflects AADT values based on 2017 HPMS. Since FHWA only requires traffic counts once every three years, some AADT values reported in the 2019 NPRMDS TMC network may be based on 2014 traffic counts. The NPMRDS reported AADT values should be calculating PM3 Performance Measures, but for other analysis it is better to use more recent sources of traffic counts. This notebook provides resources for estimating order-of-magnitude traffic volumes using NPMRDS data itself.

