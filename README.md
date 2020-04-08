# FHWA PM3 NPMRDS Travel Time Reliability Processing Tools

NPMRDS Travel Time data is genearlly too voluminous to be effectively managed or manipulated using "common" desktop tools such as Microsoft Excel or Tableau. This repository provides some scripts and tools for effectively working with voluminous NPMRDS data.

## PostgreSQL and R

This repository provides two alternative "tool suites" to calculate PM3 performance measures:

* Postgres: a fast, modern, and free database, spatially enabled with PostGIS. This notebook is best for spatial visualization, such as connecting QGIS or ArcGIS to your PM3 data
* R: R is a statistical programming language. Using the data.table library, processing PM3 scores is fast and light

## Guidance

The PM3 Performance measures are best described in [FHWA's June 1, 2017 PM3 Webinar Presentation](https://www.fhwa.dot.gov/tpm/rule/170601pm3.pdf)

FWHA has also provided both [General Guidance and Step-by-Step Metric
Calculation Procedures for National Performance Measures for
Congestion, Reliability, and Freight, and CMAQ Traffic Congestion](https://www.fhwa.dot.gov/tpm/guidance/hif18040.pdf] and [FHWA Computation Procedure for Travel Time Based and Percent Non-Single Occupancy Vehicle (non-SOV) Travel Performance Measures](https://www.fhwa.dot.gov/tpm/guidance/hif18024.pdf). To be clear, these are two different 40+ page guides, published by FHWA within a month of each other—one created by a consultant, the other created by FHWA. 

If the two competing FHWA guides contradict themselves, reference the definitive [Federal Register PM3 Final Rule](https://www.federalregister.gov/documents/2018/05/31/2018-11652/national-performance-management-measures-assessing-performance-of-the-national-highway-system).

## TMC Network

Each year the TMC network is updated. This reflects improvements in TTI's conflation methodolgies, corrections submitted by users, as well as changes in the underlying attribute data.

The NPMRDS TMC network is a set of INRIX/HERE TMC segments to which selected HPMS attribtues have been conflated. There is a *two year lag* between the TMC network and the HPMS data. For example, the 2019 NPMRDS TMC network reflects AADT values based on 2017 HPMS. Since FHWA only requires traffic counts once every three years, some AADT values reported in the 2019 NPRMDS TMC network may be based on 2014 traffic counts. The NPMRDS reported AADT values should be calculating PM3 Performance Measures, but for other analysis it is better to use more recent sources of traffic counts. This notebook provides resources for estimating order-of-magnitude traffic volumes using NPMRDS data itself.

