# DGE Analysis

This R package contains the R-Shiny application developed to perform differential gene expression analysis.\
Note: This application is still under development. An extensive readme is coming.

## Installing
1. Install the "devtools" package (if not already installed):
```r
install.packages("devtools")
```
2. Install the dgeAnalysis package:
```r
library("devtools")
devtools::install_github("LUMC/DGE_analysis")
```
3. Launch the application:
```r
library(dgeAnalysis)
dgeAnalysis::startApp()
```

## Quick start
1. Start the application
2. Go to "Data upload"
3. Upload files:
    * Samplesheet (metadata), Contains information about each sample.
    * Count data, Contains the read alignment results (read counts per feature).
    * Annotation, Contains more information about genes (location, length, etc.).
4. Set a comparison
5. Run the analysis
6. View the results
