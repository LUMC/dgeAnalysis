# dgeAnalysis

This R package contains the R-Shiny application v1.5.1 developed to perform differential gene expression analysis.
* dgeAnalysis manual: https://github.com/LUMC/dgeAnalysis/blob/master/MANUAL.pdf

## Installing
1. Install the "devtools" package (if not already installed):
```r
install.packages("devtools")
```
2. Install the "dgeAnalysis" package:
```r
library("devtools")
devtools::install_github("LUMC/dgeAnalysis")
```
3. Launch the application:
```r
library("dgeAnalysis")
dgeAnalysis::startApp()
```

## Quick start
1. Start the application
2. Go to "Data upload"
3. Upload files: (both CSV and TSV is accepted)
    * Samplesheet (metadata), Contains information about each sample.
    * Count data, Contains the read alignment results (read counts per feature).
    * Annotation, Contains more information about genes (location, length, etc.).
4. Set a comparison
5. Run the analysis
6. View the results

Note: In the folder "exampleData" there is a .zip file, containing three data files. These example files can be used to e.g. check if the application is working and/or your data has the correct format.

## Requirements
R version: 4.1+

Linux install libraries:
````
sudo apt-get update && apt-get install \
   build-essential \
   gdebi-core \
   pandoc \
   pandoc-citeproc \
   libcairo2-dev \
   libjpeg-dev \
   libxt-dev \
   libssl-dev \
   libcurl4-gnutls-dev \
   libxml2-dev \
   libcurl4-openssl-dev \
   -y
````

Windows install RTools and pandoc:\
R 4.0 and up:\
&nbsp; https://cran.r-project.org/bin/windows/Rtools/rtools40-i686.exe \
&nbsp; https://cran.r-project.org/bin/windows/Rtools/rtools40-x86_64.exe \
Pandoc:\
&nbsp; https://pandoc.org/installing.html

## Fix install problems
If there are problems installing the application, it might be worth to try installing specific packages manually:
```R
## Shiny environment
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("shinyjs")) install.packages("shinyjs")

## Differential expression analysis
if (!require("BiocManager")) install.packages("BiocManager")
if (!require("knitr")) install.packages("knitr")
if (!require("SummarizedExperiment")) BiocManager::install("SummarizedExperiment")
if (!require("edgeR")) BiocManager::install("edgeR")
if (!require("limma")) BiocManager::install("limma")
if (!require("DESeq2")) BiocManager::install("DESeq2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")
if (!require("broom")) install.packages("broom")
if (!require("plotly")) install.packages("plotly")
if (!require("Rtsne")) install.packages("Rtsne")
if (!require("rmarkdown")) install.packages("rmarkdown")

## Enrichment analysis
if (!require("gprofiler2")) install.packages("gprofiler2")
if (!require("igraph")) install.packages("igraph")
```
