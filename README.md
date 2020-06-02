# dgeAnalysis

This R package contains the R-Shiny application developed to perform differential gene expression analysis.
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
R version: 3.6+

Linux libraries:
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

Windows RTools:\
   https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe

## Fix install problems
If there are problems installing the application, it might be worth to try installing specific packages manually:
```
## Install or import required packages
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
if (!require("tidyr")) install.packages("tidyr")
if (!require("scales")) install.packages("scales")
if (!require("broom")) install.packages("broom")
if (!require("plotly")) install.packages("plotly")

## Pathway analysis
if (!require("clusterProfiler")) BiocManager::install("clusterProfiler")
if (!require("DOSE")) BiocManager::install("DOSE")
if (!require("graphite")) BiocManager::install("graphite")
if (!require("ReactomePA")) BiocManager::install("ReactomePA")
if (!require("igraph")) install.packages("igraph")
if (!require("org.Hs.eg.db")) BiocManager::install("org.Hs.eg.db")
if (!require("org.Mm.eg.db")) BiocManager::install("org.Mm.eg.db")
if (!require("reshape2")) install.packages("reshape2")

## WGCNA
if (!require("WGCNA")) install.packages("WGCNA")
```
