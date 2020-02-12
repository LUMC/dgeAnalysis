

## Install or import required packages ##
if (!require("shiny")) devtools::install_github('tomkuipers1402/shiny')
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
