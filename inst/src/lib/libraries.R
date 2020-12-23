
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
if (!require("tidyr")) install.packages("tidyr")
if (!require("scales")) install.packages("scales")
if (!require("broom")) install.packages("broom")
if (!require("plotly")) install.packages("plotly")
if (!require("rmarkdown")) install.packages("rmarkdown")

## Pathway analysis
if (!require("gprofiler2")) install.packages("gprofiler2")
if (!require("igraph")) install.packages("igraph")
