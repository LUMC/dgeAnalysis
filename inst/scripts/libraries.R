

## Install or import required packages ##
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

#tinytex voor pdf files
#Using shiny from my own repository from github
#dit omdat er een fout zit in bookmarken van de applicatie
#meerdere file inputs worden onder dezelfde naam opgeslagen en bij inlezen overschreven
#dit is gefixt door de naam een unieke naam mee te geven.
#in github: shiny -> R -> fileupload.R
#filename <- file.path(.dir, paste0(as.character(length(.files$name)), maybeGetExtension(fileBasename)))
#into:
#filename <- file.path(.dir, paste0(as.character(createUniqueId(10)), maybeGetExtension(fileBasename)))
