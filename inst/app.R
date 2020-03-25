
## USE ONLY FOR DEVELOPMENT!!!

options(shiny.maxRequestSize = 50*1024^2)
options(spinner.color="#0088cc")
# suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))
options(warn = -1)

source("inst/src/scripts/libraries.R", local = TRUE)
source("inst/src/scripts/de.R", local = TRUE)
source("inst/src/scripts/enrichment.R", local = TRUE)
source("inst/src/scripts/markdown.R", local = TRUE)
source("inst/src/scripts/plots.R", local = TRUE)

shiny::runApp("inst/src/", host="0.0.0.0", port=1402, launch.browser=TRUE)
