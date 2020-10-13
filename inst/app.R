
## USE ONLY FOR DEVELOPMENT!!!

options(shiny.maxRequestSize = 50*1024^2)
options(spinner.color="#0088cc")
# suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))
options(warn = -1)

source("inst/src/lib/libraries.R", local = TRUE)
source("R/de.R", local = TRUE)
source("R/enrichment.R", local = TRUE)
source("R/markdown.R", local = TRUE)
source("R/de_plots.R", local = TRUE)
source("R/wcgna.R", local = TRUE)

shiny::runApp("inst/src/", host="0.0.0.0", port=1402, launch.browser=TRUE)
