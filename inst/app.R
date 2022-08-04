
## USE ONLY FOR DEVELOPMENT!!!

options(shiny.maxRequestSize = 100 * 1024 ^ 2)
options(spinner.color = "#0088cc")

##suppressWarnings(rm(
##  list = c(
##    "deTab",
##    "normDge",
##    "enrich",
##    "inUse_deTab",
##    "inUse_normDge",+
##    inUse_enrich
##  ),
##  envir = .GlobalEnv
##))

options(warn = -1)

source("inst/src/lib/libraries.R", local = TRUE)
source("R/de.R", local = TRUE)
source("R/enrichment.R", local = TRUE)
source("R/markdown.R", local = TRUE)
source("R/de_plots.R", local = TRUE)


source("R/plots.R", local = TRUE)
source("R/plot_prep.R", local = TRUE)


shiny::runApp(
  appDir = "inst/src/",
  host = "0.0.0.0",
  port = 1402,
  launch.browser = TRUE
)
