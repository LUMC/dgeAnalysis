
source("scripts/libraries.R", local = TRUE)
source("shiny/ui_main.R", local = TRUE)
source("shiny/svr_main.R", local = TRUE)
source("scripts/de.R", local = TRUE)
source("scripts/enrichment.R", local = TRUE)
source("scripts/markdown.R", local = TRUE)
source("scripts/plots.R", local = TRUE)

options(shiny.maxRequestSize = 50*1024^2)
options(spinner.color="#0088cc")
# suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))
# options(warn = -1)

shiny::shinyApp(ui = ui, server = server)
