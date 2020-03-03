
source("scripts/libraries.R", local = TRUE)
source("shiny/ui_main.R", local = TRUE)
source("shiny/svr_main.R", local = TRUE)
source("scripts/de.R", local = TRUE)
source("scripts/enrichment.R", local = TRUE)
source("scripts/markdown.R", local = TRUE)
source("scripts/plots.R", local = TRUE)

options(shiny.maxRequestSize = 50*1024^2)

shiny::shinyApp(ui = ui, server = server)
