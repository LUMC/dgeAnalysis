
source("scripts/libraries.R", local = TRUE)
source("shiny/shiny_UI.R", local = TRUE)
source("shiny/shiny_server.R", local = TRUE)
source("scripts/functions.R", local = TRUE)

options(shiny.maxRequestSize = 50*1024^2)

shiny::shinyApp(ui = ui, server = server)