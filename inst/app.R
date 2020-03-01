
source("scripts/libraries.R", local = TRUE)
source("shiny/shiny_UI.R", local = TRUE)
source("shiny/shiny_server.R", local = TRUE)
source("scripts/functions.R", local = TRUE)

shiny::shinyApp(ui = ui, server = server)