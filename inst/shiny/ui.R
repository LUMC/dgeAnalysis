
#source(system.file("shiny/home/ui.R", package = "dgeAnalysis"), local = TRUE)
#source(system.file("shiny/data_upload/ui.R", package = "dgeAnalysis"), local = TRUE)
source("home/ui.R")
source("data_upload/ui.R")

ui <- dashboardPage(
  skin = "blue",
  title = "dgeAnalysis",
  
  dashboardHeader(
    title = span(tagList(icon("dna"), "dgeAnalysis")),
    titleWidth = 400
  ),
  dashboardSidebar(
    collapsed = FALSE,
    width = 350,
    
    sidebarMenu(
      sidebarMenuOutput("sidebar_tabs")
    )
  ),
  dashboardBody(
    includeCSS("../css/styles.css"),
    tabItems(
      tab_home,
      tab_data_upload
    )
  )
)
