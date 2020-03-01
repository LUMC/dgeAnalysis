
source("shiny/home/UI.R", local = TRUE)
source("shiny/upload/UI.R", local = TRUE)
source("shiny/run_analysis/UI.R", local = TRUE)
source("shiny/alignment/UI.R", local = TRUE)


ui <- dashboardPage(
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
    includeCSS("css/styles.css", local = TRUE),
    
    tabItems(
      tab_home,
      tab_upload,
      tab_run_analysis,
      tab_alignment
    )
  )
)

