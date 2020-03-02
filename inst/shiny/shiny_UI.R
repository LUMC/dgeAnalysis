
source("shiny/home/UI.R", local = TRUE)
source("shiny/upload/UI.R", local = TRUE)
source("shiny/run_analysis/UI.R", local = TRUE)
source("shiny/alignment/UI.R", local = TRUE)
source("shiny/raw_data/UI.R", local = TRUE)
source("shiny/norm_data/UI.R", local = TRUE)
source("shiny/pca/UI.R", local = TRUE)
source("shiny/heatmaps/UI.R", local = TRUE)
source("shiny/analysis/UI.R", local = TRUE)
source("shiny/bias/UI.R", local = TRUE)
source("shiny/enrichment/kegg/UI.R", local = TRUE)
source("shiny/export/UI.R", local = TRUE)


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
    useShinyjs(),
    includeCSS("css/styles.css", local = TRUE),
    
    tabItems(
      tab_home,
      tab_upload,
      tab_run_analysis,
      tab_alignment,
      tab_raw_data,
      tab_norm_data,
      tab_pca,
      tab_heatmaps,
      tab_analysis,
      tab_bias,
      tab_enrich_kegg,
      tab_export
    )
  )
)

