
source("tabs/home/ui_home.R", local = TRUE)
source("tabs/upload/ui_upload.R", local = TRUE)
source("tabs/run_analysis/ui_run.R", local = TRUE)
source("tabs/alignment/ui_alignment.R", local = TRUE)
source("tabs/raw_data/ui_raw.R", local = TRUE)
source("tabs/norm_data/ui_norm.R", local = TRUE)
source("tabs/pca/ui_pca.R", local = TRUE)
source("tabs/heatmaps/ui_heatmaps.R", local = TRUE)
source("tabs/analysis/ui_analysis.R", local = TRUE)
source("tabs/bias/ui_bias.R", local = TRUE)
source("tabs/enrichment/kegg/ui_kegg.R", local = TRUE)
source("tabs/enrichment/reactome/ui_reactome.R", local = TRUE)
source("tabs/enrichment/go/ui_go.R", local = TRUE)
source("tabs/enrichment/do/ui_do.R", local = TRUE)
source("tabs/wgcna/ui_wgcna.R", local = TRUE)
source("tabs/export/ui_export.R", local = TRUE)
source("tabs/about/ui_about.R", local = TRUE)


ui <- dashboardPage(
  skin = "blue",
  title = "dgeAnalysis",
  
  dashboardHeader(
    title = span(tagList(icon("dna"), "dgeAnalysis")),
    titleWidth = 400,
    tags$li(class = "dropdown", tags$a(textOutput("current_page")))
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
    tags$head(tags$link(rel="shortcut icon", href = "lumcFavicon.png")),
    
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
      tab_enrich_reactome,
      tab_enrich_go,
      tab_enrich_do,
      tab_wgcna,
      tab_export,
      tab_about
    )
  )
)
