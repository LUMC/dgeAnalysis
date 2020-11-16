
options(warn = -1)

server <- function(input, output, session) {
  ## Check if objects need to reload
  checkReload <- function() {
    is.null(input$app_mode)
    if (input$app_mode == "view") {
      inUse_normDge <<- get_normDge()
      inUse_deTab <<- data_detab()
    }
    
    is.null(input$file_samples)
    is.null(input$file_counts)
    is.null(input$file_annotation)
    is.null(input$file_samples_view)
    is.null(input$file_counts_view)
    is.null(input$file_norm_view)
    is.null(input$file_detab_view)
    
    is.null(input$run_button)
    is.null(input$run_enrichment)
  }
  
  ## All page names
  pages <- list(
    home = "Home",
    upload = "Data upload",
    run_analysis = "Run Analysis",
    alignment = "Alignment",
    raw_data = "Raw Data",
    norm_data = "Normalization",
    pca = "PCA",
    heatmaps = "Heatmaps",
    analysis = "DE analysis",
    bias = "Bias",
    enrich_kegg = "KEGG Enrichment",
    enrich_reactome = "Reactome Enrichment",
    enrich_go = "Gene Ontology",
    enrich_do = "Disease Ontology",
    wgcna = "WGCNA",
    export = "Export",
    about = "About"
  )
  
  ## Render current page name to ui
  output[["current_page"]] <- renderText({
    tryCatch({
      page_name <- pages[[input$sidebar]]
    }, error = function(err) {
      return(NULL)
    })
  })
  
  ## Render navigation bar to ui
  output[["sidebar_tabs"]] <- renderMenu({
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Data upload",
        tabName = "upload",
        icon = icon("upload")
      ),
      menuItem(
        "Run Analysis",
        tabName = "run_analysis",
        icon = icon("calculator")
      ),
      menuItem(
        "Pre-Analysis",
        icon = icon("clipboard-check"),
        menuItem(
          "Alignment",
          tabName = "alignment",
          icon = icon("chart-line")
        ),
        menuItem(
          "Raw Data",
          tabName = "raw_data",
          icon = icon("chart-line")
        )
      ),
      menuItem(
        "Analysis",
        icon = icon("binoculars"),
        menuItem(
          "Normalization",
          tabName = "norm_data",
          icon = icon("chart-line")
        ),
        menuItem(
          "PCA",
          tabName = "pca",
          icon = icon("chart-line")
        ), 
        menuItem(
          "Heatmaps",
          tabName = "heatmaps",
          icon = icon("chart-line")
        ),
        menuItem(
          "DE analysis",
          tabName = "analysis",
          icon = icon("chart-line")
        ),
        menuItem(
          "Bias",
          tabName = "bias",
          icon = icon("chart-line")
        )
      ),
      menuItem(
        "Enrichment",
        icon = icon("bezier-curve"),
        menuItem(
          "Run enrichment",
          tabName = "run_gprofiler",
          icon = icon("calculator")
        ),
        menuItem(
          "Enrichment",
          tabName = "gprofiler",
          icon = icon("chart-line")
        )
      ),
      menuItem(
        "WGCNA",
        tabName = "wgcna",
        icon = icon("code-branch")
      ),
      menuItem(
        "Export",
        tabName = "export",
        icon = icon("download")
      ),
      menuItem(
        "About",
        tabName = "about",
        icon = icon("info-circle")
      ),
      img(src = "lumcLogo.png", width = "200px")
    )
  })
  
  
  source("tabs/home/svr_home.R", local = TRUE)
  source("tabs/upload/svr_upload.R", local = TRUE)
  source("tabs/run_analysis/svr_run.R", local = TRUE)
  source("tabs/alignment/svr_alignment.R", local = TRUE)
  source("tabs/raw_data/svr_raw.R", local = TRUE)
  source("tabs/norm_data/svr_norm.R", local = TRUE)
  source("tabs/pca/svr_pca.R", local = TRUE)
  source("tabs/heatmaps/svr_heatmaps.R", local = TRUE)
  source("tabs/analysis/svr_analysis.R", local = TRUE)
  source("tabs/bias/svr_bias.R", local = TRUE)
  source("tabs/run_gprofiler/svr_enrich.R", local = TRUE)
  source("tabs/gprofiler/svr_gprofiler.R", local = TRUE)
  source("tabs/wgcna/svr_wgcna.R", local = TRUE)
  source("tabs/export/svr_export.R", local = TRUE)
  source("tabs/about/svr_about.R", local = TRUE)
  
}
