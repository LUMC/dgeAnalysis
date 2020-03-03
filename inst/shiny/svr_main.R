
#suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))

server <- function(input, output, session) {
  
  checkReload <- function(){
    is.null(input$design_value)
    is.null(input$matrix_value)
    is.null(input$cpm_value)
    is.null(input$alpha_value)
    is.null(input$setGeneName)
    is.null(input$analysis_method)
    is.null(input$sample_data_rows_selected)
    is.null(input$choose_analysis)
    is.null(input$run_button)
  }

  output[["sidebar_tabs"]] <- renderMenu({
    sidebarMenu(id = "sidebar",
      menuItem("Home", tabName = "home",icon = icon("home")),
      menuItem("Data upload", tabName = "upload", icon = icon("upload")),
      menuItem("Run Analysis", tabName = "run_analysis", icon = icon("calculator")),
      menuItem("Pre-Analysis", icon = icon("clipboard-check"),
               menuItem("Alignment", tabName = "alignment", icon = icon("chart-line")),
               menuItem("Raw Data", tabName = "raw_data", icon = icon("chart-line"))
      ),
      menuItem("Analysis", icon = icon("binoculars"),
               menuItem("Normalization", tabName = "norm_data", icon = icon("chart-line")),
               menuItem("PCA", tabName = "pca", icon = icon("chart-line")),
               menuItem("Heatmaps", tabName = "heatmaps", icon = icon("chart-line")),
               menuItem("DE analysis", tabName = "analysis", icon = icon("chart-line")),
               menuItem("Bias", tabName = "bias", icon = icon("chart-line"))
      ),
      menuItem("Enrichment", icon = icon("bezier-curve"),
               menuSubItem("KEGG", tabName = "enrich_kegg", icon = icon("bezier-curve")),
               menuSubItem("Reactome", tabName = "enrich_reactome", icon = icon("bezier-curve")),
               menuSubItem("Gene Ontology", tabName = "enrich_go", icon = icon("bezier-curve")),
               menuSubItem("Disease Ontology", tabName = "enrich_do", icon = icon("bezier-curve"))
      ),
      menuItem("Export", tabName = "export", icon = icon("download")),
      img(src="lumcLogo.png", width="200px")
    )
  })

  
  source("shiny/home/svr_home.R", local = TRUE)
  source("shiny/upload/svr_upload.R", local = TRUE)
  source("shiny/run_analysis/svr_run.R", local = TRUE)
  source("shiny/alignment/svr_alignment.R", local = TRUE)
  source("shiny/raw_data/svr_raw.R", local = TRUE)
  source("shiny/norm_data/svr_norm.R", local = TRUE)
  source("shiny/pca/svr_pca.R", local = TRUE)
  source("shiny/heatmaps/svr_heatmaps.R", local = TRUE)
  source("shiny/analysis/svr_analysis.R", local = TRUE)
  source("shiny/bias/svr_bias.R", local = TRUE)
  source("shiny/enrichment/kegg/svr_kegg.R", local = TRUE)
  source("shiny/enrichment/reactome/svr_reactome.R", local = TRUE)
  source("shiny/enrichment/go/svr_go.R", local = TRUE)
  source("shiny/enrichment/do/svr_do.R", local = TRUE)
  source("shiny/export/svr_export.R", local = TRUE)

}
