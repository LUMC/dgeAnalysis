
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

  
  source("shiny/home/server.R", local = TRUE)
  source("shiny/upload/server.R", local = TRUE)
  source("shiny/run_analysis/server.R", local = TRUE)
  source("shiny/alignment/server.R", local = TRUE)
  source("shiny/raw_data/server.R", local = TRUE)
  source("shiny/norm_data/server.R", local = TRUE)
  source("shiny/pca/server.R", local = TRUE)
  source("shiny/heatmaps/server.R", local = TRUE)
  source("shiny/analysis/server.R", local = TRUE)
  source("shiny/bias/server.R", local = TRUE)
  source("shiny/enrichment/kegg/server.R", local = TRUE)
  source("shiny/enrichment/reactome/server.R", local = TRUE)
  source("shiny/enrichment/go/server.R", local = TRUE)
  source("shiny/enrichment/do/server.R", local = TRUE)
  source("shiny/export/server.R", local = TRUE)

}
