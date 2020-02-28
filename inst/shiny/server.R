
server <- function(input, output, session) {
  output[["sidebar_tabs"]] <- renderMenu({
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Tables", icon = icon("table"), 
               menuSubItem("Input tables", tabName = "input_tables", icon = icon("table")),
               menuSubItem("Analysis tables", tabName = "analysis_tables", icon = icon("table"))),
      menuItem("Plots", icon = icon("line-chart"),
               menuSubItem("Alignment", tabName = "alignment_plots", icon = icon("line-chart")),
               menuSubItem("Raw data", tabName = "raw_plots", icon = icon("line-chart")),
               menuSubItem("Normalization", tabName = "normalization_plots", icon = icon("line-chart")),
               menuSubItem("PCA", tabName = "pca_plots", icon = icon("line-chart")),
               menuSubItem("Heatmaps", tabName = "heatmaps", icon = icon("line-chart")),
               menuSubItem("Analysis", tabName = "analysis_plots", icon = icon("line-chart")),
               menuSubItem("Bias", tabName = "bias_plots", icon = icon("line-chart"))),
      menuItem("Enrichment", icon = icon("bezier-curve"),
               menuSubItem("KEGG", tabName = "gsea_kegg", icon = icon("bezier-curve")),
               menuSubItem("Reactome", tabName = "gsea_reactome", icon = icon("bezier-curve")),
               menuSubItem("Gene Ontology", tabName = "gsea_go", icon = icon("bezier-curve")),
               menuSubItem("Disease Ontology", tabName = "gsea_do", icon = icon("bezier-curve"))),
      menuItem("Export", tabName = "data_export", icon = icon("file-export")),
      img(src='../www/lumcLogo.png', width="200px")
    )
  })
}

#source(system.file("shiny/home/server.R", package = "dgeAnalysis"), local = TRUE)
#source(system.file("shiny/data_upload/server.R", package = "dgeAnalysis"), local = TRUE)
source("home/server.R")
source("data_upload/server.R")
