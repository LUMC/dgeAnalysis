
tab_export <- tabItem(
  tabName = "export",
  align = "center",
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  br(),
  h2("Download tables"),
  selectInput(
    inputId = "dataset_select",
    label = "",
    choices = c(
      "Normalized counts" = "normCounts",
      "Log2 Normalized counts" = "normCountslog2",
      "Full DE table" = "deTab",
      "DE genes only" = "deg",
      "PC values per gene (PCA)" = "pcGene",
      "Filtered DE table (enrichment)" = "filtered",
      "gProfiler2 pathways" = "enrichment"
    )
  ), 
  downloadButton("downloadCSV", label = "Download as CSV"),
  downloadButton("downloadTSV", label = "Download as TSV"),
  br(),
  br(),
  br(),
  br(),
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  br(),
  h2("Download R Markdown analysis"),
  downloadButton("downloadDGE_HTML", label = "Download dge report"),
  br(),
  br(),
  downloadButton("downloadENRICH_HTML", label = "Download enrichment report"),
  br(),
  br(),
  br(),
  br(),
  HTML('<hr style="border-color: #0088cc;">')
)
