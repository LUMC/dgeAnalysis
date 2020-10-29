
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
      "KEGG pathways" = "kegg",
      "Reactome pathways" = "reactome",
      "GO pathways" = "go",
      "DO pathways" = "do"
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
  downloadButton("downloadMHTML", label = "Download as HTML"),
  br(),
  br(),
  br(),
  br(),
  HTML('<hr style="border-color: #0088cc;">')
)
