
tab_export <- tabItem(
  tabName = "export",
  align="center",
  br(),
  br(),
  br(),
  h2("Download tables"),
  selectInput("dataset_select", "",
              c("Normalized counts" = "normCounts",
                "Full DE table" = "deTab",
                "DE genes only" = "deg",
                "PC values per gene" = "pcGene",
                "KEGG pathways" = "kegg",
                "Reactome pathways" = "reactome",
                "GO pathways" = "go",
                "DO pathways" = "do")
  ),
  downloadButton("downloadCSV", label = "Download as CSV"),
  downloadButton("downloadTSV", label = "Download as TSV"),
  br(),
  br(),
  br(),
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  h2("Download R Markdown analysis"),
  downloadButton("downloadMHTML", label = "Download as HTML")
)
