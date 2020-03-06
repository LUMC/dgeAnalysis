
tab_export <- tabItem(
  tabName = "export",
  align="center",
  br(),
  
  #h2("Bookmark current state of application"),
  #bookmarkButton(),
  #br(),
  #br(),
  br(),
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  h2("Download tables"),
  selectInput("dataset_select", "",
              c("Normalized counts" = "normCounts",
                "Full DE table" = "deTab",
                "DE genes only" = "deg",
                "KEGG pathways" = "kegg",
                "Reactome pathways" = "reactome",
                "GO pathways" = "go",
                "DO pathways" = "do")
  ),
  downloadButton("downloadCSV", label = "Download as CSV"),
  downloadButton("downloadTSV", label = "Download as TSV"),
  downloadButton("downloadXLSX", label = "Download as XLSX"),
  br(),
  br(),
  br(),
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  h2("Download R Markdown analysis"),
  downloadButton("downloadMHTML", label = "Download as HTML")
)