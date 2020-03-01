
tab_upload <- tabItem(
  tabName = "upload",
  align="center",
  br(),
  
  div(
    style="float: left; width:50%;",
    radioButtons(
      "appMode",
      "",
      inline = TRUE,
      selected = "new",
      c("New Analysis" = "new")
    ),
    br(),
    fileInput("file_samples", "Choose your samples (metadata) file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
    fileInput("file_counts", "Choose your raw counts file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
    fileInput("file_annotation", "Choose your annotation file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt"))
  ),
  
  div(
    style="float: left; width:50%;",
    radioButtons(
      "appMode",
      "",
      inline = TRUE,
      c("View Analysis" = "view")
    ),
    br(),
    fileInput("file_samples_view", "Choose your samples (metadata) file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
    fileInput("file_counts_view", "Choose your raw counts file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
    fileInput("file_norm_view", "Choose your normalized counts file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
    fileInput("file_detab_view", "Choose your DE file:",
              multiple = FALSE,
              accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt"))
  )
)