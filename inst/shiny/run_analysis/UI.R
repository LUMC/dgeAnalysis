
tab_run_analysis <- tabItem(
  tabName = "run_analysis",
  align="center",
  br(),
  
  h2("Settings"),
  
  radioButtons(
    "analysis_method",
    "Choose analysis method:",
    inline = TRUE,
    selected = "analysisEdgeR",
    c("Limma/Voom" = "analysisLimma",
      "EdgeR" = "analysisEdgeR",
      "DESeq2" = "analysisDESeq2")
  ),
  
  uiOutput("design_value"),
  uiOutput("matrix_value"),
  
  sliderInput(
    "alpha_value",
    "Set alpha (P-Value cutoff):",
    value = 0.05,
    min = 0.01,
    max = 1,
    step=0.01
  ),
  
  sliderInput(
    "cpm_value",
    "Set Log2CPM cutoff:",
    value = 1,
    min = 0,
    max = 10,
    step=0.1
  ),
  
  uiOutput("setGeneName"),
  
  br(),
  
  actionButton(
    "run_button",
    "Run Analysis"
  ),
  
  br()
)
