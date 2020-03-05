
tab_run_analysis <- tabItem(
  tabName = "run_analysis",
  align="center",
  br(),
  
  h2("Settings"),
  HTML('<hr style="border-color: #0088cc;">'),
  
  fluidRow(
    column(
      width = 6,
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
      uiOutput("matrix_value")
    ),
    column(
      width = 6,
      sliderInput("alpha_value", "Set FDR cutoff (adjusted P-Value):", value = 0.05,  min = 0.01,  max = 1, step=0.01),
      sliderInput("cpm_value", "Set Log2CPM cutoff:", value = 1, min = 0,  max = 10, step=0.1)
    )
  ),
  HTML('<hr style="border-color: #0088cc;">'),
  
  uiOutput("excludeSamples"),
  uiOutput("setGeneName"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  actionButton(
    "run_button",
    "Run Analysis"
  ),
  
  br()
)
