
tab_run_analysis <- tabItem(
  tabName = "run_analysis",
  align = "center",
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
        c(
          "Limma/Voom" = "analysisLimma",
          "EdgeR" = "analysisEdgeR",
          "DESeq2" = "analysisDESeq2"
        )
      ),
      br(),
      sliderInput(
        "alpha_value",
        "Set FDR cutoff (adjusted P-Value):",
        value = 0.05,
        min = 0.01,
        max = 1,
        step = 0.01
      ),
      sliderInput(
        "cpm_value",
        "Set Log2CPM cutoff:",
        value = 1,
        min = 0,
        max = 10,
        step = 0.1
      )
    ),
    column(
      width = 6,
      uiOutput("design_base"),
      radioButtons(
        "design_type",
        "",
        inline = TRUE,
        c("Basic design" = "basic",
          "Advanced design" = "advanced")
      ),
      uiOutput("design_value"),
      uiOutput("matrix")
    )
  ),
  br(),
  fluidRow(column(
    width = 6,
    h4("Current design in use:"),
    div(br(),
        uiOutput("show_design"),
        br(),
        style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%")
  ),
  column(
    width = 6,
    h4("Find genes that respond to:"),
    div(br(),
        uiOutput("show_matrix"),
        br(),
        style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%")
  )),
  HTML('<hr style="border-color: #0088cc;">'),
  
  uiOutput("excludeSamples"),
  uiOutput("setGeneName"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  actionButton("run_button",
               "Run Analysis"),
  
  br()
)
