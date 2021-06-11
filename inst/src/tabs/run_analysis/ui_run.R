
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
        inputId = "analysis_method",
        label = "Choose analysis method:",
        inline = TRUE,
        selected = "analysisEdgeR",
        choices = c(
          "Limma/Voom" = "analysisLimma",
          "EdgeR" = "analysisEdgeR",
          "DESeq2" = "analysisDESeq2"
        )
      ),
      br(),
      sliderInput(
        inputId = "alpha_value",
        label = "Set FDR cutoff (adjusted P-Value):",
        value = 0.05,
        min = 0.01,
        max = 1,
        step = 0.01
      ),
      sliderInput(
        inputId = "cpm_value",
        label = "Set Log2CPM cutoff:",
        value = 1,
        min = 0,
        max = 10,
        step = 0.1
      ),
      sliderInput(
        inputId = "cpm_perc",
        label = "Set how many samples should meet CPM cutoff:",
        value = 25,
        min = 0,
        max = 100,
        step = 1,
        post = "%"
      )
    ),
    column(
      width = 6,
      br(), br(), br(),
      uiOutput("design_base"),
      radioButtons(
        inputId = "design_type",
        label = "",
        inline = TRUE,
        choices = c("Basic design" = "basic",
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
    h4("Read this comparison as:"),
    div(br(),
        uiOutput("show_matrix"),
        br(),
        style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%")
  )),
  HTML('<hr style="border-color: #0088cc;">'),
  
  uiOutput("excludeSamples"),
  uiOutput("setGeneName"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  actionButton(inputId = "run_button",
               label = "Run Analysis"), 
  
  br()
)
