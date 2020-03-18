
tab_alignment <- tabItem(
  tabName = "alignment",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Summary",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          radioButtons(
            "setSummary",
            "Set distribution:",
            c("Actual" = "actual",
              "Percentage" = "percent")
          ),
          br(),
          uiOutput("align_sum_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("align_sum", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Complexity",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          radioButtons(
            "setComplexity",
            "Set distribution:",
            c("Actual" = "actual",
              "Percentage" = "percent")
          ),
          sliderInput("comp_rank", "Set number of ranks (Genes):", value = 1000,  min = 10,  max = 10000, step=10),
          br(),
          uiOutput("complex_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("complex", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)