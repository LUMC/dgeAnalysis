
tab_alignment <- tabItem(
  tabName = "alignment",
  align = "center",
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
            inputId =  "setSummary",
            label = "Set distribution:",
            choices = c("Actual" = "actual",
              "Percentage" = "percent"),
            selected = "percent"
          ),
          uiOutput("group_sum"),
          br(),
          uiOutput("align_sum_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
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
            inputId = "setComplexity",
            label = "Set distribution:",
            choices = c("Actual" = "value",
              "Percentage" = "fraction"),
            selected = "fraction"
          ),
          uiOutput("group_color"),
          sliderInput(
            inputId = "comp_rank",
            label = "Set number of genes:",
            value = 1000,
            min = 10,
            max = 10000,
            step = 10
          ),
          br(),
          uiOutput("complex_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
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
