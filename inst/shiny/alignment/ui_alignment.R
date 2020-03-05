
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
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("align_sum", height = "600px") %>% withSpinner(),
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
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("complex", height = "600px") %>% withSpinner(),
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)