
tab_alignment <- tabItem(
  tabName = "alignment",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Summary",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("align_sum", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Summary %",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("align_sum_perc", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Complexity",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("complex", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Complexity %",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("complex_perc", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)