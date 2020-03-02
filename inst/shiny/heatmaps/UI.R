
tab_heatmaps <- tabItem(
  tabName = "heatmaps",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("100 most variable genes",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("var_heat", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("100 most DE genes",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("dge_heat", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)