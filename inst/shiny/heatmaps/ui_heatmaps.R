
tab_heatmaps <- tabItem(
  tabName = "heatmaps",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Most variable genes",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("var_heat", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Most DE genes",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("dge_heat", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)