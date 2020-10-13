
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
          width = 3,
          uiOutput("heatmap_var_ngenes"),
          uiOutput("group_var"),
          br(),
          uiOutput("var_heat_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
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
          width = 3,
          uiOutput("heatmap_dge_ngenes"),
          uiOutput("group_dge"),
          br(),
          uiOutput("dge_heat_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
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
