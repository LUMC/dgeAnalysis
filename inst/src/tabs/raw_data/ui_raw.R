
tab_raw_data <- tabItem(
  tabName = "raw_data",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Count distribution",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("raw_line_color"),
          uiOutput("dist_line_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("dist_line", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Violin distribution",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("raw_violin_group"),
          uiOutput("dist_violin_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("dist_violin", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "MDS",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_raw_mds"),
          br(),
          uiOutput("un_cluster_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("un_cluster", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_raw_mds") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
