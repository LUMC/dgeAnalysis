
tab_pca <- tabItem(
  tabName = "pca",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "PCA variance",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("variance_pca_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("variance_pca", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Sample PCA 2D",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_pca2d"),
          uiOutput("setpc_pca2d"),
          br(),
          uiOutput("samples_pca_2d_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("samples_pca_2d", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_pca") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Sample PCA 3D",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_pca3d"),
          uiOutput("setpc_pca3d"),
          br(),
          uiOutput("samples_pca_3d_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("samples_pca_3d", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
