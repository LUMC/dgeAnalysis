
tab_pca <- tabItem(
  tabName = "pca",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("PCA variance",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("variance_pca", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Sample PCA 2D",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("samples_pca_2d", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             DT::dataTableOutput("samples_pca_2d_clicked") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Sample PCA 3D",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("samples_pca_3d", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)