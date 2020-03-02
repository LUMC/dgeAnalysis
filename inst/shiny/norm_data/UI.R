
tab_norm_data <- tabItem(
  tabName = "norm_data",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Line distribution",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("norm_dist_line", height = "600px") %>% withSpinner()
    ),
    tabPanel("Boxplot distribution",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("norm_dist_boxplot", height = "600px") %>% withSpinner()
    ),
    tabPanel("Multidimensional scaling 2D",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("norm_un_cluster_2d", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             DT::dataTableOutput("norm_un_cluster_2d_clicked") %>% withSpinner()
    ),
    tabPanel("Multidimensional scaling 3D",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("norm_un_cluster_3d", height = "600px") %>% withSpinner()
    )
  )
)