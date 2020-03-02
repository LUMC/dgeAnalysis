
tab_raw_data <- tabItem(
  tabName = "raw_data",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Line distribution",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("dist_line", height = "600px") %>% withSpinner()
    ),
    tabPanel("Boxplot distribution",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("dist_boxplot", height = "600px") %>% withSpinner()
    ),
    tabPanel("Multidimensional scaling 2D",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("un_cluster_2d", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             DT::dataTableOutput("un_cluster_2d_clicked") %>% withSpinner()
    ),
    tabPanel("Multidimensional scaling 3D",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("un_cluster_3d", height = "600px") %>% withSpinner()
    )
  )
)