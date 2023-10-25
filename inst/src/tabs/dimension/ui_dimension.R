
tab_dimension <- tabItem(
  tabName = "dimension",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "PCA",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_pca"),
          uiOutput("setpc_pca"),
          selectInput(
            inputId = "color_groups",
            label = "Select group for custom colorization",
            multiple = TRUE,
            choices = c("Click to add group" = ""),
            selected = 1
          ),
          uiOutput("color_picker"),
          br(),
          uiOutput("pca_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("pca", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_pca") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "PCA variance",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("variance_pca_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("variance_pca", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "t-SNE",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_dim_tsne"),
          br(),
          uiOutput("dim_tsne_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("dim_tsne", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_dim_tsne") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "MDS",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_norm_mds"),
          br(),
          uiOutput("norm_un_cluster_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("norm_un_cluster", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_norm_mds") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Dendrogram",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("color_dendro"),
          br(),
          uiOutput("dendro_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("dim_dendro", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)

