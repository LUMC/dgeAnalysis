
tab_norm_data <- tabItem(
  tabName = "norm_data",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Normalized Counts",
      HTML('<hr style="border-color: #0088cc;">'),
      fluidRow(column(
        12,
        DT::dataTableOutput("normalized_counts") %>% withSpinner()
      )),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Count distribution",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("norm_line_color"),
          uiOutput("norm_dist_line_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("norm_dist_line", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Boxplot distribution",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("norm_dist_boxplot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("norm_dist_boxplot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Voom",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("norm_voom_ngenes"),
          br(),
          uiOutput("norm_voom_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("norm_voom_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_norm_voom") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
