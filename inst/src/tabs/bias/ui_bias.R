
tab_bias <- tabItem(
  tabName = "bias",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "GC bias",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("selectGC"),
          br(),
          uiOutput("gc_bias_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("gc_bias", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_biasgc") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Feature length bias",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("selectLength"),
          br(),
          uiOutput("len_bias_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("len_bias", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_biaslength") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Gene strand",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          br(),
          uiOutput("geneStrand_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("geneStrand_bias", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
