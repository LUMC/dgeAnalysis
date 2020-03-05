
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
          uiOutput("selectGC")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("gc_bias", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Feature length bias",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("selectLength")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("len_bias", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)