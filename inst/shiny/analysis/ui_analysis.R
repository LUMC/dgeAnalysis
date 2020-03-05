
tab_analysis <- tabItem(
  tabName = "analysis",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "DE table",
      HTML('<hr style="border-color: #0088cc;">'),
      radioButtons(
        "setdeTab",
        "Show selection:",
        inline = TRUE,
        c("All genes" = "all",
          "DE genes" = "deg")
      ),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("detab_table") %>% withSpinner())
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "DE ratio",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("de_ratio", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "MA",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("ma_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_ma") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Volcano",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          numericInput("vulcanoLogCut", "LogFC Cutoff", 1, min = 0, max = 25, step=0.01),
          numericInput("vulcanoPCut", "P-Value Cutoff", 0.05, min = 0.001, max = 1, step=0.001),
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("volcano_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_volcano") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Barcode",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("barcode_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "P-Value",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("p_val_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
