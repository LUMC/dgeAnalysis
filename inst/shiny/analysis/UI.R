
tab_analysis <- tabItem(
  tabName = "analysis",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("DE table",
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
    tabPanel("DE ratio",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("de_ratio", height = "600px") %>% withSpinner()
    ),
    tabPanel("MA",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("ma_plot", height = "600px") %>% withSpinner()
    ),
    tabPanel("Volcano",
             HTML('<hr style="border-color: #0088cc;">'),
             numericInput("vulcanoLogCut", "LogFC Cutoff", 1, min = 0, max = 25, step=0.01),
             numericInput("vulcanoPCut", "P-Value Cutoff", 0.05, min = 0.001, max = 1, step=0.001),
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("volcano_plot", height = "600px") %>% withSpinner()
    ),
    tabPanel("Barcode",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("barcode_plot", height = "600px") %>% withSpinner()
    ),
    tabPanel("P-Value",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("p_val_plot", height = "600px") %>% withSpinner()
    )
  ),
  br(),
  HTML('<hr style="border-color: #0088cc;">'),
  DT::dataTableOutput("selected_clicked_plots") %>% withSpinner(),
  HTML('<hr style="border-color: #0088cc;">')
)