
tab_bias <- tabItem(
  tabName = "bias",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("GC bias",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("gc_bias", height = "600px") %>% withSpinner(),
             uiOutput("selectGC"),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Feature length bias",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("len_bias", height = "600px") %>% withSpinner(),
             uiOutput("selectLength"),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)