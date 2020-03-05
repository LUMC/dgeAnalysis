
tab_enrich_do <- tabItem(
  tabName = "enrich_do",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Enrichment table",
      HTML('<hr style="border-color: #0088cc;">'),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("do_data_table") %>% withSpinner())
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Enriched terms barplot",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("bar_do_slider"),
          selectInput(
            inputId = "bar_do_value",
            label = "Color plot with:",
            selected = "pvalue",
            c("P-Value" = "pvalue",
              "Adjusted P-Value" = "p.adjust",
              "Q-Value" = "qvalues")
          )
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("do_barplot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Gene-concept network",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          sliderInput("cnet_do_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1)
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("cnet_do_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("cnet_do_table") %>% withSpinner())
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathway network",
      HTML('<hr style="border-color: #0088cc;">'),
      plotlyOutput("gsea_do_plot", height = "600px") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathways from RGD",
      HTML('<hr style="border-color: #0088cc;">'),
      uiOutput("select_do_pathway", align="center"),
      htmlOutput("pathway_from_do", align="center") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
