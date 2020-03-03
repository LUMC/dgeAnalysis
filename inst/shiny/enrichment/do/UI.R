
tab_enrich_do <- tabItem(
  tabName = "enrich_do",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Enrichment table",
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("do_data_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Enriched terms barplot",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("bar_do_slider"),
             selectInput("bar_do_value", "Create plot with:",
                         selected = "pvalue",
                         c("P-Value" = "pvalue",
                           "Adjusted P-Value" = "p.adjust",
                           "Q-Value" = "qvalues")
             ),
             plotlyOutput("do_barplot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Gene-concept network",
             HTML('<hr style="border-color: #0088cc;">'),
             sliderInput("cnet_do_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
             plotlyOutput("cnet_do_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("cnet_do_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathway network",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("gsea_do_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathways from RGD",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("select_do_pathway", align="center"),
             htmlOutput("pathway_from_do", align="center") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
