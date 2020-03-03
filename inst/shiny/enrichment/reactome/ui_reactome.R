
tab_enrich_reactome <- tabItem(
  tabName = "enrich_reactome",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Enrichment table",
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("reactome_data_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Enriched terms barplot",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("bar_reactome_slider"),
             selectInput("bar_reactome_value", "Create plot with:",
                         selected = "pvalue",
                         c("P-Value" = "pvalue",
                           "Adjusted P-Value" = "p.adjust",
                           "Q-Value" = "qvalues")
             ),
             plotlyOutput("reactome_barplot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Gene-concept network",
             HTML('<hr style="border-color: #0088cc;">'),
             sliderInput("cnet_reactome_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
             plotlyOutput("cnet_reactome_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("cnet_reactome_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathway network",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("gsea_reactome_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("reactome_pathway", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("reactome_pathway_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathways from Reactome",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("select_reactome_pathway"),
             htmlOutput("pathway_from_reactome") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)