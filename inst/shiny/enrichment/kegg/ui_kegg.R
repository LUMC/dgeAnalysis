
tab_enrich_kegg <- tabItem(
  tabName = "enrich_kegg",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Enrichment table",
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("kegg_data_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Enriched terms barplot",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("bar_kegg_slider"),
             selectInput("bar_kegg_value", "Create plot with:",
                         selected = "pvalue",
                         c("P-Value" = "pvalue",
                           "Adjusted P-Value" = "p.adjust",
                           "Q-Value" = "qvalues")
             ),
             plotlyOutput("kegg_barplot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Gene-concept network",
             HTML('<hr style="border-color: #0088cc;">'),
             sliderInput("cnet_kegg_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
             plotlyOutput("cnet_kegg_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("cnet_kegg_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathway network",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("gsea_kegg_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("kegg_pathway", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("kegg_pathway_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathways from KEGG",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("select_kegg_pathway"),
             htmlOutput("pathway_from_kegg") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)