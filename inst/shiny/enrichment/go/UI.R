
tab_enrich_go <- tabItem(
  tabName = "enrich_go",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Enrichment table",
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("go_data_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Enriched terms barplot",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("bar_go_slider"),
             selectInput("bar_go_value", "Create plot with:",
                         selected = "pvalue",
                         c("P-Value" = "pvalue",
                           "Adjusted P-Value" = "p.adjust",
                           "Q-Value" = "qvalues")
             ),
             plotlyOutput("go_barplot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Gene-concept network",
             HTML('<hr style="border-color: #0088cc;">'),
             sliderInput("cnet_go_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
             plotlyOutput("cnet_go_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">'),
             fluidRow(
               column(
                 12,
                 DT::dataTableOutput("cnet_go_table") %>% withSpinner())
             ),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathway network",
             HTML('<hr style="border-color: #0088cc;">'),
             plotlyOutput("gsea_go_plot", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Pathways from QuickGO",
             HTML('<hr style="border-color: #0088cc;">'),
             uiOutput("select_go_pathway", align="center"),
             htmlOutput("pathway_from_go", align="center") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  ),
  radioButtons("selectOntology", "Selected a sub-ontology:",
               inline = TRUE,
               selected = "CC",
               c("Biological Process (BP)" = "BP",
                 "Cellular Component (CC)" = "CC",
                 "Molecular Function (MF)" = "MF")
  )
)