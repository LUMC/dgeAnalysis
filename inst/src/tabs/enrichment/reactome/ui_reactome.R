
tab_enrich_reactome <- tabItem(
  tabName = "enrich_reactome",
  align="center",
  br(),
  
  radioButtons("choose_reactome", "Select enrichment type:",
               inline = TRUE,
               c("Gene set enrichment" = "gse",
                 "Over represented enrichment" = "enrich")
  ),
  
  tabsetPanel(
    tabPanel(
      title = "Enrichment table",
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("reactome_data_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Enriched terms barplot",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("bar_reactome_slider"),
          selectInput(
            inputId = "bar_reactome_value",
            label = "Color plot with:",
            selected = "pvalue",
            c("P-Value" = "pvalue",
              "Adjusted P-Value" = "p.adjust",
              "Q-Value" = "qvalues")
          ),
          br(),
          uiOutput("reactome_barplot_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("reactome_barplot", height = "600px") %>% withSpinner()
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
          sliderInput("cnet_reactome_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
          br(),
          uiOutput("cnet_reactome_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("cnet_reactome_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("cnet_reactome_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathway network",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("reactome_network_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("gsea_reactome_plot", height = "600px") %>% withSpinner(),
          HTML('<hr style="border-color: #0088cc;">'),
          plotlyOutput("reactome_pathway", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("reactome_pathway_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathways from Reactome",
      HTML('<hr style="border-color: #0088cc;">'),
      uiOutput("select_reactome_pathway"),
      htmlOutput("pathway_from_reactome") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
