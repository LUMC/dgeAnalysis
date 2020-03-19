

tab_enrich_go <- tabItem(
  tabName = "enrich_go",
  align = "center",
  br(),
  
  radioButtons("choose_go", "Select enrichment type:",
               inline = TRUE,
               c("Gene set enrichment" = "gse",
                 "Over represented enrichment" = "enrich")
  ),
  
  radioButtons(
    inputId = "selectOntology",
    label = "Selected a sub-ontology:",
    inline = TRUE,
    selected = "CC",
    c(
      "Biological Process (BP)" = "BP",
      "Cellular Component (CC)" = "CC",
      "Molecular Function (MF)" = "MF"
    )
  ),
  
  tabsetPanel(
    tabPanel(
      title = "Enrichment table",
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("go_data_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Enriched terms barplot",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("bar_go_slider"),
          selectInput(
            inputId = "bar_go_value",
            label = "Color plot with:",
            selected = "pvalue",
            c("P-Value" = "pvalue",
              "Adjusted P-Value" = "p.adjust",
              "Q-Value" = "qvalues")
          ),
          br(),
          uiOutput("go_barplot_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("go_barplot", height = "600px") %>% withSpinner()
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
          sliderInput("cnet_go_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step = 1),
          br(),
          uiOutput("cnet_go_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("cnet_go_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("cnet_go_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathway network",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("go_network_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("gsea_go_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathways from QuickGO",
      HTML('<hr style="border-color: #0088cc;">'),
      uiOutput("select_go_pathway", align = "center"),
      htmlOutput("pathway_from_go", align = "center") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
