
tab_enrich_kegg <- tabItem(
  tabName = "enrich_kegg",
  align = "center",
  br(),
  
  radioButtons(
    "choose_kegg",
    "Select enrichment type:",
    inline = TRUE,
    c(
      "Gene set enrichment" = "gse",
      "Over represented enrichment" = "enrich"
    )
  ),
  
  tabsetPanel(
    tabPanel(
      title = "Enrichment table",
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("kegg_data_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Enriched terms barplot",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("bar_kegg_slider"),
          selectInput(
            inputId = "bar_kegg_value",
            label = "Color plot with:",
            selected = "pvalue",
            c(
              "P-Value" = "pvalue",
              "Adjusted P-Value" = "p.adjust",
              "Q-Value" = "qvalues"
            )
          ),
          br(),
          uiOutput("kegg_barplot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("kegg_barplot", height = "600px") %>% withSpinner()
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
          sliderInput(
            "cnet_kegg_slider",
            "Number of shown pathways:",
            5,
            min = 0,
            max = 15,
            step = 1
          ),
          uiOutput("cnet_kegg_select_pathway"),
          tags$b("Labels on/off:"),
          checkboxInput("cnet_kegg_annoP", "Pathway labels", value = TRUE),
          checkboxInput("cnet_kegg_annoG", "Gene labels", value = FALSE),
          br(),
          uiOutput("cnet_kegg_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("cnet_kegg_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("cnet_kegg_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathway heatmap",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          sliderInput(
            "heat_kegg_slider",
            "Number of shown pathways:",
            5,
            min = 0,
            max = 15,
            step = 1
          ),
          uiOutput("heat_kegg_select_pathway"),
          br(),
          uiOutput("heat_kegg_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("heat_kegg_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("heat_kegg_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathway network",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          tags$b("Labels on/off:"),
          checkboxInput("kegg_network_annoP", "Pathway labels", value = FALSE),
          br(),
          uiOutput("kegg_network_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("gsea_kegg_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathways from KEGG",
      HTML('<hr style="border-color: #0088cc;">'),
      uiOutput("select_kegg_pathway"),
      htmlOutput("pathway_from_kegg") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
