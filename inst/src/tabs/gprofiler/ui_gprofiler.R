
tab_gprofiler <- tabItem(
  tabName = "gprofiler",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Enrichment table",
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("enrich_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "gProfiler2",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          br(),
          uiOutput("gProfiler2_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("enrich_gprofiler", height = "600px") %>% withSpinner()
        )
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
          sliderInput(
            inputId = "terms_slider",
            label = "Amount of shown pathways:",
            value = 25,
            min = 1,
            max = 50,
            step = 1
          ),
          br(),
          uiOutput("enrich_barplot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("enrich_barplot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "DE genes in terms",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          sliderInput(
            inputId = "DEterms_slider",
            label = "Amount of shown pathways:",
            value = 25,
            min = 1,
            max = 50,
            step = 1
          ),
          br(),
          uiOutput("enrich_DEbarplot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("enrich_DEbarplot", height = "600px") %>% withSpinner()
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
            "cnet_slider",
            "Amount of shown pathways:",
            value = 5,
            min = 0,
            max = 15,
            step = 1
          ),
          uiOutput("cnet_select_pathway"),
          tags$b("Labels on/off:"),
          checkboxInput("cnet_annoP", "Pathway labels", value = TRUE),
          checkboxInput("cnet_annoG", "Gene labels", value = FALSE),
          br(),
          uiOutput("enrich_cnet_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("cnet_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("cnet_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)