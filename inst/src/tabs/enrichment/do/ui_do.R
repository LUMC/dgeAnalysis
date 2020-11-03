
tab_enrich_do <- tabItem(
  tabName = "enrich_do",
  align = "center",
  br(),
  
  radioButtons(
    inputId = "choose_do",
    label = "Select enrichment type:",
    inline = TRUE,
    choices = c(
      "Gene set enrichment" = "gse",
      "Over represented enrichment" = "enrich"
    )
  ), 
  
  tabsetPanel(
    tabPanel(
      title = "Enrichment table",
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("do_data_table") %>% withSpinner(),
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
            choices = c(
              "P-Value" = "pvalue",
              "Adjusted P-Value" = "p.adjust",
              "Q-Value" = "qvalues"
            )
          ), 
          br(),
          uiOutput("do_barplot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
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
          sliderInput(
            inputId = "cnet_do_slider",
            label = "Amount of shown pathways:",
            value = 5,
            min = 0,
            max = 15,
            step = 1
          ), 
          uiOutput("cnet_do_select_pathway"),
          tags$b("Labels on/off:"),
          checkboxInput(inputId = "cnet_do_annoP",
                        label = "Pathway labels",
                        value = TRUE),
          checkboxInput(inputId = "cnet_do_annoG",
                        label = "Gene labels",
                        value = FALSE), 
          br(),
          uiOutput("cnet_do_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("cnet_do_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("cnet_do_table") %>% withSpinner(),
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
            inputId = "heat_do_slider",
            label = "Number of shown pathways:",
            value = 5,
            min = 0,
            max = 15,
            step = 1
          ),
          uiOutput("heat_do_select_pathway"),
          br(),
          uiOutput("heat_do_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("heat_do_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("heat_do_table") %>% withSpinner(),
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
          checkboxInput(inputId = "do_network_annoP",
                        label = "Pathway labels",
                        value = FALSE), 
          br(),
          uiOutput("do_network_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("gsea_do_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Pathways from RGD",
      HTML('<hr style="border-color: #0088cc;">'),
      uiOutput("select_do_pathway", align = "center"),
      htmlOutput("pathway_from_do", align = "center") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
