tab_wgcna <- tabItem(
  tabName = "wgcna",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "Sample Tree",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("color_wgcna_tree"),
          br(),
          uiOutput("wgcna_tree_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_sample_tree", height = "600px") %>% withSpinner()
        )
      )
    ),
    
    tabPanel(
      title = "Trait heatmap",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          br(),
          uiOutput("wgcna_trait_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_trait_heat", height = "600px") %>% withSpinner()
        )
      )
    ),
    
    tabPanel(
      title = "Power",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          sliderInput(
            "power_cutoff",
            "R^2 cutoff",
            0.8,
            min = 0,
            max = 1,
            step = 0.01
          ),
          sliderInput(
            "power_numberOf",
            "Number of powers",
            25,
            min = 1,
            max = 50,
            step = 1
          ),
          radioButtons(
            "setPowerTab",
            "Choose plot:",
            c("SFT index" = "power",
              "Connectivity" = "soft")
          ),
          br(),
          uiOutput("wgcna_power_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_power_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Dendrogram module",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          sliderInput(
            "module_power",
            "Number of powers",
            6,
            min = 1,
            max = 50,
            step = 1
          ),
          uiOutput("wgcna_number_genes"),
          br(),
          uiOutput("wgcna_module_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_dendro_gene_module", height = "600px") %>% withSpinner(),
          plotlyOutput("wgcna_dendro_module", height = "600px") %>% withSpinner()
        )
      )
    ),
    
    tabPanel(
      title = "Module - Trait relation",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          br(),
          uiOutput("wgcna_module_trait_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_module_trait", height = "600px") %>% withSpinner()
        )
      )
    ),
    
    tabPanel(
      title = "Network heatmap",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          br(),
          uiOutput("wgcna_network_heat_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_network_heat", height = "600px") %>% withSpinner()
        )
      )
    )
  )
)
