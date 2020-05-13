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
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_sample_tree", height = "600px") %>% withSpinner()
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
          sliderInput("power_cutoff", "R^2 cutoff", 0.8, min = 0, max = 1, step = 0.01),
          sliderInput("power_numberOf", "Number of powers", 25, min = 1, max = 50, step = 1),
          radioButtons(
            "setPowerTab",
            "Choose plot:",
            c("SFT index" = "power",
              "Connectivity" = "soft")
          ),
          br(),
          uiOutput("wgcna_power_info"),
          span(icon("copyright"), "LUMC - SASC", style="color: #e3e3e3;")
        ), 
        mainPanel(
          width = 9,
          plotlyOutput("wgcna_power_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)