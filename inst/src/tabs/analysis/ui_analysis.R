
tab_analysis <- tabItem(
  tabName = "analysis",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "DE table",
      HTML('<hr style="border-color: #0088cc;">'),
      radioButtons(
        inputId = "setdeTab",
        label = "Show selection:",
        inline = TRUE,
        choices = c("All genes" = "all",
                    "DE genes" = "deg")
      ), 
      DT::dataTableOutput("detab_table") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "DE ratio",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("de_ratio_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("de_ratio", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "MA",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("ma_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("ma_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_ma") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Volcano",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("volcano_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("volcano_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">'),
      DT::dataTableOutput("selected_volcano") %>% withSpinner(),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Barcode",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("group_analysis_bar"),
          selectInput(
            inputId = "selected_analysis_bar",
            label = "Add specific genes:",
            multiple = TRUE,
            choices = c("Click to add gene" = ""),
            selected = 1
          ),
          sliderInput(
            inputId = "slider_barcode",
            label = "Set the number of genes to show:",
            value = 10,
            min = 1,
            max = 50,
            step = 1
          ),
          br(),
          uiOutput("barcode_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("barcode_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "P-Value",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          uiOutput("p_val_plot_info"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("p_val_plot", height = "600px") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
