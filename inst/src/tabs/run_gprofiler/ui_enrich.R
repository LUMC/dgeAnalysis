
tab_run_gprofiler <- tabItem(
  tabName = "run_gprofiler",
  align = "center",
  br(),
  
  tabsetPanel(
    tabPanel(
      title = "gProfiler2",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 6,
          box(
            title = "Select database",
            status = "primary",
            solidHeader = TRUE,
            width = 10,
            collapsible = TRUE,
            collapsed = TRUE,
            checkboxGroupInput(
              "source_go",
              "Gene Ontology",
              c(
                "GO molecular function" = "GO:MF",
                "GO cellular component" = "GO:CC",
                "GO biological process" = "GO:BP"
              ),
              selected = c("GO:MF", "GO:CC", "GO:BP")
            ),
            checkboxGroupInput(
              "source_bp",
              "Gene Ontology",
              c(
                "KEGG" = "KEGG",
                "Reactome" = "REAC",
                "WikiPathways" = "WP"
              ),
              selected = c("KEGG", "REAC", "WP")
            ),
            checkboxGroupInput(
              "source_hp",
              "Human phenotype ontology",
              c(
                "HP" = "HP"
              ),
              selected = "HP"
            )
          ),
          sliderInput(
            inputId = "enrich_pvalue",
            label = "Set FDR cutoff",
            value = 0.05,
            min = 0,
            max = 1,
          ),
          radioButtons(
            inputId = "enrich_sign",
            label = "Only get significant results:",
            inline = TRUE,
            choices = c(
              "True" = "TRUE",
              "False" = "FALSE"
            )
          ),
          uiOutput("enrich_organism"),
          br(),
          actionButton(inputId = "run_enrichment",
                       label = "Run Enrichment"),
          br(),
          br(),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 6,
          uiOutput("enrich_input")
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    ),
    
    tabPanel(
      title = "Filter DE table",
      HTML('<hr style="border-color: #0088cc;">'),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 3,
          h2("Available filters"),
          br(),
          radioButtons(
            inputId = "filter_exp",
            label = "Show selection:",
            inline = TRUE,
            choices = c("All genes" = "all",
                        "All DE genes" = "deg",
                        "Up regulated" = "up",
                        "Down regulated" = "down")
          ),
          sliderInput(
            inputId = "filter_pvalue",
            label = "P-value filter",
            value = 0.05,
            min = 0,
            max = 1,
          ),
          sliderInput(
            inputId = "filter_fdr",
            label = "FDR filter",
            value = 0.05,
            min = 0,
            max = 1,
          ),
          sliderInput(
            inputId = "filter_fc",
            label = "Log2FC filter",
            value = c(-2, 2),
            min = -10,
            max = 10,
            step = 0.1
          ),
          uiOutput("enrich_ngenes"),
          span(icon("copyright"), "LUMC - SASC", style = "color: #e3e3e3;")
        ),
        mainPanel(
          width = 9,
          DT::dataTableOutput("enrich_detab") %>% withSpinner()
        )
      ),
      HTML('<hr style="border-color: #0088cc;">')
    )
  )
)
