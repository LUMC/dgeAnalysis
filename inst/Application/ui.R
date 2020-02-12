

## Front-end of Shiny program
enableBookmarking(store = "server")
ui <- function(request){
  dashboardPage(
    skin = "blue",
    title = "Shiny analysis",

    dashboardHeader(
      title = span(tagList(icon("dna"), "Give me a name")),
      titleWidth = 400,
      tags$li(class = "dropdown", tags$a(textOutput("analysis_state")))
    ),

    dashboardSidebar(
      collapsed = FALSE,
      width = 350,
      sidebarMenu(
        id = "sidebar",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data upload", tabName = "data_upload", icon = icon("upload")),
        menuItem("Tables", icon = icon("table"),
                 menuSubItem("Input tables", tabName = "input_tables", icon = icon("table")),
                 menuSubItem("Analysis tables", tabName = "analysis_tables", icon = icon("table"))),
        menuItem("Plots", icon = icon("line-chart"),
                 menuSubItem("Alignment", tabName = "alignment_plots", icon = icon("line-chart")),
                 menuSubItem("Raw data", tabName = "raw_plots", icon = icon("line-chart")),
                 menuSubItem("Normalization", tabName = "normalization_plots", icon = icon("line-chart")),
                 menuSubItem("PCA", tabName = "pca_plots", icon = icon("line-chart")),
                 menuSubItem("Heatmaps", tabName = "heatmaps", icon = icon("line-chart")),
                 menuSubItem("Analysis", tabName = "analysis_plots", icon = icon("line-chart")),
                 menuSubItem("Bias", tabName = "bias_plots", icon = icon("line-chart"))),
        menuItem("Enrichment", icon = icon("bezier-curve"),
                 menuSubItem("KEGG", tabName = "gsea_kegg", icon = icon("bezier-curve")),
                 menuSubItem("Reactome", tabName = "gsea_reactome", icon = icon("bezier-curve")),
                 menuSubItem("Gene Ontology", tabName = "gsea_go", icon = icon("bezier-curve")),
                 menuSubItem("Disease Ontology", tabName = "gsea_do", icon = icon("bezier-curve"))),
        menuItem("Export", tabName = "data_export", icon = icon("file-export")),
        img(src='lumcLogo.png', width="200px")
      )
    ),

    dashboardBody(
      useShinyjs(),
      includeCSS("css/styles.css"),
      tabItems(
        tabItem(
          "home",
          h1("Welcome")
        ),

        tabItem(
          "data_upload", align="center",
          br(),
          div(
            switchInput(
              "choose_analysis",
              value=TRUE,
              onLabel = "New analysis",
              offLabel = "View analysis",
              label = icon("grip-horizontal"),
              size = 'large',
              labelWidth = "250px",
              handleWidth = "500px",
              width = "750px"
            )
          ),

          div(
            h1("Choose your files"),
            # br(),
            fileInput("file_samples", "Choose your samples (metadata) file:",
                      multiple = FALSE,
                      accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
            fileInput("file_counts", "Choose your counts file:",
                      multiple = FALSE,
                      accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt"))
          ),

          div(id = "new_analysis_div",
              hidden=FALSE,
            uiOutput("new_analysis_input")
          ),
          div(id = "view_analysis_div",
            uiOutput("view_analysis_input")
          )
        ),

        tabItem(
          "input_tables", align="center",
          tabsetPanel(
            tabPanel("Samples",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("sample_data") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Counts",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("count_data") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Annotation",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("annotation_data") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Excluded samples",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("excluded_samples") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "analysis_tables", align="center",
          tabsetPanel(
            tabPanel("Normalized Counts",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("normalized_counts") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("All Genes",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("all_genes_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("DEG",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("deg_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "alignment_plots", align="center",
          tabsetPanel(
            tabPanel("Summary",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("align_sum", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Summary %",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("align_sum_perc", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Complexity",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("complex", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Complexity %",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("complex_perc", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "raw_plots", align="center",
          tabsetPanel(
            tabPanel("Line distribution",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("dist_line", height = "600px") %>% withSpinner()
            ),
            tabPanel("Boxplot distribution",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("dist_boxplot", height = "600px") %>% withSpinner()
            ),
            tabPanel("Multidimensional scaling 2D",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("un_cluster_2d", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     DT::dataTableOutput("un_cluster_2d_clicked") %>% withSpinner()
            ),
            tabPanel("Multidimensional scaling 3D",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("un_cluster_3d", height = "600px") %>% withSpinner()
            )
          ),
          br(),
          uiOutput("group_by1"),
          HTML('<hr style="border-color: #0088cc;">')
        ),

        tabItem(
          "normalization_plots", align="center",
          tabsetPanel(
            tabPanel("Line distribution",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("norm_dist_line", height = "600px") %>% withSpinner()
            ),
            tabPanel("Boxplot distribution",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("norm_dist_boxplot", height = "600px") %>% withSpinner()
            ),
            tabPanel("Multidimensional scaling 2D",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("norm_un_cluster_2d", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     DT::dataTableOutput("norm_un_cluster_2d_clicked") %>% withSpinner()
            ),
            tabPanel("Multidimensional scaling 3D",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("norm_un_cluster_3d", height = "600px") %>% withSpinner()
            )
          ),
          br(),
          uiOutput("group_by2"),
          HTML('<hr style="border-color: #0088cc;">')
        ),

        tabItem(
          "pca_plots", align="center",
          tabsetPanel(
            tabPanel("PCA variance",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("variance_pca", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Sample PCA 2D",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("samples_pca_2d", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     DT::dataTableOutput("samples_pca_2d_clicked") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Sample PCA 3D",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("samples_pca_3d", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Genes PCA 2D",
                     br(),
                     h1("Not in yet")
                     #plotlyOutput("genes_pca_2d", height = "600px") %>% withSpinner()
            ),
            tabPanel("Genes PCA 3D",
                     br(),
                     uiOutput("pca_feature"),
                     h1("Not in yet")
                     #plotlyOutput("genes_pca_3d", height = "600px") %>% withSpinner()
            )
          ),
          br(),
          uiOutput("group_by3")
        ),

        tabItem(
          "heatmaps", align="center",
          tabsetPanel(
            tabPanel("100 most variable genes",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("var_heat", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("100 most DE genes",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("dge_heat", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "analysis_plots", align="center",
          tabsetPanel(
            tabPanel("Voom",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("voom_plot", height = "600px") %>% withSpinner()
            ),
            tabPanel("DE ratio",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("de_ratio", height = "600px") %>% withSpinner()
            ),
            tabPanel("MA",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("ma_plot", height = "600px") %>% withSpinner()
            ),
            tabPanel("Volcano",
                     HTML('<hr style="border-color: #0088cc;">'),
                     numericInput("vulcanoLogCut", "LogFC Cutoff", 1, min = 0, max = 25, step=0.01),
                     numericInput("vulcanoPCut", "P-Value Cutoff", 0.05, min = 0.001, max = 1, step=0.001),
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("volcano_plot", height = "600px") %>% withSpinner()
            ),
            tabPanel("Barcode",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("barcode_plot", height = "600px") %>% withSpinner()
            ),
            tabPanel("P-Value",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("p_val_plot", height = "600px") %>% withSpinner()
            )
          ),
          br(),
          uiOutput("group_by4"),
          HTML('<hr style="border-color: #0088cc;">'),
          DT::dataTableOutput("selected_clicked_plots") %>% withSpinner(),
          HTML('<hr style="border-color: #0088cc;">')
        ),

        tabItem(
          "bias_plots", align="center",
          tabsetPanel(
            tabPanel("GC bias",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("gc_bias", height = "600px") %>% withSpinner(),
                     uiOutput("selectGC"),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Feature length bias",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("len_bias", height = "600px") %>% withSpinner(),
                     uiOutput("selectLength"),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "gsea_kegg", align="center",
          div(
            switchInput(
              "choose_kegg",
              value=TRUE,
              onLabel = "Over represented enrichment",
              offLabel = "Gene set enrichment",
              label = icon("grip-horizontal"),
              size = 'large',
              labelWidth = "250px",
              handleWidth = "500px",
              width = "750px"
            )
          ),
          tabsetPanel(
            tabPanel("Enrichment table",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("kegg_data_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Enriched terms barplot",
                     HTML('<hr style="border-color: #0088cc;">'),
                     uiOutput("bar_kegg_slider"),
                     selectInput("bar_kegg_value", "Create plot with:",
                                 selected = "pvalue",
                                 c("P-Value" = "pvalue",
                                   "Adjusted P-Value" = "p.adjust",
                                   "Q-Value" = "qvalues")
                     ),
                     plotlyOutput("kegg_barplot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Gene-concept network",
                     HTML('<hr style="border-color: #0088cc;">'),
                     sliderInput("cnet_kegg_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
                     plotlyOutput("cnet_kegg_plot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("cnet_kegg_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Pathway network",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("gsea_kegg_plot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("kegg_pathway", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("kegg_pathway_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Pathways from KEGG",
                     HTML('<hr style="border-color: #0088cc;">'),
                     uiOutput("select_kegg_pathway"),
                     htmlOutput("pathway_from_kegg") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "gsea_reactome", align="center",
          div(
            switchInput(
              "choose_reactome",
              value=TRUE,
              onLabel = "Over represented enrichment",
              offLabel = "Gene set enrichment",
              label = icon("grip-horizontal"),
              size = 'large',
              labelWidth = "250px",
              handleWidth = "500px",
              width = "750px"
            )
          ),
          tabsetPanel(
            tabPanel("Enrichment table",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("reactome_data_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Enriched terms barplot",
                     HTML('<hr style="border-color: #0088cc;">'),
                     uiOutput("bar_reactome_slider"),
                     selectInput("bar_reactome_value", "Create plot with:",
                                 selected = "pvalue",
                                 c("P-Value" = "pvalue",
                                   "Adjusted P-Value" = "p.adjust",
                                   "Q-Value" = "qvalues")
                     ),
                     plotlyOutput("reactome_barplot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Gene-concept network",
                     HTML('<hr style="border-color: #0088cc;">'),
                     sliderInput("cnet_reactome_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
                     plotlyOutput("cnet_reactome_plot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("cnet_reactome_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Pathway network",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("gsea_reactome_plot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("reactome_pathway", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("reactome_pathway_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Pathways from Reactome",
                     HTML('<hr style="border-color: #0088cc;">'),
                     uiOutput("select_reactome_pathway"),
                     htmlOutput("pathway_from_reactome") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "gsea_go", align="center",
          div(
            switchInput(
              "choose_go",
              value=TRUE,
              onLabel = "Over represented enrichment",
              offLabel = "Gene set enrichment",
              label = icon("grip-horizontal"),
              size = 'large',
              labelWidth = "250px",
              handleWidth = "500px",
              width = "750px"
            )
          ),
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
        ),

        tabItem(
          "gsea_do", align="center",
          div(
            switchInput(
              "choose_do",
              value=TRUE,
              onLabel = "Over represented enrichment",
              offLabel = "Gene set enrichment",
              label = icon("grip-horizontal"),
              size = 'large',
              labelWidth = "250px",
              handleWidth = "500px",
              width = "750px"
            )
          ),
          tabsetPanel(
            tabPanel("Enrichment table",
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("do_data_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Enriched terms barplot",
                     HTML('<hr style="border-color: #0088cc;">'),
                     uiOutput("bar_do_slider"),
                     selectInput("bar_do_value", "Create plot with:",
                                 selected = "pvalue",
                                 c("P-Value" = "pvalue",
                                   "Adjusted P-Value" = "p.adjust",
                                   "Q-Value" = "qvalues")
                     ),
                     plotlyOutput("do_barplot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Gene-concept network",
                     HTML('<hr style="border-color: #0088cc;">'),
                     sliderInput("cnet_do_slider", "Amount of shown pathways:", 5, min = 1, max = 15, step=1),
                     plotlyOutput("cnet_do_plot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">'),
                     fluidRow(
                       column(
                         12,
                         DT::dataTableOutput("cnet_do_table") %>% withSpinner())
                     ),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Pathway network",
                     HTML('<hr style="border-color: #0088cc;">'),
                     plotlyOutput("gsea_do_plot", height = "600px") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            ),
            tabPanel("Pathways from RGD",
                     HTML('<hr style="border-color: #0088cc;">'),
                     uiOutput("select_do_pathway", align="center"),
                     htmlOutput("pathway_from_do", align="center") %>% withSpinner(),
                     HTML('<hr style="border-color: #0088cc;">')
            )
          )
        ),

        tabItem(
          "data_export",
          h2("Bookmark current state of application"),
          bookmarkButton(),
          br(),
          br(),
          br(),
          HTML('<hr style="border-color: #0088cc;">'),
          br(),
          h2("Download tables"),
          selectInput("dataset_select", "",
                      c("Samples" = "samples",
                        "Raw counts" = "rawCounts",
                        "Annotation" = "annotation",
                        "Normalized counts" = "normCounts",
                        "All genes" = "deTab",
                        "DE genes" = "deg",
                        "KEGG pathways" = "kegg",
                        "Reactome pathways" = "reactome",
                        "GO pathways" = "go")
                      ),
          downloadButton("downloadCSV", label = "Download as CSV"),
          downloadButton("downloadTSV", label = "Download as TSV"),
          downloadButton("downloadXLSX", label = "Download as XLSX"),
          br(),
          br(),
          br(),
          HTML('<hr style="border-color: #0088cc;">'),
          br(),
          h2("Download R Markdown analysis"),
          downloadButton("downloadMHTML", label = "Download as HTML")
        )
      )
    )
  )
}
