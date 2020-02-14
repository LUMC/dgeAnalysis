

## Back-end of Shiny program ##

options(shiny.maxRequestSize = 50*1024^2)
options(warn = -1)
options(spinner.color="#0088cc")
shinyOptions(appDir = Sys.getenv("HOME"))
# rm(list="deTab", "normDge")

server <- function(input, output, session) {

  ## ----- INPUT CHECKS -----

  ## check if all components for analysis are present
  checkReload <- function(){
    is.null(input$design_value)
    is.null(input$matrix_value)
    is.null(input$cpm_value)
    is.null(input$alpha_value)
    is.null(input$setGeneName)
    is.null(input$analysis_method)
    is.null(input$sample_data_rows_selected)
    is.null(input$choose_analysis)
  }

  ## --------------------------------------------------------------------------

  ## ----- BOOKMARK -----

  ## Restore bookmark
  onRestored(function(state) {
    if (isFALSE(input$choose_analysis)) {
      shinyjs::toggle("view_analysis_div")
      shinyjs::toggle("new_analysis_div")
    }
    output$analysis_state <- renderText({
      "Bookmark loaded"
    })
  })
  onRestore(function(state) {
    bookmark_loaded <<- TRUE
    tryCatch({
      inUse_deTab <<- state$values$inUse_deTab
      inUse_normDge <<- state$values$inUse_normDge
      deTab <<- state$values$deTab
      normDge <<- state$values$normDge
      vdeTab <<- state$values$vnormDge
      vnormDge <<- state$values$vdeTab
    }, error = function(err) {
      return(NULL)
    })
    updateTabsetPanel(session, "sidebar", selected = "data_upload")
  })

  ## Save data in bookmark
  onBookmark(function(state) {
    tryCatch({
      state$values$inUse_deTab <- inUse_deTab
      state$values$inUse_normDge <- inUse_normDge
      state$values$deTab <- deTab
      state$values$normDge <- normDge
      state$values$vdeTab <- vdeTab
      state$values$vnormDge <- vnormDge
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- OBSERVE EVENTS -----

  ## Start an analysis
  observeEvent(input$run_button, {
    tryCatch({
      if (isTRUE(bookmark_loaded)) {
        bookmark_loaded <<- FALSE
        return(NULL)
      }
    }, error = function(err) {
    })

    showModal(modalDialog("Analysis is running...",
                          footer=NULL))
    results <- tryCatch(
      {
        rmarkdown::render(paste(input$analysis_method, ".Rmd", sep=''),
                          params = list(data_samples = data_samples(),
                                        data_counts = data_counts(),
                                        data_annotation = data_annotation(),
                                        excluded_samples = rownames(excluded_selected()),
                                        setGeneName = input$setGeneName,
                                        cpm_value = input$cpm_value,
                                        design_value = paste("~0 +", gsub(",", " +", toString(c(input$design_value)))),
                                        matrix_value = if (!is.null(input$design_value)){input$matrix_value},
                                        alpha = input$alpha_value),
                          output_file = paste(input$analysis_method, '.html', sep=''))
        load("analysis.RData", envir=.GlobalEnv)
        inUse_deTab <<- deTab
        inUse_normDge <<- normDge
        f <- "Analysis succesfull"
      }, error = function(err) {
        f <- "Analysis failed with an error"
        print(err)
        return(f)
      }
    )
    output$analysis_state <- renderText({
      results
    })
    removeModal()
  })

  ## monitor the app mode (new/view)
  observe({
    if (isFALSE(input$choose_analysis)) {
      is.null(input$vfile_normalized)
      vnormDge <<- get_normDge()
      inUse_normDge <<- vnormDge
    }
  })

  ## monitor the app mode (new/view)
  observe({
    if (isFALSE(input$choose_analysis)) {
      is.null(input$vfile_de)
      vdeTab <<- get_deTab()
      inUse_deTab <<- vdeTab
    }
  })

  ## --------------------------------------------------------------------------

  ## ----- READ INPUT DATA -----

  ## Read sample data file ##
  data_samples <- reactive({
    if (is.null(input$file_samples$datapath)){return(NULL)}
    data_samples <- read.csv(input$file_samples$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_samples) < 1) {
      data_samples <- read.csv(input$file_samples$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
    data_samples
  })

  ## Render sample data to ui
  output$sample_data <- DT::renderDataTable({
    tryCatch({
      DT::datatable(data_samples(), options = list(pageLength = 50, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Read count data file ##
  data_counts <- reactive({
    if (is.null(input$file_counts$datapath)){return(NULL)}
    data_counts <- read.csv(input$file_counts$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_counts) < 1) {
      data_counts <- read.csv(input$file_counts$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
    data_counts
  })

  ## Render count data to ui
  output$count_data <- DT::renderDataTable({
    tryCatch({
      DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Read annotation data file ##
  data_annotation <- reactive({
    if (is.null(input$file_annotation$datapath)){return(NULL)}
    data_annotation <- read.csv(input$file_annotation$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_annotation) < 1) {
      data_annotation <- read.csv(input$file_annotation$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
    data_annotation
  })

  ## Render annotation data to ui
  output$annotation_data <- DT::renderDataTable({
    tryCatch({
      DT::datatable(data_annotation(), options = list(pageLength = 50, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- GET SELECTED ROWS -----

  ## Get selected samples from samples ##
  output$excluded_samples <- DT::renderDataTable({
    DT::datatable(excluded_selected(), selection = list(mode = "multiple"), options = list(pageLength = 50, scrollX = TRUE))
  })
  excluded_selected <- reactive({
    ids <- input$sample_data_rows_selected
    data_samples()[ids,]
  })

  ## --------------------------------------------------------------------------

  ## ----- ANALYSIS TABLES -----

  ## Get normalized counts from viewer
  data_normalized <- reactive({
    if (is.null(input$vfile_normalized$datapath)){return(NULL)}
    data_normalized <- read.csv(input$vfile_normalized$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_normalized) < 1) {
      data_normalized <- read.csv(input$vfile_normalized$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
    data_normalized
  })

  ## Create table with normalized counts
  output$normalized_counts <- DT::renderDataTable({
    tryCatch({
      checkReload()
      DT::datatable(inUse_normDge$counts, options = list(pageLength = 50, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Get deTab from viewer
  data_deTab <- reactive({
    if (is.null(input$vfile_de$datapath)){return(NULL)}
    data_deTab <- read.csv(input$vfile_de$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_deTab) < 1) {
      data_deTab <- read.csv(input$vfile_de$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
    data_deTab
  })

  #Get deTab
  output$all_genes_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      DT::datatable(inUse_deTab, options = list(pageLength = 50, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Get all DEG
  output$deg_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      DT::datatable(inUse_deTab[inUse_deTab$DE != 0,], options = list(pageLength = 50, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- DOWNLOAD TABLES -----

  ## get the right dataset for download
  datasetInput <- reactive({
    checkReload()
    switch(input$dataset_select,
           "samples" = data_samples(),
           "rawCounts" = data_counts(),
           "annotation" = data_annotation(),
           "normCounts" = inUse_normDge$counts,
           "deTab" = inUse_deTab,
           "deg" = inUse_deTab[inUse_deTab$DE != 0,],
           "kegg" = as.data.frame(get_kegg())[ , -c(1, 10, 11)],
           "reactome" = as.data.frame(get_reactome())[ , -c(1, 10, 11)],
           "go" = as.data.frame(get_reactome())[ , -c(1, 10, 11)])
  })

  ## create filename and save data as CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".csv", sep = "")
    },
    content = function(file) {
      write.table(datasetInput(), file, row.names = TRUE, col.names=NA, sep = ",")
    }
  )

  ## create filename and save data as TSV
  output$downloadTSV <- downloadHandler(
    filename = function() {
      paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".tsv", sep = "")
    },
    content = function(file) {
      write.table(datasetInput(), file, row.names = TRUE, col.names=NA, sep = "\t")
    }
  )

  ## create filename and save data as XLSX
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".xlsx", sep = "")
    },
    content = function(file) {
      write.table(datasetInput(), file, row.names = TRUE, col.names=NA)
    }
  )

  ## --------------------------------------------------------------------------

  ## ----- ALIGNMENT PLOTS -----

  ## get se_counts from table
  get_secounts <- reactive({
    se_counts <- readCountsFromTable(data_counts()[,!colnames(data_counts()) %in% rownames(excluded_selected())],
                                     data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])
    se_counts
  })

  ## Create alignment summary plot
  output$align_sum <- renderPlotly({
    tryCatch({
      checkReload()
      alignmentSummaryPlot(get_secounts(), perc = F)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Create alignment summary percentage plot
  output$align_sum_perc <- renderPlotly({
    tryCatch({
      checkReload()
      alignmentSummaryPlot(get_secounts())
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Create complexity plot
  output$complex <- renderPlotly({
    tryCatch({
      checkReload()
      complexityPlot(get_secounts(), perc = F)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Create complexity percentage plot
  output$complex_perc <- renderPlotly({
    tryCatch({
      checkReload()
      complexityPlot(get_secounts())
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- RAW DATA PLOTS -----

  ## get dge from raw count table
  get_raw_dge <- reactive({
    data_counts <- data_counts()
    data_counts <- data_counts[!grepl('^__', rownames(data_counts)),]
    se <- readCountsFromTable(data_counts()[,!colnames(data_counts()) %in% rownames(excluded_selected())],
                              data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])
    se <- addSamplesFromTableToSE(se, data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])
    if (!is.null(data_annotation())) {
      se <- addAnnotationsFromTableToSE(se, data_annotation())
    }
    dge <- DGEList(counts = assay(se), samples = colData(se), genes = rowData(se))
    dge <- dge[ rowSums( abs( dge$counts ) ) > 1, ]
    dge$counts <- cpm(dge, log = TRUE)
    dge
  })

  ## Distribution plot line
  output$dist_line <- renderPlotly({
    tryCatch({
      checkReload()
      countDistributionLinePlot(get_raw_dge())
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Distribution plot boxplot
  output$dist_boxplot <- renderPlotly({
    tryCatch({
      checkReload()
      countDistributionBoxPlot(get_raw_dge())
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Multidimensional scaling 2D
  output$un_cluster_2d <- renderPlotly({
    tryCatch({
      checkReload()
      multidimensionalScaling2dPlot(get_raw_dge(), input$group_by1, "")
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Selected data points un_cluster_2d
  output$un_cluster_2d_clicked <- DT::renderDataTable({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_selected", source = "un_cluster_2d")
      DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Multidimensional scaling 3D
  output$un_cluster_3d <- renderPlotly({
    tryCatch({
      checkReload()
      multidimensionalScaling3dPlot(get_raw_dge(), input$group_by1)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- NORMALIZATION PLOTS -----

  ## Create normDge from table. if an error occurs (mostly because negative numbers) -> log2 values is converted with 2^
  get_normDge <- reactive({
    tryCatch({
      se <- readCountsFromTable(data_normalized()[,!colnames(data_normalized()) %in% rownames(excluded_selected())],
                                data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])
      se <- addSamplesFromTableToSE(se, data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])
      normDge <- tryCatch({
          return(DGEList(counts = assay(se), samples = colData(se)))
        }, error = function(err) {
          temp <- DGEList(counts = 2^(assay(se)), samples = colData(se))
          temp$counts <- log2(temp$counts)
          return(temp)
        }
      )
      normDge
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Normalized distribution plot line
  output$norm_dist_line <- renderPlotly({
    tryCatch({
      checkReload()
      countDistributionLinePlot(inUse_normDge)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Normalized distribution plot boxplot
  output$norm_dist_boxplot <- renderPlotly({
    tryCatch({
      checkReload()
      countDistributionBoxPlot(inUse_normDge)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Normalized Multidimensional scaling 2D
  output$norm_un_cluster_2d <- renderPlotly({
    tryCatch({
      checkReload()
      multidimensionalScaling2dPlot(inUse_normDge, input$group_by2, "norm_")
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Selected data points norm_un_cluster_2d
  output$norm_un_cluster_2d_clicked <- DT::renderDataTable({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_selected", source = "norm_un_cluster_2d")
      DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Normalized Multidimensional scaling 3D
  output$norm_un_cluster_3d <- renderPlotly({
    tryCatch({
      checkReload()
      multidimensionalScaling3dPlot(inUse_normDge, input$group_by2)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- PCA PLOTS -----

  ## Create select/text input to select a gene
  output$pca_feature <- renderUI({
    genes <- rownames(inUse_normDge$counts)
    selectizeInput("pca_feature", choices = genes, label = 'Search feature', options = list(maxOptions = 10))
  })

  ## variance PCA
  output$variance_pca <- renderPlotly({
    tryCatch({
      checkReload()
      variancePcaPlot(inUse_normDge)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## samples PCA 2D
  output$samples_pca_2d <- renderPlotly({
    tryCatch({
      checkReload()
      samplePca2dPlot(inUse_normDge, input$group_by3)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Selected data points samples_pca_2d
  output$samples_pca_2d_clicked <- DT::renderDataTable({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_selected", source = "samples_pca_2d")
      DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## samples PCA 3D
  output$samples_pca_3d <- renderPlotly({
    tryCatch({
      checkReload()
      samplePca3dPlot(inUse_normDge, input$group_by3)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## genes PCA 2D :::: NOT IN AT THE MOMENT
  output$genes_pca_2d <- renderPlotly({
    tryCatch({
      checkReload()
      if (isTRUE(input$choose_analysis)) {
        genesPca2dPlot(normDge, input$group_by3)
      } else  {
        genesPca2dPlot(get_normDge(), input$group_by3)
      }
    }, error = function(err) {
      return(NULL)
    })
  })

  ## genes PCA 3D :::: NOT IN AT THE MOMENT
  output$genes_pca_3d <- renderPlotly({
    tryCatch({
      checkReload()
      if (isTRUE(input$choose_analysis)) {
        genesPca3dPlot(normDge, input$pca_feature)
      } else {
        genesPca3dPlot(get_normDge(), input$pca_feature)
      }
    }, error = function(err) {
      return(NULL)
    })
  })

  ## samples PCA CPM :::: NOT IN AT THE MOMENT
  output$samples_pca_cpm <- renderPlotly({
    tryCatch({
      checkReload()
      if (isTRUE(input$choose_analysis)) {
        samplePcaCpmPlot(normDge, input$group_by3)
      } else {
        samplePcaCpmPlot(get_normDge(), input$group_by3)
      }
    }, error = function(err) {
      return(NULL)
    })
  })

  ## genes PCA CPM :::: NOT IN AT THE MOMENT
  output$genes_pca_cpm <- renderPlotly({
    tryCatch({
      checkReload()
      if (isTRUE(input$choose_analysis)) {
        genesPcaCpmPlot(normDge, input$group_by3)
      } else {
        genesPcaCpmPlot(get_normDge(), input$group_by3)
      }
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- HEATMAPS PLOTS -----

  ## Variable heatmap top 100
  output$var_heat <- renderPlotly({
    tryCatch({
      checkReload()
      variableHeatmapPlot(inUse_normDge)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## DGE heatmap top 100
  output$dge_heat <- renderPlotly({
    tryCatch({
      checkReload()
      topDgeHeatmapPlot(inUse_deTab, inUse_normDge)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- ANALYSIS PLOTS -----

  ## get deTab from table
  get_deTab <- reactive({
    deTab <- data_deTab()
    deTab
  })

  ## Selected data points plots
  output$selected_clicked_plots <- DT::renderDataTable({
    tryCatch({
      checkReload()
      ids <- unique(c(input$normalized_counts_rows_selected,
                      input$all_genes_table_rows_selected,
                      input$deg_table_rows_selected))
      s <- event_data(event = "plotly_selected", source = "analysis_plots")

      table_select <- rownames(inUse_deTab[ids,])
      plot_select <- append(table_select, s$key)
      DT::datatable(inUse_deTab[plot_select,], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Voom plot
  output$voom_plot <- renderPlotly({
    tryCatch({
      checkReload()
      ids <- unique(c(input$normalized_counts_rows_selected,
                      input$all_genes_table_rows_selected,
                      input$deg_table_rows_selected))
      s <- event_data(event = "plotly_selected", source = "analysis_plots")

      table_select <- rownames(inUse_deTab[ids,])
      plot_select <- append(s$key, table_select)
      voomPlot(inUse_normDge, inUse_deTab, plot_select)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Residual variances :::: NOT IN AT THE MOMENT
  output$res_var <- renderPlotly({
    tryCatch({
      checkReload()
      if (isTRUE(input$choose_analysis)) {
        residualVariancePlot(deTab)
      } else {
        residualVariancePlot(get_deTab())
      }
    }, error = function(err) {
      return(NULL)
    })
  })

  ## DE ratio
  output$de_ratio <- renderPlotly({
    tryCatch({
      checkReload()
      deRatioPlot(inUse_deTab)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Mean-Difference (MA) plots
  output$ma_plot <- renderPlotly({
    tryCatch({
      checkReload()
      ids <- unique(c(input$normalized_counts_rows_selected,
                    input$all_genes_table_rows_selected,
                    input$deg_table_rows_selected))
      s <- event_data(event = "plotly_selected", source = "analysis_plots")

      table_select <- rownames(inUse_deTab[ids,])
      plot_select <- append(s$key, table_select)
      ma_plot(inUse_deTab, plot_select)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Volcano plots
  output$volcano_plot <- renderPlotly({
    tryCatch({
      checkReload()
      ids <- unique(c(input$normalized_counts_rows_selected,
                      input$all_genes_table_rows_selected,
                      input$deg_table_rows_selected))
      s <- event_data(event = "plotly_selected", source = "analysis_plots")

      table_select <- rownames(inUse_deTab[ids,])
      plot_select <- append(s$key, table_select)
      volcanoPlot(inUse_deTab, input$vulcanoLogCut, -log10(input$vulcanoPCut), plot_select)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Barcode plot
  output$barcode_plot <- renderPlotly({
    tryCatch({
      checkReload()
      barcodePlot(inUse_deTab, inUse_normDge, input$group_by4)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## P value plots
  output$p_val_plot <- renderPlotly({
    tryCatch({
      checkReload()
      pValuePlot(inUse_deTab)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- BIAS PLOTS -----

  ## GC bias
  output$gc_bias <- renderPlotly({
    tryCatch({
      checkReload()
      biasPlot(inUse_deTab, input$selectGC, NA)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## feature length bias
  output$len_bias <- renderPlotly({
    tryCatch({
      checkReload()
      biasPlot(inUse_deTab, input$selectLength, "log")
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- GENE SET ENRICHMENT KEGG -----

  ## get and create kegg enrichment
  get_kegg <- reactive({
    tryCatch({
      checkReload()
      organism <- get_organismID(inUse_deTab)
      org <- list(ENS="hsa",
                  ENSMUS="mmu")
      organism <- org[[organism]]
      if (isTRUE(input$choose_kegg)) {
        enrich <- clusterProfiler::enrichKEGG(inUse_deTab$entrez[inUse_deTab$DE!=0], organism=organism, pvalueCutoff=0.05)
      } else {
        set.seed(1234)
        geneList <- get_geneList(inUse_deTab)
        enrich <- clusterProfiler::gseKEGG(geneList, organism=organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
      }
      enrich
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create kegg enrichment table
  output$kegg_data_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_kegg())
      enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
      DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create barplot with kegg enrichment terms
  output$kegg_barplot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_kegg())
      enrichBarplot(enrich, input$bar_kegg_slider, input$bar_kegg_value)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create cnet plot with kegg input
  output$cnet_kegg_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_kegg()

      geneSets <- extract_geneSets(enrich, input$cnet_kegg_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_kegg_slider)
      plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## get all genes of cnet plot
  output$cnet_kegg_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- get_kegg()

      geneSets <- extract_geneSets(enrich, input$cnet_kegg_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_kegg_slider)
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create kegg plot with all pathways
  output$gsea_kegg_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_kegg()
      graphData <- emap_plotly(enrich)
      plotlyGraph(graphData, "KEGG", "P-Value", 0)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create kegg plot of specific pathway by selection
  output$kegg_pathway <- renderPlotly({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_click", source = "KEGG")

      graphData <- viewPathwayPlot(inUse_deTab, 'kegg', s$key)
      plotlyGraph(graphData, s$key, "Log2FC", 0)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## get all genes of specific pathway by selection
  output$kegg_pathway_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_click", source = "KEGG")

      graphData <- viewPathwayPlot(inUse_deTab, 'kegg', s$key)
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Show dropdown of all found kegg pathways
  output$select_kegg_pathway <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_kegg())
      selectInput("kegg_select", "Select a pathway:",
                  enrich$Description
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## show selected pathway from kegg
  output$pathway_from_kegg <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_kegg())
      getpathway <- rownames(enrich)[enrich$Description %in% input$kegg_select]
      getFromKegg <- includeHTML(paste("https://www.genome.jp/kegg-bin/show_pathway?", getpathway, sep=""))
      getFromKegg <- gsub('src="', 'src="https://www.genome.jp', getFromKegg)
      getFromKegg <- gsub('href="', 'href="https://www.genome.jp', getFromKegg)
      getFromKegg <- gsub("<head.*</head>", "", getFromKegg)
      getFromKegg <- gsub("<table.*</table>", "", getFromKegg)
      getFromKegg
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- GENE SET ENRICHMENT REACTOME -----

  ## get and create reactome enrichment
  get_reactome <- reactive({
    tryCatch({
      checkReload()
      organism <- get_organismID(inUse_deTab)
      org <- list(ENS="human",
                  ENSMUS="mouse")
      organism <- org[[organism]]
      if (isTRUE(input$choose_reactome)) {
        enrich <- ReactomePA::enrichPathway(inUse_deTab$entrez[inUse_deTab$DE!=0], organism=organism, pvalueCutoff=0.05)
      } else {
        set.seed(1234)
        geneList <- get_geneList(inUse_deTab)
        enrich <- ReactomePA::gsePathway(geneList, organism=organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
      }
      enrich
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create reactome enrichment table
  output$reactome_data_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_reactome())
      enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
      DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create barplot with reactome enrichment terms
  output$reactome_barplot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_reactome())
      enrichBarplot(enrich, input$bar_reactome_slider, input$bar_reactome_value)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create cnet plot with reactome input
  output$cnet_reactome_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_reactome()

      geneSets <- extract_geneSets(enrich, input$cnet_reactome_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_reactome_slider)
      plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## get all genes of cnet plot
  output$cnet_reactome_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- get_reactome()

      geneSets <- extract_geneSets(enrich, input$cnet_reactome_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_reactome_slider)
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create reactome plot with all pathways
  output$gsea_reactome_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_reactome()
      graphData <- emap_plotly(enrich)
      plotlyGraph(graphData, "Reactome", "P-Value", 0)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create reactome plot of specific pathway by selection
  output$reactome_pathway <- renderPlotly({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_click", source = "Reactome")

      graphData <- viewPathwayPlot(inUse_deTab, 'reactome', s$key)
      plotlyGraph(graphData, s$key, "Log2FC", 0)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## get all genes of specific pathway by selection
  output$reactome_pathway_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      s <- event_data(event = "plotly_click", source = "Reactome")

      graphData <- viewPathwayPlot(inUse_deTab, 'reactome', s$key)
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Show dropdown of all found reactome pathways
  output$select_reactome_pathway <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_reactome())
      selectInput("reactome_select", "Select a pathway:",
                  enrich$Description
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## show selected pathway from kegg
  output$pathway_from_reactome <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_reactome())
      getpathway <- rownames(enrich)[enrich$Description %in% input$reactome_select]
      getFromReactome <- includeHTML(paste("https://reactome.org/ContentService/exporter/diagram/", getpathway, ".svg?title=false", sep=""))
      getFromReactome <- HTML(paste('<a href="https://reactome.org/PathwayBrowser/#/', getpathway, '" target="_blank">\n', getFromReactome, '\n</a>', sep=""))
      getFromReactome
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- GENE SET ENRICHMENT GENE ONTOLOGY -----

  ## Biological Process (BP)
  ## Cellular Component (CC)
  ## Molecular Function (MF)

  ## get and create gene ontology enrichment
  get_go <- reactive({
    tryCatch({
      checkReload()
      organism <- get_organismID(inUse_deTab)
      org <- list(ENS="org.Hs.eg.db",
                  ENSMUS="org.Mm.eg.db")
      organism <- org[[organism]]
      if (isTRUE(input$choose_go)) {
        enrich <- clusterProfiler::enrichGO(inUse_deTab$entrez[inUse_deTab$DE!=0],  ont = input$selectOntology, organism, pvalueCutoff=0.05)
      } else {
        set.seed(1234)
        geneList <- get_geneList(inUse_deTab)
        enrich <- clusterProfiler::gseGO(geneList, ont = input$selectOntology, organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
      }
      enrich
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create gene ontology enrichment table
  output$go_data_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_go())
      enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
      DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create barplot with gene ontology enrichment terms
  output$go_barplot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_go())
      enrichBarplot(enrich, input$bar_go_slider, input$bar_go_value)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create cnet plot with gene ontology input
  output$cnet_go_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_go()

      geneSets <- extract_geneSets(enrich, input$cnet_go_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_go_slider)
      plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## get all genes of cnet plot
  output$cnet_go_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- get_go()

      geneSets <- extract_geneSets(enrich, input$cnet_go_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_go_slider)
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create gene ontology plot with all pathways
  output$gsea_go_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_go()
      graphData <- emap_plotly(enrich)
      plotlyGraph(graphData, input$selectOntology, "P-Value", 0)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Show dropdown of all found gene ontology pathways
  output$select_go_pathway <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_go())
      selectInput("go_select", "Select a pathway:",
                  enrich$Description
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## show selected pathway from gene ontology
  output$pathway_from_go <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_go())
      getpathway <- rownames(enrich)[enrich$Description %in% input$go_select]
      getFromGo <- paste('<img src="https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/', getpathway, '/chart">', sep="")
      getFromGo <- HTML(paste('<a href="https://www.ebi.ac.uk/QuickGO/GTerm?id=', getpathway, '" target="_blank">\n', getFromGo, '\n</a>', sep=""))
      getFromGo
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- GENE SET ENRICHMENT DISEASE -----

  # ONLY SUPPORTS HUMAN!!!

  ## get and create disease enrichment
  get_do <- reactive({
    tryCatch({
      checkReload()
      if (isTRUE(input$choose_do)) {
        enrich <- DOSE::enrichDO(inUse_deTab$entrez[inUse_deTab$DE!=0], pvalueCutoff=0.05)
      } else {
        set.seed(1234)
        geneList <- get_geneList(inUse_deTab)
        enrich <- DOSE::gseDO(geneList, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
      }
      enrich
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create disease enrichment table
  output$do_data_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_do())
      enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
      DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create barplot with disease enrichment terms
  output$do_barplot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_do())
      enrichBarplot(enrich, input$bar_do_slider, input$bar_do_value)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create cnet plot with disease input
  output$cnet_do_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_do()

      geneSets <- extract_geneSets(enrich, input$cnet_do_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_do_slider)
      plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## get all genes of cnet plot
  output$cnet_do_table <- DT::renderDataTable({
    tryCatch({
      checkReload()
      enrich <- get_do()

      geneSets <- extract_geneSets(enrich, input$cnet_do_slider)
      graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_do_slider)
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }, error = function(err) {
      return(NULL)
    })
  })

  ## create disease plot with all pathways
  output$gsea_do_plot <- renderPlotly({
    tryCatch({
      checkReload()
      enrich <- get_do()
      graphData <- emap_plotly(enrich)
      plotlyGraph(graphData, "Disease ontology", "P-Value", 0)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Show dropdown of all found gene ontology pathways
  output$select_do_pathway <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_do())
      selectInput("do_select", "Select a pathway:",
                  enrich$Description
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## show selected pathway from gene ontology
  output$pathway_from_do <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_do())
      getpathway <- rownames(enrich)[enrich$Description %in% input$do_select]

      getFromDo <- includeHTML(paste("https://rgd.mcw.edu/rgdweb/ontology/view.html?acc_id=", getpathway, sep=""))
      getFromDo <- gsub('.*<div id="browser_graph" style="width:800px;overflow:auto">\\s*|\\s*</div>.*', "", getFromDo)
      if (grepl('<img id="termHierarchy"', getFromDo)) {
        getFromDo <- gsub('src="', 'src="https://rgd.mcw.edu', getFromDo)
        getFromDo <- gsub('href="', 'href="https://rgd.mcw.edu/rgdweb/ontology/', getFromDo)
      } else {
        getFromDo <- NULL
      }
      getFromDo
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- RENDER UI DATA UPLOAD -----

  ## show or hide the right analysis div (toggle button)
  observeEvent(input$choose_analysis, {
    shinyjs::toggle("view_analysis_div")
    shinyjs::toggle("new_analysis_div")
    if (isFALSE(input$choose_analysis)) {
      tryCatch({
        inUse_deTab <<- get_deTab()
        inUse_normDge <<- get_normDge()
      }, error = function(err) {
        return(NULL)
      })
      output$analysis_state <- renderText({
        "View analysis"
      })
    } else {
      tryCatch({
        inUse_deTab <<- deTab
        inUse_normDge <<- normDge
      }, error = function(err) {
        return(NULL)
      })
      output$analysis_state <- renderText({
        "No analysis done"
      })
    }
  })

  ## New analysis ui (div)
  output$new_analysis_input <- renderUI({
    tagList(
      fileInput("file_annotation", "Choose your annotation file (optional):",
                multiple = FALSE,
                accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
      HTML('<hr style="border-color: #0088cc;">'),
      h2("Settings"),
      radioButtons("analysis_method", "Choose analysis method:",
                   inline = TRUE,
                   selected = "analysisEdgeR",
                   c("Limma/Voom" = "analysisLimma",
                     "EdgeR" = "analysisEdgeR",
                     "DESeq2" = "analysisDESeq2")
      ),
      uiOutput("design_value"),
      uiOutput("matrix_value"),
      sliderInput("alpha_value", "Set alpha (P-Value cutoff):", 0.05, min = 0.01, max = 1, step=0.01),
      sliderInput("cpm_value", "Set Log2CPM cutoff:", 1, min = 0, max = 10, step=0.1),
      uiOutput("setGeneName"),
      br(),
      actionButton("run_button", "Run Analysis"),
      br()
    )
  })

  ## View analysis ui (div)
  output$view_analysis_input <- renderUI({
    tagList(
      fileInput("vfile_normalized", "Choose your normalized counts file:",
                multiple = FALSE,
                accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt")),
      fileInput("vfile_de", "Choose your DE file:",
                multiple = FALSE,
                accept = c("CSV", ".csv", "TSV", ".tsv", "TXT", ".txt"))
    )
  })

  ## Create design options
  output$design_value <- renderUI({
    tryCatch({
      if (is.null(input$file_samples$datapath)){return(NULL)}
      checkboxGroupInput("design_value",
                         "Select columns to compare:",
                         choices = colnames(data_samples()),
                         inline = TRUE
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Create matrix options
  output$matrix_value <- renderUI({
    tryCatch({
      if (is.null(input$design_value)) {
        return(NULL)
      }

      choice <- NULL
      for (col in input$design_value) {
        for (x in combn(levels(droplevels(data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])[[col]]), 2, simplify = FALSE)) {
          choice <- c(choice, gsub(",", " -", toString(x)))
        }
        for (x in combn(rev(levels(droplevels(data_samples()[!rownames(data_samples()) %in% rownames(excluded_selected()),])[[col]])), 2, simplify = FALSE)) {
          choice <- c(choice, gsub(",", " -", toString(x)))
        }
      }

      selectInput("matrix_value",
                  "Select values to compare:",
                  choices = choice
                  )
    }, error = function(err) {
      return(NULL)
    })
  })

  # CPM cutoff slider | NOT IN USE ANYMORE
  output$setGeneName <- renderUI({
    tryCatch({
      if (is.null(data_annotation())) {
        return(NULL)
      }

      radioButtons("setGeneName", "Use gene ID or gene symbols:",
                   inline = TRUE,
                   c("Gene ID" = "id",
                     "Gene Symbol" = "symbol")
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- RENDER UI GROUP BY COLORS -----

  ## Group by columns of sample metadata
  output$group_by1 <- renderUI({
    tryCatch({
      selectInput("group_by1", "Group by:",
                  colnames(data_samples())
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Group by columns of sample metadata
  output$group_by2 <- renderUI({
    tryCatch({
      selectInput("group_by2", "Group by:",
                  colnames(data_samples())
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Group by columns of sample metadata
  output$group_by3 <- renderUI({
    tryCatch({
      selectInput("group_by3", "Group by:",
                  colnames(data_samples())
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## Group by columns of sample metadata
  output$group_by4 <- renderUI({
    tryCatch({
      selectInput("group_by4", "Group by:",
                  colnames(data_samples())
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- RENDER UI SELECT BIAS COLUMN -----

  ## dropdown with all gc choices
  output$selectGC <- renderUI({
    tryCatch({
      checkReload()
      selectInput("selectGC", "Show bias based on:",
                  grep('gc$', colnames(inUse_deTab), value=TRUE, ignore.case=TRUE)
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## dropdown with all length choices
  output$selectLength <- renderUI({
    tryCatch({
      checkReload()
      selectInput("selectLength", "Show bias based on:",
                  grep('length$', colnames(inUse_deTab), value=TRUE, ignore.case=TRUE)
      )
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- RENDER UI ENRICHMENT BARPLOT SLIDERS -----

  output$bar_kegg_slider <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_kegg())
      sliderInput("bar_kegg_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
    }, error = function(err) {
      return(NULL)
    })
  })

  output$bar_reactome_slider <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_reactome())
      sliderInput("bar_reactome_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
    }, error = function(err) {
      return(NULL)
    })
  })

  output$bar_go_slider <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_go())
      sliderInput("bar_go_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
    }, error = function(err) {
      return(NULL)
    })
  })

  output$bar_do_slider <- renderUI({
    tryCatch({
      checkReload()
      enrich <- as.data.frame(get_do())
      sliderInput("bar_do_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
    }, error = function(err) {
      return(NULL)
    })
  })

  ## --------------------------------------------------------------------------

  ## ----- DOWNLOAD MARKDOWN -----

  ## Download markdown output as HTML
  output$downloadMHTML <- downloadHandler(
    filename = paste(input$analysis_method, '_', gsub('-', '', Sys.Date()), '.html', sep=''),
    content <- function(file) {
      file.copy(paste(input$analysis_method, '.html', sep=''), file)
    }
  )

  ## --------------------------------------------------------------------------

}
