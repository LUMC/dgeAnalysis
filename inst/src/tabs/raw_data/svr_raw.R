
## Distribution plot line
output[["dist_line"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionLinePlot(get_raw_dge())
  }, error = function(err) {
    return(NULL)
  })
})

## Distribution plot boxplot
output[["dist_boxplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionBoxPlot(get_raw_dge())
  }, error = function(err) {
    return(NULL)
  })
})

## Make dge voom ready
get_pre_voom <- reactive({
  dge <- get_raw_dge()
  dge$counts <- dge$counts[!grepl('^__', rownames(dge$counts)), ]
  selectedFeatures <-
    rownames(dge$counts)[apply(dge$counts, 1, function(v)
      sum(v >= input$slider_raw_voom)) >= 1 / 4 * ncol(dge$counts)]
  highExprDge <- dge[selectedFeatures, , keep.lib.sizes = FALSE]
  highExprDge
})

## Voom plot
output[["raw_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    dge <- get_raw_dge()
    dge$counts <- dge$counts[!grepl('^__', rownames(dge$counts)), ]
    selectedFeatures <-
      rownames(dge$counts)[apply(dge$counts, 1, function(v)
        sum(v >= input$slider_raw_voom)) >= 1 / 4 * ncol(dge$counts)]
    highExprDge <- dge[selectedFeatures, , keep.lib.sizes = FALSE]
    
    voomPlot(highExprDge, "raw_voom")
  }, error = function(err) {
    return(NULL)
  })
})

## Show amount of genes left after filtering
output[["raw_voom_ngenes"]] <- renderUI({
  tryCatch({
    h2("Predicted after filtering:",
       br(),
       nrow(get_pre_voom()),
       "Genes")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points raw_voom_plot
output[["selected_raw_voom"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_voom")
    counts <- data.frame(get_raw_dge()$counts[!grepl('^__', rownames(get_raw_dge()$counts)), ])
    DT::datatable(counts[unlist(s$key), ], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Multidimensional scaling 2D
output[["un_cluster_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling2dPlot(get_raw_dge(), input$group_raw_mds2d, "raw_mds2d")
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds 2d
output[["group_raw_mds2d"]] <- renderUI({
  tryCatch({
    selectInput("group_raw_mds2d", "Color by:",
                colnames(data_samples()))
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points un_cluster_2d
output[["selected_raw_mds2d"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_mds2d")
    if (is.null(s)) {
      s <- ""
    }
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available"
    )), rownames = FALSE, colnames = ""))
  })
})

## Multidimensional scaling 3D
output[["un_cluster_3d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling3dPlot(get_raw_dge(), input$group_raw_mds3d)
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds 2d
output[["group_raw_mds3d"]] <- renderUI({
  tryCatch({
    selectInput("group_raw_mds3d", "Color by:",
                colnames(data_samples()))
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["dist_line_info"]] <- renderUI({
  infoText <-
    "The line plot shows the density of the log2CPM values per sample. The density shows the
        distribution of count values per sample and is used to determine big differences between
        samples."
  informationBox(infoText)
})

output[["dist_boxplot_info"]] <- renderUI({
  infoText <-
    "The box plot has a similar purpose, but the data can be viewed in a different
        format. The distribution can be seen between the Log2CPM at the corresponding
        samples."
  informationBox(infoText)
})

output[["raw_voom_plot_info"]] <- renderUI({
  infoText <-
    "The voom plot provides a check on the filtering, which is performed at the beginning of the
        analysis. The method used to calculate this is 'voom'. Voom is an acronym for
        mean-variance modeling at the observational level. This means that the mean-variance in
        the data is calculated and gives each observation a certain weight. Problems during the
        filtering of low expressed genes will be visible in this plot."
  informationBox(infoText)
})

output[["un_cluster_2d_info"]] <- renderUI({
  infoText <-
    "This MDS plot (multidimensional scaling plot) can be viewed as a 2D plot with
        calculations of two dimensions. With the MDS plot distances between samples is
        shown, based on similarities and differences."
  informationBox(infoText)
})

output[["un_cluster_3d_info"]] <- renderUI({
  infoText <-
    "This MDS plot (multidimensional scaling plot) can be viewed as a 3D plot with
        calculations of three dimensions. With the MDS plot distances between samples is
        shown, based on similarities and differences."
  informationBox(infoText)
})
