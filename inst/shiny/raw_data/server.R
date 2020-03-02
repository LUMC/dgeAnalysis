
## get dge from raw count table
get_raw_dge <- reactive({
  data_counts <- data_counts()
  data_counts <- data_counts[!grepl('^__', rownames(data_counts)),]
  se <- readCountsFromTable(data_counts(),
                            data_samples())
  se <- addSamplesFromTableToSE(se, data_samples())
  if (!is.null(data_annotation())) {
    se <- addAnnotationsFromTableToSE(se, data_annotation())
  }
  dge <- DGEList(counts = assay(se), samples = colData(se), genes = rowData(se))
  dge <- dge[ rowSums( abs( dge$counts ) ) > 1, ]
  dge$counts <- cpm(dge, log = TRUE)
  dge
})

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

## Multidimensional scaling 2D
output[["un_cluster_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling2dPlot(get_raw_dge(), input$group_by1, "")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points un_cluster_2d
output[["un_cluster_2d_clicked"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "un_cluster_2d")
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Multidimensional scaling 3D
output[["un_cluster_3d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling3dPlot(get_raw_dge(), input$group_by1)
  }, error = function(err) {
    return(NULL)
  })
})
