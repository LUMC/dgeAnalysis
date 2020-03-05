
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

## Voom plot
output[["raw_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    dge <- get_raw_dge()
    dge$counts <- dge$counts[!grepl('^__', rownames(dge$counts)),]
    selectedFeatures <- rownames( dge$counts )[ apply( dge$counts, 1, function( v ) sum( v >= 1 ) ) >= 1/4 * ncol( dge$counts ) ]
    highExprDge <- dge[ selectedFeatures,, keep.lib.sizes = FALSE ]
    
    voomPlot(highExprDge, "raw_voom")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points raw_voom_plot
output[["selected_raw_voom"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_voom")
    counts <- data.frame(get_raw_dge()$counts[!grepl('^__', rownames(get_raw_dge()$counts)),])
    DT::datatable(counts[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Multidimensional scaling 2D
output[["un_cluster_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling2dPlot(get_raw_dge(), input$group_by1, "raw_mds2d")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points un_cluster_2d
output[["selected_raw_mds2d"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_mds2d")
    if(is.null(s)){s <- ""}
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available")), rownames = FALSE, colnames = ""))
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
