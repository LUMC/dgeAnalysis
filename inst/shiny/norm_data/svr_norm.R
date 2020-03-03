
## Create table with normalized counts
output[["normalized_counts"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    DT::datatable(inUse_normDge$counts, options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized distribution plot line
output[["norm_dist_line"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionLinePlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized distribution plot boxplot
output[["norm_dist_boxplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionBoxPlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## Voom plot
output[["voom_plot"]] <- renderPlotly({
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

## Normalized Multidimensional scaling 2D
output[["norm_un_cluster_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling2dPlot(inUse_normDge, input$group_by2, "norm_")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points norm_un_cluster_2d
output[["norm_un_cluster_2d_clicked"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "norm_un_cluster_2d")
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized Multidimensional scaling 3D
output[["norm_un_cluster_3d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling3dPlot(inUse_normDge, input$group_by2)
  }, error = function(err) {
    return(NULL)
  })
})
