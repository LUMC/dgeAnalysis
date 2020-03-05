
## Create table with normalized counts
output[["normalized_counts"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    DT::datatable(inUse_normDge$counts, options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
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

## Normalized voom plot
output[["norm_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    voomPlot(inUse_normDge, "norm_voom")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points norm_voom_plot
output[["selected_norm_voom"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "norm_voom")
    DT::datatable(data.frame(inUse_normDge$counts)[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Normalized Multidimensional scaling 2D
output[["norm_un_cluster_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling2dPlot(inUse_normDge, input$group_norm_mds2d, "norm_mds2d")
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds 2d
output[["group_norm_mds2d"]] <- renderUI({
  tryCatch({
    selectInput("group_norm_mds2d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points norm_un_cluster_2d
output[["selected_norm_mds2d"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "norm_mds2d")
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized Multidimensional scaling 3D
output[["norm_un_cluster_3d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling3dPlot(inUse_normDge, input$group_norm_mds3d)
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds 3d
output[["group_norm_mds3d"]] <- renderUI({
  tryCatch({
    selectInput("group_norm_mds3d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})
