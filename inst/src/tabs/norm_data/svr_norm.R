
## Create table with normalized counts
output[["normalized_counts"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    DT::datatable(inUse_normDge$counts,
                  options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
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

## Show amount of genes left after filtering
output[["norm_voom_ngenes"]] <- renderUI({
  tryCatch({
    checkReload()
    h2("After filtering:", br(), nrow(normDge$counts), "Genes")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points norm_voom_plot
output[["selected_norm_voom"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "norm_voom")
    DT::datatable(data.frame(inUse_normDge$counts)[unlist(s$key), ],
                  options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
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
    selectInput(
      inputId = "group_norm_mds2d",
      label = "Color by:",
      choices = colnames(data_samples())
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
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
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
    selectInput(
      inputId = "group_norm_mds3d",
      label = "Color by:",
      choices = colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["norm_dist_line_info"]] <- renderUI({
  infoText <-
    "The line plot shows the density of the log2CPM values per sample. The density shows the
  distribution of count values per sample and is used to determine big differences between
  samples."
  informationBox(infoText)
})

output[["norm_dist_boxplot_info"]] <- renderUI({
  infoText <-
    "The box plot has a similar purpose, but the data can be viewed in a different
  format. The distribution can be seen between the Log2CPM at the corresponding
  samples."
  informationBox(infoText)
})

output[["norm_voom_plot_info"]] <- renderUI({
  infoText <-
    "The voom plot provides a check on the filtering, which is performed at the beginning of the
  analysis. The method used to calculate this is 'voom'. Voom is an acronym for
  mean-variance modeling at the observational level. This means that the mean-variance in
  the data is calculated and gives each observation a certain weight. Problems during the
  filtering of low expressed genes will be visible in this plot."
  informationBox(infoText)
})

output[["norm_un_cluster_2d_info"]] <- renderUI({
  infoText <-
    "This MDS plot (multidimensional scaling plot) can be viewed as a 2D plot with
  calculations of two dimensions. With the MDS plot distances between samples is
  shown, based on similarities and differences."
  informationBox(infoText)
})

output[["norm_un_cluster_3d_info"]] <- renderUI({
  infoText <-
    "This MDS plot (multidimensional scaling plot) can be viewed as a 3D plot with
  calculations of three dimensions. With the MDS plot distances between samples is
  shown, based on similarities and differences."
  informationBox(infoText)
})
