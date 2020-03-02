
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
