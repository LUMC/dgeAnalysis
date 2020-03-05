
## Variable heatmap
output[["var_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    variableHeatmapPlot(inUse_normDge, input$slider_heatmap_var)
  }, error = function(err) {
    return(NULL)
  })
})

## Set amount of variable genes to show
output[["heatmap_var_ngenes"]] <- renderUI({
  tryCatch({
    sliderInput("slider_heatmap_var", "Set the number of genes to show:", value = 100, min = 2,  max = nrow(inUse_normDge), step=1)
  }, error = function(err) {
    return(NULL)
  })
})

## Sort samples
output[["heatmap_var_sort"]] <- renderUI({
  tryCatch({
    selectInput("sort_heatmap_var", "Sort samples by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})


## DGE heatmap
output[["dge_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    if (is.null(inUse_deTab)){return(NULL)}
    topDgeHeatmapPlot(inUse_deTab, inUse_normDge, input$slider_heatmap_dge)
  }, error = function(err) {
    return(NULL)
  })
})

## Set amount of DGE genes to show
output[["heatmap_dge_ngenes"]] <- renderUI({
  tryCatch({
    sliderInput("slider_heatmap_dge", "Set the number of genes to show:", value = 100, min = 2,  max = nrow(inUse_normDge), step=1)
  }, error = function(err) {
    return(NULL)
  })
})
