
## Variable heatmap top 100
output[["var_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    variableHeatmapPlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## DGE heatmap top 100
output[["dge_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    topDgeHeatmapPlot(inUse_deTab, inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})
