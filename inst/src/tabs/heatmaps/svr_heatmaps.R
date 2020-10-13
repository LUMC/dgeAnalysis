
## Variable heatmap
output[["var_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    variableHeatmapPlot(inUse_normDge, input$group_var, input$slider_heatmap_var)
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

## Select a group to sort var heatmap
output[["group_var"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      "group_var",
      "Group by:",
      c("None"="None", colnames(data_samples()))
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
    topDgeHeatmapPlot(inUse_deTab, inUse_normDge, input$group_dge, input$slider_heatmap_dge)
  }, error = function(err) {
    return(NULL)
  })
})

## Select a group to sort dge heatmap
output[["group_dge"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      "group_dge",
      "Group by:",
      c("None"="None", colnames(data_samples()))
    )
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

## INFORMATION BOXES

output[["var_heat_info"]] <- renderUI({
  infoText <- "This heatmap is based on the most variable genes. This is calculated
        by getting the average Log2CPM difference of a gene between all samples. The
        genes are then sorted based on the calculated difference. The genes with the
        biggest difference between Log2CPM values between all samples are listed as
        'The most variable' and are shown in the heatmap."
  informationBox(infoText)
})

output[["dge_heat_info"]] <- renderUI({
  infoText <- "This heatmap is based on the most differentially expressed genes. This 
        is defined by sorting all genes based on the adjusted p-value. The genes with
        the lowest adjusted p-values are considered 'most expressed'."
  informationBox(infoText)
})
