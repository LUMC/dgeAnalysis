
## variance PCA
output[["variance_pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    variancePcaPlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## samples PCA 2D
output[["samples_pca_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    samplePca2dPlot(inUse_normDge, input$group_by3)
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points samples_pca_2d
output[["samples_pca_2d_clicked"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "samples_pca_2d")
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## samples PCA 3D
output$samples_pca_3d <- renderPlotly({
  tryCatch({
    checkReload()
    samplePca3dPlot(inUse_normDge, input$group_by3)
  }, error = function(err) {
    return(NULL)
  })
})
