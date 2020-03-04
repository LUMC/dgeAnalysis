
## Set deTab table
output[["detab_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    if (input$setdeTab == "all") {
      DT::datatable(inUse_deTab, options = list(pageLength = 50, scrollX = TRUE))
    } else {
      DT::datatable(inUse_deTab[inUse_deTab$DE != 0,], options = list(pageLength = 50, scrollX = TRUE))
    }
  }, error = function(err) {
    return(NULL)
  })
})

## DE ratio
output[["de_ratio"]] <- renderPlotly({
  tryCatch({
    checkReload()
    deRatioPlot(inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})

output[["selected_ma"]] <- DT::renderDataTable({
  s <- event_data(event = "plotly_selected", source = "analysis_ma")
  DT::datatable(inUse_deTab[s$key,], options = list(pageLength = 15, scrollX = TRUE))
})

## Mean-Difference (MA) plots
output[["ma_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    ma_plot(inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})

output[["selected_volcano"]] <- DT::renderDataTable({
  s <- event_data(event = "plotly_selected", source = "analysis_volcano")
  DT::datatable(inUse_deTab[s$key,], options = list(pageLength = 15, scrollX = TRUE))
})

## Volcano plots
output[["volcano_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    volcanoPlot(inUse_deTab, input$vulcanoLogCut, -log10(input$vulcanoPCut))
  }, error = function(err) {
    return(NULL)
  })
})

## Barcode plot
output[["barcode_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    barcodePlot(inUse_deTab, inUse_normDge, input$group_by4)
  }, error = function(err) {
    print(err)
    return(NULL)
  })
})

## P value plots
output[["p_val_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    pValuePlot(inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})
