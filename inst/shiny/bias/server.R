
## GC bias
output[["gc_bias"]] <- renderPlotly({
  tryCatch({
    checkReload()
    biasPlot(inUse_deTab, input$selectGC, NA)
  }, error = function(err) {
    return(NULL)
  })
})

## feature length bias
output[["len_bias"]] <- renderPlotly({
  tryCatch({
    checkReload()
    biasPlot(inUse_deTab, input$selectLength, "log")
  }, error = function(err) {
    return(NULL)
  })
})
