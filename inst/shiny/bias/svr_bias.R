
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

## dropdown with all gc choices
output[["selectGC"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput("selectGC", "Show bias based on:",
                grep('gc$', colnames(inUse_deTab), value=TRUE, ignore.case=TRUE)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## dropdown with all length choices
output[["selectLength"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput("selectLength", "Show bias based on:",
                grep('length$', colnames(inUse_deTab), value=TRUE, ignore.case=TRUE)
    )
  }, error = function(err) {
    return(NULL)
  })
})
