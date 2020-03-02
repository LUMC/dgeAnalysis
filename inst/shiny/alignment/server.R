
## get se_counts from table
get_secounts <- reactive({
  se_counts <- readCountsFromTable(data_counts(),
                                   data_samples())
  se_counts
})

## Create alignment summary plot
output[["align_sum"]] <- renderPlotly({
  tryCatch({
    checkReload()
    se <- get_secounts()
    save(se, file="test.RData")
    alignmentSummaryPlot(get_secounts(), perc = F)
  }, error = function(err) {
    return(NULL)
  })
})

## Create alignment summary percentage plot
output[["align_sum_perc"]] <- renderPlotly({
  tryCatch({
    checkReload()
    alignmentSummaryPlot(get_secounts())
  }, error = function(err) {
    return(NULL)
  })
})

## Create complexity plot
output[["complex"]] <- renderPlotly({
  tryCatch({
    checkReload()
    complexityPlot(get_secounts(), perc = F)
  }, error = function(err) {
    return(NULL)
  })
})

## Create complexity percentage plot
output[["complex_perc"]] <- renderPlotly({
  tryCatch({
    checkReload()
    complexityPlot(get_secounts())
  }, error = function(err) {
    return(NULL)
  })
})
