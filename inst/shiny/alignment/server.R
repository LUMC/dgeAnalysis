
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
    if (input$setSummary == "actual"){
      perc = F
    } else {
      perc = T
    }
    alignmentSummaryPlot(get_secounts(), perc)
  }, error = function(err) {
    return(NULL)
  })
})

## Create complexity plot
output[["complex"]] <- renderPlotly({
  tryCatch({
    checkReload()
    if (input$setComplexity == "actual"){
      perc = F
    } else {
      perc = T
    }
    complexityPlot(get_secounts(), perc)
  }, error = function(err) {
    return(NULL)
  })
})
