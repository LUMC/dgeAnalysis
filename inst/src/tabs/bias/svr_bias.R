
## GC bias
output[["gc_bias"]] <- renderPlotly({
  tryCatch({
    checkReload()
    biasPlot(inUse_deTab, input$selectGC, NA)
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

## feature length bias
output[["len_bias"]] <- renderPlotly({
  tryCatch({
    checkReload()
    biasPlot(inUse_deTab, input$selectLength, "log")
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

## INFORMATION BOXES

output[["gc_bias_info"]] <- renderUI({
  infoText <- "The GC bias plots are generated based on the average log fold change on the Y-axis. The bias plots
        can only be used if an annotation is used to perform the analysis. If annotation values are
        present, a column can be selected with the use of a drop-down button. The columns should
        contain the text: 'GC', otherwise they will not be recognized as a column suitable
        for GC bias calculations."
  informationBox(infoText)
})

output[["len_bias_info"]] <- renderUI({
  infoText <- "The feature-length bias plots are generated based on the average log fold change on the Y-axis. The bias plots
        can only be used if an annotation is used to perform the analysis. If annotation values are
        present, a column can be selected with the use of a drop-down button. The columns should
        contain the text: 'length', otherwise they will not be recognized as a column suitable
        for length bias calculations."
  informationBox(infoText)
})
