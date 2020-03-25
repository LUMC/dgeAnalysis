
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
    complexityPlot(get_secounts(), perc, input$comp_rank)
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["align_sum_info"]] <- renderUI({
  infoText <- "In the summary plots, a general overview can be seen of the count data. These plots show a
        bar plot with the alignment results and a complexity plot. The alignment plot is a stacked bar
        plot, where every 'row' stands for a sample and the stacked bars per row are the different
        tags. These tags indicate the distribution of the read counts, given by a read counter (e.g.
        HTSeq)."
  informationBox(infoText)
})

output[["complex_info"]] <- renderUI({
  infoText <- "The complexity plots show how many reads cover a certain amount of genes. On the X-axis,
        there is a ranking from zero to one thousand. This ranking stands for the number of genes.
        On the Y-axis the proportion of the reads relative to the total amount of counts is shown.
        Normally the expectation is a growing curve. Big deviations between samples of the number
        of reads for a gene, will be visible."
  informationBox(infoText)
})