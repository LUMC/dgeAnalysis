
## Create table with normalized counts
output[["normalized_counts"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    DT::datatable(inUse_normDge$counts,
                  options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Normalized distribution plot line
output[["norm_dist_line"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionLinePlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized distribution plot boxplot
output[["norm_dist_boxplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionBoxPlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized voom plot
output[["norm_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    voomPlot(inUse_normDge, "norm_voom")
  }, error = function(err) {
    return(NULL)
  })
})

## Show amount of genes left after filtering
output[["norm_voom_ngenes"]] <- renderUI({
  tryCatch({
    checkReload()
    h2("After filtering:", br(), nrow(normDge$counts), "Genes")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points norm_voom_plot
output[["selected_norm_voom"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "norm_voom")
    DT::datatable(data.frame(inUse_normDge$counts)[unlist(s$key), ],
                  options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})


## INFORMATION BOXES

output[["norm_dist_line_info"]] <- renderUI({
  infoText <-
    "The line plot shows the density of the log2CPM values per sample. The density shows the
  distribution of count values per sample and is used to determine big differences between
  samples."
  informationBox(infoText)
})

output[["norm_dist_boxplot_info"]] <- renderUI({
  infoText <-
    "The box plot has a similar purpose, but the data can be viewed in a different
  format. The distribution can be seen between the Log2CPM at the corresponding
  samples."
  informationBox(infoText)
})

output[["norm_voom_plot_info"]] <- renderUI({
  infoText <-
    "The voom plot provides a check on the filtering, which is performed at the beginning of the
  analysis. The method used to calculate this is 'voom'. Voom is an acronym for
  mean-variance modeling at the observational level. This means that the mean-variance in
  the data is calculated and gives each observation a certain weight. Problems during the
  filtering of low expressed genes will be visible in this plot."
  informationBox(infoText)
})
