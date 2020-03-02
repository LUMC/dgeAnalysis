
## get deTab from table
get_deTab <- reactive({
  deTab <- data_deTab()
  deTab
})

## Selected data points plots
output[["selected_clicked_plots"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    ids <- unique(c(input$normalized_counts_rows_selected,
                    input$all_genes_table_rows_selected,
                    input$deg_table_rows_selected))
    s <- event_data(event = "plotly_selected", source = "analysis_plots")
    
    table_select <- rownames(inUse_deTab[ids,])
    plot_select <- append(table_select, s$key)
    DT::datatable(inUse_deTab[plot_select,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Voom plot
output[["voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    ids <- unique(c(input$normalized_counts_rows_selected,
                    input$all_genes_table_rows_selected,
                    input$deg_table_rows_selected))
    s <- event_data(event = "plotly_selected", source = "analysis_plots")
    
    table_select <- rownames(inUse_deTab[ids,])
    plot_select <- append(s$key, table_select)
    voomPlot(inUse_normDge, inUse_deTab, plot_select)
  }, error = function(err) {
    return(NULL)
  })
})

## Residual variances :::: NOT IN AT THE MOMENT
output[["res_var"]] <- renderPlotly({
  tryCatch({
    checkReload()
    if (isTRUE(input$choose_analysis)) {
      residualVariancePlot(deTab)
    } else {
      residualVariancePlot(get_deTab())
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

## Mean-Difference (MA) plots
output[["ma_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    ids <- unique(c(input$normalized_counts_rows_selected,
                    input$all_genes_table_rows_selected,
                    input$deg_table_rows_selected))
    s <- event_data(event = "plotly_selected", source = "analysis_plots")
    
    table_select <- rownames(inUse_deTab[ids,])
    plot_select <- append(s$key, table_select)
    ma_plot(inUse_deTab, plot_select)
  }, error = function(err) {
    return(NULL)
  })
})

## Volcano plots
output[["volcano_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    ids <- unique(c(input$normalized_counts_rows_selected,
                    input$all_genes_table_rows_selected,
                    input$deg_table_rows_selected))
    s <- event_data(event = "plotly_selected", source = "analysis_plots")
    
    table_select <- rownames(inUse_deTab[ids,])
    plot_select <- append(s$key, table_select)
    volcanoPlot(inUse_deTab, input$vulcanoLogCut, -log10(input$vulcanoPCut), plot_select)
  }, error = function(err) {
    return(NULL)
  })
})

## Barcode plot
output[["barcode_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    ids <- unique(c(input$normalized_counts_rows_selected,
                    input$all_genes_table_rows_selected,
                    input$deg_table_rows_selected))
    s <- event_data(event = "plotly_selected", source = "analysis_plots")
    
    table_select <- rownames(inUse_deTab[ids,])
    plot_select <- append(s$key, table_select)
    barcodePlot(inUse_deTab, inUse_normDge, input$group_by4, plot_select)
  }, error = function(err) {
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
