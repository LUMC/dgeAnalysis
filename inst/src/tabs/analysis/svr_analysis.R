
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
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## DE ratio
output[["de_ratio"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    plot_data <- de_ratio(inUse_deTab)
    
    ## Create plot
    bar_plot(
      df = plot_data,
      group = "Var1",
      x = "Var1",
      y = "perc",
      fill = "Var1",
      title = "Differential expression ratio",
      xlab = "",
      ylab = "Ratio"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Mean-Difference (MA) plots
output[["ma_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    index <- round(seq(1, nrow(inUse_deTab), length.out = 1000))
    plot_data <- ma(inUse_deTab)
    
    ## Create plot
    toWebGL(
      scatter_plot(
        df = plot_data,
        source = "analysis_ma",
        group = "DE",
        key = "gene",
        index = index,
        x = "avgLog2CPM",
        y = "avgLog2FC",
        title = "MA Plot",
        xlab = "Average Log2CPM",
        ylab = "Average Log2FC"
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Genes selected in MA plot
output[["selected_ma"]] <- DT::renderDataTable({
  tryCatch({
    s <- event_data(event = "plotly_selected", source = "analysis_ma")
    DT::datatable(inUse_deTab[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Volcano plots
output[["volcano_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    plot_data <- volcano(inUse_deTab)
    
    ## Create plot
    toWebGL(
      scatter_plot(
        df = plot_data,
        source = "analysis_volcano",
        group = "DE",
        key = "gene",
        x = "avgLog2FC",
        y = "FDR",
        title = "Volcano Plot",
        xlab = "Average Log2FC",
        ylab = "-Log10 P-value (FDR)"
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Genes selected in volcano plot
output[["selected_volcano"]] <- DT::renderDataTable({
  tryCatch({
    s <- event_data(event = "plotly_selected", source = "analysis_volcano")
    DT::datatable(inUse_deTab[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Barcode plot
output[["barcode_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    plot_data <- barcode(inUse_deTab, input$slider_barcode, input$selected_analysis_bar)
    
    ## Create plot
    toWebGL(
      barcode_plot(
        df = plot_data,
        group = input$group_analysis_bar,
        x = "value",
        y = "row",
        title = "Barcode Plot",
        xlab = "Log2CPM",
        ylab = ""
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of barcode
output[["group_analysis_bar"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "group_analysis_bar",
      label = "Color by:",
      choices = c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific gene to barplot
observe({
  tryCatch({
    checkReload()
    updateSelectizeInput(
      session = session,
      inputId = "selected_analysis_bar",
      choices = rownames(inUse_deTab),
      server = TRUE
    )
  }, error = function(err) {
    return(NULL)
  })
})

## P value plots
output[["p_val_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    plot_data <- pvalue(inUse_deTab)
    
    ## Create plot
    bar_plot(
      df = plot_data,
      x = "p",
      y = "x",
      title = "P-Value plot",
      xlab = "P-Value",
      ylab = "Count"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["de_ratio_info"]] <- renderUI({
  infoText <-
    "The DE ratio plot provides a quick overview of the amount of differentially expressed genes.
        (down, unregulated and up regulated) The exact number of genes is shown in the plot. the spread
        of the Y-axis is based on the percentages. This is calculated based on the total number of genes
        after filtering."
  informationBox(infoText)
})

output[["ma_plot_info"]] <- renderUI({
  infoText <-
    "The MA plot, plots all genes after filtering. This plot creates a figure with
        log intensity ratios (M values) and log intensity means (A values). The MA plot can give a
        overview of all genes and their amount of expression in a comparison of two groups. on the
        X-axis plots the Log2CPM values (A) against the Log2FC values on the Y-axis (M). In
        in this plot the genes are stained based on the DE results (down, unregulated and upregulated). so,
        the trend of expression is shown. This trend shows the average expression flow."
  informationBox(infoText)
})

output[["volcano_plot_info"]] <- renderUI({
  infoText <-
    "The volcano plot also shows the most expressed genes and can give a good idea of the
        expression results. The X-axis contains the Log2FC (magnitude of change) and the Y-axis
        shows -Log10 p-value (statistical significance). A major advantage of the volcano plot is that it
        makes genes with a high fold-change (which are statistically significant) clearly visible. As genes
        are down-regulated, they will be visible on the left side of the plot and up-regulated genes
        more to the right. The most statistically significant genes are placed at the top."
  informationBox(infoText)
})

output[["barcode_plot_info"]] <- renderUI({
  infoText <-
    "The barcode plot shows the most expressed genes. The X-axis shows the Log2CPM
        for each sample per gene. The Y-axis shows the most expressed genes (from top to
        bottom). Each bar represents a sample. With this plot, differences between samples for a
        specific gene is visible."
  informationBox(infoText)
})

output[["p_val_plot_info"]] <- renderUI({
  infoText <-
    "The p-value plot is used as a quality control plot. For example, if all bars are all
        same height, the analysis can be described as “noisy”. The ideal pattern would be that
        bars around a p-value of zero are high. Then the bars have to be in a nice (steep)
        curve and should be (almost) zero at the p-value of one."
  informationBox(infoText)
})
