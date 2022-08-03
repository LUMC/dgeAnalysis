
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
    
    stackCounts <- data.frame(stackDge(inUse_normDge))
    stackCounts <- merge(
      x = stackCounts,
      y = inUse_normDge$samples,
      by.x = "sample",
      by.y = "row.names",
      all.x = TRUE
    )
    
    density_plot(
      df = stackCounts,
      group = input$norm_line_color,
      title = "Gene count distribution",
      x = "Log2CPM",
      y = "Density"
    )
  }, error = function(err) {
    print(err)
    return(NULL)
  })
})

## Select a group to color line plot
output[["norm_line_color"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "norm_line_color",
      label = "Group by:",
      choices = c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized distribution plot boxplot
output[["norm_dist_boxplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    stackCounts <- data.frame(stackDge(inUse_normDge))
    
    violin_plot(
      df = stackCounts,
      group = "sample",
      title = "Gene count distribution",
      x = "Log2CPM",
      y = ""
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized voom plot
output[["norm_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    v <- voom(2 ^ (inUse_normDge$counts), save.plot = TRUE)
    v <- data.frame(
      x = v$voom.xy$x,
      y = v$voom.xy$y,
      gene = names(v$voom.xy$x),
      Genes = "Genes"
    )
    v <- v[order(v$x), ]
    index <- round(seq(1, nrow(v), length.out = 1000))
    
    toWebGL(
      scatter_plot(
        df = v,
        source = "norm_voom",
        group = "Genes",
        key = "gene",
        index = index,
        x = "x",
        y = "y",
        title = "Voom Plot",
        xlab = "Average Log2 count",
        ylab = "SQRT (Standart Deviation)"
      )
    )
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
        distribution of counts per sample and is used to detect large differences between
        samples."
  informationBox(infoText)
})

output[["norm_dist_boxplot_info"]] <- renderUI({
  infoText <-
    "The box plot serves a similar purpose to the line plot, but the data can be viewed in a different way
        format. The distribution can be seen between the Log2CPM at the corresponding
        samples."
  informationBox(infoText)
})

output[["norm_voom_plot_info"]] <- renderUI({
  infoText <-
    "The voom plot provides a check on the filtering, which is performed at the beginning of the
        analysis. The method to calculate this is 'voom'. Voom is an acronym for
        mean variance modeling at the observational level. This means that the mean variance in
        the data is calculated and gives each observation a certain weight. Problems during the
        filtering of low expressed genes will be visible in this plot."
  informationBox(infoText)
})
