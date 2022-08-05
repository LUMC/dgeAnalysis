
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
    
    ## Only plot if UI is loaded
    if(is.null(input$norm_line_color)) {
      break
    }
    
    ## Get input data
    plot_data <- count_dist(inUse_normDge)
    
    ## Create plot
    line_plot(
      df = plot_data,
      x = "x",
      y = "y",
      group = input$norm_line_color,
      title = "Gene count distribution",
      xlab = "Log2CPM",
      ylab = "Density"
    )
  }, error = function(err) {
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

## Normalized distribution plot violin
output[["norm_dist_violin"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$norm_violin_group)) {
      break
    }
    
    ## Get input data
    plot_data <- violin_dist(inUse_normDge)
    
    ## Create plot
    violin_plot(
      df = plot_data,
      group = input$norm_violin_group,
      title = "Gene count distribution",
      xlab = "",
      ylab = "Log2CPM"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Select a group to group violin plot
output[["norm_violin_group"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "norm_violin_group",
      label = "Group by:",
      choices = c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized voom plot
output[["norm_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    plot_data <- voom_data(inUse_normDge)
    index <- round(seq(1, nrow(plot_data), length.out = 1000))
    
    ## Create plot
    toWebGL(ggplotly(
      scatter_plot(
        df = plot_data,
        group = "Genes",
        key = "gene",
        index = index,
        x = "x",
        y = "y",
        title = "Voom Plot",
        xlab = "Average Log2 count",
        ylab = "SQRT (Standart Deviation)"
      ),
      source = "norm_voom"
    ))
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

output[["norm_dist_violin_info"]] <- renderUI({
  infoText <-
    "The violin plot serves a similar purpose to the line plot, but the data can be viewed in a different way
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
