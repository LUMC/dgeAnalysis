
## Distribution plot line
output[["dist_line"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    dge <- get_raw_dge()
    plot_data <- count_dist(dge)
    
    ## Create plot
    line_plot(
      df = plot_data,
      x = "x",
      y = "y",
      group = input$raw_line_color,
      title = "Gene count distribution",
      xlab = "Log2CPM",
      ylab = "Density"
    )
  }, error = function(err) {
    print(err)
    return(NULL)
  })
})

## Select a group to color line plot
output[["raw_line_color"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "raw_line_color",
      label = "Group by:",
      choices = c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Distribution plot boxplot
output[["dist_boxplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    dge <- get_raw_dge()
    plot_data <- violin_dist(dge)
    
    ## Create plot
    violin_plot(
      df = plot_data,
      group = "sample",
      title = "Gene count distribution",
      xlab = "",
      ylab = "Log2CPM"
    )
  }, error = function(err) {
    return(NULL)
  })
})


## Multidimensional scaling
output[["un_cluster"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    dge <- get_raw_dge()
    plot_data <- mds_clust(dge)
    
    ## Create plot
    scatter_plot(
      df = plot_data,
      size = 4,
      source = "raw_mds",
      key = "sample",
      x = "x",
      y = "y",
      group = input$group_raw_mds,
      title = "MDS Plot",
      xlab = "MDS 1",
      ylab = "MDS 2"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds
output[["group_raw_mds"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "group_raw_mds",
      label = "Color by:",
      choices =c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points un_cluster
output[["selected_raw_mds"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_mds")
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    print(err)
    return(DT::datatable(data.frame(c(
      "No data available"
    )), rownames = FALSE, colnames = ""))
  })
})

## INFORMATION BOXES

output[["dist_line_info"]] <- renderUI({
  infoText <-
    "The line plot shows the density of the log2CPM values per sample. The density shows the
        distribution of counts per sample and is used to detect large differences between
        samples."
  informationBox(infoText)
})

output[["dist_boxplot_info"]] <- renderUI({
  infoText <-
    "The box plot serves a similar purpose as the line plot, but the data can be viewed in a different way
        format. The distribution can be seen between the Log2CPM at the corresponding
        samples."
  informationBox(infoText)
})

output[["un_cluster_info"]] <- renderUI({
  infoText <-
    "This MDS plot (multidimensional scaling plot) can be viewed as a 2D plot with
        calculations of two dimensions. With the MDS plotting distances between samples, samples
        shown, based on similarities and differences."
  informationBox(infoText)
})
