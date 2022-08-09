
## Distribution plot line
output[["dist_line"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$raw_line_color)) {
      break
    }
    
    ## Get input data
    dge <- get_raw_dge()
    plot_data <- count_dist(dge)
    text <- 'paste("Sample:", sample,
                  "\nLog2CPM:", round(x, 2))'
    
    ## Create plot
    ggplotly(
      line_plot(
        df = plot_data,
        x = "x",
        y = "y",
        text = text,
        group = input$raw_line_color,
        title = "Gene count distribution",
        xlab = "Log2CPM",
        ylab = "Density"
      ),
      tooltip = "text"
    )
  }, error = function(err) {
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

## Distribution plot violin
output[["dist_violin"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$raw_violin_group)) {
      break
    }
    
    ## Get input data
    dge <- get_raw_dge()
    plot_data <- violin_dist(dge, input$raw_violin_group)
    text <- 'paste("Sample:", sample)'
    
    ## Create plot
    gg <- ggplotly(
      violin_plot(
        df = plot_data,
        text = text,
        group = input$raw_violin_group,
        title = "Gene count distribution",
        xlab = "",
        ylab = "Log2CPM"
      ),
      tooltip = "text"
    )
    
    ## Fix labels & plot
    fix_violin_hover(gg)
  }, error = function(err) {
    return(NULL)
  })
})

## Select a group to group violin plot
output[["raw_violin_group"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "raw_violin_group",
      label = "Group by:",
      choices = c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})


## Multidimensional scaling
output[["un_cluster"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$group_raw_mds)) {
      break
    }
    
    ## Get input data
    dge <- get_raw_dge()
    plot_data <- mds_clust(dge)
    text <- 'paste("Sample:", sample)'
    
    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        x = "x",
        y = "y",
        text = text,
        group = input$group_raw_mds,
        size = 5,
        key = "sample",
        title = "MDS Plot",
        xlab = "MDS 1",
        ylab = "MDS 2"
      ),
      source = "raw_mds",
      tooltip = "text"
    ) %>% layout(dragmode = "select", clickmode = "event+select")
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
    if (is.null(s)) {
      throw()
    }
    
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
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

output[["dist_violin_info"]] <- renderUI({
  infoText <-
    "The violin plot serves a similar purpose as the line plot, but the data can be viewed in a different way
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
