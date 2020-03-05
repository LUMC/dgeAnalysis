
## Distribution plot line
output[["dist_line"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionLinePlot(get_raw_dge())
  }, error = function(err) {
    return(NULL)
  })
})

## Distribution plot boxplot
output[["dist_boxplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    countDistributionBoxPlot(get_raw_dge())
  }, error = function(err) {
    return(NULL)
  })
})

## Make dge voom ready
get_pre_voom <- reactive({
  dge <- get_raw_dge()
  dge$counts <- dge$counts[!grepl('^__', rownames(dge$counts)),]
  selectedFeatures <- rownames( dge$counts )[ apply( dge$counts, 1, function( v ) sum( v >= input$slider_raw_voom ) ) >= 1/4 * ncol( dge$counts ) ]
  highExprDge <- dge[ selectedFeatures,, keep.lib.sizes = FALSE ]
  highExprDge
})

## Voom plot
output[["raw_voom_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    dge <- get_raw_dge()
    dge$counts <- dge$counts[!grepl('^__', rownames(dge$counts)),]
    selectedFeatures <- rownames( dge$counts )[ apply( dge$counts, 1, function( v ) sum( v >= input$slider_raw_voom ) ) >= 1/4 * ncol( dge$counts ) ]
    highExprDge <- dge[ selectedFeatures,, keep.lib.sizes = FALSE ]
    
    voomPlot(highExprDge, "raw_voom")
  }, error = function(err) {
    return(NULL)
  })
})

## Show amount of genes ledt after filtering
output[["raw_voom_ngenes"]] <- renderUI({
  tryCatch({
    h2("After filtering:", br(), nrow(get_pre_voom()), "Genes")
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points raw_voom_plot
output[["selected_raw_voom"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_voom")
    counts <- data.frame(get_raw_dge()$counts[!grepl('^__', rownames(get_raw_dge()$counts)),])
    DT::datatable(counts[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Multidimensional scaling 2D
output[["un_cluster_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling2dPlot(get_raw_dge(), input$group_raw_mds2d, "raw_mds2d")
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds 2d
output[["group_raw_mds2d"]] <- renderUI({
  tryCatch({
    selectInput("group_raw_mds2d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points un_cluster_2d
output[["selected_raw_mds2d"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "raw_mds2d")
    if(is.null(s)){s <- ""}
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available")), rownames = FALSE, colnames = ""))
  })
})

## Multidimensional scaling 3D
output[["un_cluster_3d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    multidimensionalScaling3dPlot(get_raw_dge(), input$group_raw_mds3d)
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds 2d
output[["group_raw_mds3d"]] <- renderUI({
  tryCatch({
    selectInput("group_raw_mds3d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})
