
## Sample Tree
output[["wgcna_sample_tree"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    sampleTree = hclust(dist(t(inUse_normDge$counts)), method = "average")
    plot_dendro_new(sampleTree, inUse_normDge$samples[[input$color_wgcna_tree]])
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of Sample Tree
output[["color_wgcna_tree"]] <- renderUI({
  tryCatch({
    selectInput("color_wgcna_tree", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Power plot
output[["wgcna_power_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    if (input$setPowerTab == "power") {
      plot_power(get_power(), input$power_cutoff)
    } else {
      plot_soft(get_power())
    }
  }, error = function(err) {
    return(NULL)
  })
})

get_power <- reactive({
  tryCatch({
    showModal(
      modalDialog(
        h1("Running soft threshold calculations..."),
        h4("This can take a while..."),
        img(src="loading.gif", width = "50%"),
        footer=NULL
      )
    )
    
    powers <- c(c(1:input$power_numberOf))
    ## invisible(capture.output(soft <- pickSoftThreshold(t(inUse_normDge$counts), powerVector = 20, verbose = 5)))
    soft <- pickSoftThreshold(t(inUse_normDge$counts), powerVector = powers, verbose = 5)
    removeModal()
    soft
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["wgcna_tree_info"]] <- renderUI({
  infoText <- "Sample Tree plot colored by sample sheet value"
  informationBox(infoText)
})

output[["wgcna_power_info"]] <- renderUI({
  infoText <- "Power text"
  informationBox(infoText)
})
