
## Variable heatmap
output[["var_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$slider_heatmap_var)) {
      break
    }
    
    ## Get input data
    plot_data <- heat_var(inUse_normDge, input$slider_heatmap_var)
    text <- 'paste("Sample:", col,
                  "\nGene:", row,
                  "\nLog2CPM:", round(value, 2))'
    
    ## Create plot
    ggplotly(
      heatmap_plot(
        df = plot_data,
        x = "col",
        y = "row",
        text = text,
        group = input$group_var,
        fill = "value",
        title = "Most variable genes",
        xlab = "",
        ylab = ""
      ) + labs(fill = "Log2CPM"),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set amount of variable genes to show
output[["heatmap_var_ngenes"]] <- renderUI({
  tryCatch({
    sliderInput(
      inputId = "slider_heatmap_var",
      label = "Set the number of genes to show:",
      value = 100,
      min = 2,
      max = nrow(inUse_normDge),
      step = 1
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Select a group to sort var heatmap
output[["group_var"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "group_var",
      label = "Group by:",
      choices = c("None" = "none", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## DGE heatmap
output[["dge_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    if (is.null(inUse_deTab)) {
      return(NULL)
    }
    
    ## Only plot if UI is loaded
    if(is.null(input$slider_heatmap_dge)) {
      break
    }
    
    ## Get input data
    plot_data <- heat_de(inUse_normDge, inUse_deTab, input$slider_heatmap_dge)
    text <- 'paste("Sample:", col,
                  "\nGene:", row,
                  "\nLog2CPM:", round(value, 2))'
    
    ## Create plot
    ggplotly(
      heatmap_plot(
        df = plot_data,
        x = "col",
        y = "row",
        text = text,
        group = input$group_dge,
        fill = "value",
        title = "Most DE genes",
        xlab = "",
        ylab = ""
      ) + labs(fill = "Log2CPM"),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Select a group to sort dge heatmap
output[["group_dge"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "group_dge",
      label = "Group by:",
      choices = c("None" = "none", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set amount of DGE genes to show
output[["heatmap_dge_ngenes"]] <- renderUI({
  tryCatch({
    sliderInput(
      inputId = "slider_heatmap_dge",
      label = "Set the number of genes to show:",
      value = 100,
      min = 2,
      max = nrow(inUse_normDge),
      step = 1
    )
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["var_heat_info"]] <- renderUI({
  infoText <-
    "This heatmap is based on the most variable genes. This is calculated
        by getting the average Log2CPM difference of a gene between all samples. the
        genes are then sorted based on the calculated difference. The genes with the
        greatest difference between Log2CPM values between all samples are reported as
        'The most variable' and are shown in the heatmap."
  informationBox(infoText)
})

output[["dge_heat_info"]] <- renderUI({
  infoText <-
    "This heatmap is based on the most differentially expressed genes. this
        is defined by sorting all genes based on the adjusted p-value. The genes with
        the lowest adjusted p-values are considered 'most expressed'."
  informationBox(infoText)
})
