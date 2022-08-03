
## Variable heatmap
output[["var_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    lcpm <- inUse_normDge$counts
    var_genes <- apply(lcpm, 1, var)
    select_var <- names(sort(var_genes, decreasing = TRUE))[1:input$slider_heatmap_var]
    high_var_cpm <- lcpm[select_var, ]
    high_var_cpm <- as.data.frame(stack(high_var_cpm))
    high_var_cpm <- merge(
      x = high_var_cpm,
      y = as.data.frame(inUse_normDge$samples),
      by.x = "col",
      by.y = "row.names",
      all.x = TRUE
    )
    
    heatmap_plot(
      df = high_var_cpm,
      group = input$group_var,
      title = "Most variable genes",
      xlab = "",
      ylab = ""
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
    
    sortdeTab <- inUse_deTab[order(rank(inUse_deTab$FDR)), ]
    sortdeTab <- head(sortdeTab, input$slider_heatmap_dge)
    getnorm <- inUse_normDge[rownames(sortdeTab), ]
    getnorm <- getnorm$counts
    getnorm <- as.data.frame(stack(getnorm))
    
    getnorm <- merge(
      x = getnorm,
      y = as.data.frame(inUse_normDge$samples),
      by.x = "col",
      by.y = "row.names",
      all.x = TRUE
    )
    
    heatmap_plot(
      df = getnorm,
      group = input$group_dge,
      title = "Most DE genes",
      xlab = "",
      ylab = ""
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
