
## Sample Tree
output[["wgcna_sample_tree"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    sampleTree <- hclust(dist(t(inUse_normDge$counts)), method = "average")
    plotly_dendrogram(sampleTree, inUse_normDge$samples[[input$color_wgcna_tree]], NA)
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

## Trait heatmap
output[["wgcna_trait_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    trait <- data_samples()[, unlist(lapply(data_samples(), is.numeric)), drop = FALSE]
    plot_dendro_heat(trait)
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

## Calculate power values with soft threshold
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
    soft <- pickSoftThreshold(t(inUse_normDge$counts), powerVector = powers, verbose = 2)
    removeModal()
    soft
  }, error = function(err) {
    removeModal()
    return(NULL)
  })
})

## Dendrogram with modules
output[["wgcna_dendro_module"]] <- renderPlotly({
  tryCatch({
    checkReload()
    showModal(
      modalDialog(
        h1("Running TOM calculations..."),
        h4("This can take a while..."),
        img(src="loading.gif", width = "50%"),
        footer=NULL
      )
    )
    
    dendrogram <- get_TOM_manual()
    modules <- get_modules()[dendrogram$order]
    removeModal()
    p1 <- plotly_dendrogram(dendrogram, NULL, modules)
    p2 <- plot_modules(modules)
    subplot(
      list(p1, p2),
      nrows = 2,
      heights = c(0.9, 0.1)
    )
  }, error = function(err) {
    removeModal()
    return(NULL)
  })
})

## Number of genes for WGCNA
output[["wgcna_number_genes"]] <- renderUI({
  tryCatch({
    checkReload()
    
    max_genes <- nrow(inUse_normDge)
    if (max_genes > 5000) {
      max_genes <- 5000
    }
    sliderInput("wgcna_number_genes", "Number of genes", 1000, min = 10, max = max_genes, step = 10)
  }, error = function(err) {
    return(NULL)
  })
})

## Calculate TOMsimilarityFromExpr most variable manually
get_TOM_manual <- reactive({
  lcpm <- inUse_normDge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing=TRUE))[1:input$wgcna_number_genes]
  high_var_cpm <- as.data.frame(lcpm[select_var,])
  
  SubGeneNames <- rownames(high_var_cpm)
  adjacency <- adjacency(t(high_var_cpm), type = "signed", power = 6)
  TOM <- TOMsimilarityFromExpr(adjacency, networkType = "signed", TOMType = "signed", power = 6)
  colnames(TOM) <- rownames(TOM) <- SubGeneNames
  dissTOM <- 1-TOM
  
  geneTree <- hclust(as.dist(dissTOM), method = "average")
})

## Calculate TOMsimilarityFromExpr in blocks automatic
get_TOM_auto <- reactive({
  tryCatch({
    net <- blockwiseModules(t(inUse_normDge$counts), power = input$module_power,
                            TOMType = "unsigned", minModuleSize = 30,
                            reassignThreshold = 0, mergeCutHeight = 0.25,
                            numericLabels = TRUE, pamRespectsDendro = FALSE,
                            verbose = 3)
    net
  }, error = function(err) {
    return(NULL)
  })
})

## Get module colors from TOM
get_modules <- reactive({
  tryCatch({
    geneTree <- get_TOM_manual()
    dynamicMods <- cutreeDynamic(dendro = geneTree, method="tree", minClusterSize = 30)
    dynamicColors <- labels2colors(dynamicMods, colorSeq = setdiff(standardColors(), "black"))
    dynamicColors
  }, error = function(err) {
    return(NULL)
  })
})


## INFORMATION BOXES

output[["wgcna_tree_info"]] <- renderUI({
  infoText <- "Sample Tree plot colored by sample sheet value"
  informationBox(infoText)
})

output[["wgcna_trait_info"]] <- renderUI({
  infoText <- "Trait is retrieved from sample sheet (only numeric columns)"
  informationBox(infoText)
})

output[["wgcna_power_info"]] <- renderUI({
  infoText <- "Power text"
  informationBox(infoText)
})

output[["wgcna_module_info"]] <- renderUI({
  infoText <- "Module dendrogram"
  informationBox(infoText)
})
