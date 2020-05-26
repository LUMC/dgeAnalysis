
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
    plot_trait_heat(trait)
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
  showModal(
    modalDialog(
      h1("Running soft threshold calculations..."),
      h4("This can take a while..."),
      img(src="loading.gif", width = "50%"),
      footer=NULL
    )
  )
  
  tryCatch({
    powers <- c(c(1:input$power_numberOf))
    soft <- pickSoftThreshold(t(inUse_normDge$counts), powerVector = powers, verbose = 2)
    removeModal()
    showNotification(ui = "Power calculations has been succesful!", duration = 5, type = "message")
    soft
  }, error = function(err) {
    removeModal()
    showNotification(ui = "Power calculations failed with an error!", duration = 5, type = "error")
    showNotification(ui = as.character(err), duration = 10, type = "error")
    print(err)
    return(NULL)
  })
})

## Dendrogram with modules
output[["wgcna_dendro_gene_module"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    dissTOM <- get_TOM_manual()
    geneTree <- hclust(as.dist(dissTOM), method = "average")
    dynamicColors <- get_modules()
    
    p1 <- plotly_dendrogram(geneTree, NULL, dynamicColors)
    p2 <- plot_modules(dynamicColors)
    subplot(
      list(p1, p2),
      nrows = 2,
      heights = c(0.9, 0.1)
    )
  }, error = function(err) {
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
  showModal(
    modalDialog(
      h1("Running TOM calculations..."),
      h4("This can take a while..."),
      img(src="loading.gif", width = "50%"),
      footer=NULL
    )
  )
  
  tryCatch({
    if (is.null(input$wgcna_number_genes)) {
      amount <- 1000
    } else {
      amount <- input$wgcna_number_genes
    }
    
    lcpm <- inUse_normDge$counts
    var_genes <- apply(lcpm, 1, var)
    select_var <- names(sort(var_genes, decreasing=TRUE))[1:amount]
    high_var_cpm <- as.data.frame(lcpm[select_var,])
    
    SubGeneNames <- rownames(high_var_cpm)
    adjacency <- adjacency(t(high_var_cpm), type = "signed", power = input$module_power)
    TOM <- TOMsimilarityFromExpr(adjacency, networkType = "signed", TOMType = "signed", power = input$module_power)
    colnames(TOM) <- rownames(TOM) <- SubGeneNames
    dissTOM <- 1-TOM
    
    removeModal()
    showNotification(ui = "TOM calculations has been succesful!", duration = 5, type = "message")
    dissTOM
  }, error = function(err) {
    removeModal()
    showNotification(ui = "TOM calculations failed with an error!", duration = 5, type = "error")
    showNotification(ui = as.character(err), duration = 10, type = "error")
    print(err)
    return(NULL)
  })
})

## Calculate TOMsimilarityFromExpr in blocks automatic (Not available atm)
get_TOM_auto <- reactive({
  showModal(
    modalDialog(
      h1("Running TOM calculations..."),
      h4("This can take a while..."),
      img(src="loading.gif", width = "50%"),
      footer=NULL
    )
  )
  
  tryCatch({
    net <- blockwiseModules(t(inUse_normDge$counts), power = input$module_power,
                            TOMType = "unsigned", minModuleSize = 20,
                            reassignThreshold = 0, mergeCutHeight = 0.25,
                            numericLabels = TRUE, pamRespectsDendro = FALSE,
                            verbose = 3)
    
    removeModal()
    showNotification(ui = "TOM calculations has been succesful!", duration = 5, type = "message")
    net
  }, error = function(err) {
    removeModal()
    showNotification(ui = "TOM calculations failed with an error!", duration = 5, type = "error")
    showNotification(ui = as.character(err), duration = 10, type = "error")
    print(err)
    return(NULL)
  })
})

## Get module colors from TOM
get_modules <- reactive({
  dissTOM <- get_TOM_manual()
  geneTree <- hclust(as.dist(dissTOM), method = "average")
  
  dynamicMods <- cutreeDynamic(dendro = geneTree, method="tree", minClusterSize = 20)
  dynamicColors <- labels2colors(dynamicMods, colorSeq = setdiff(standardColors(), "black"))
  dynamicColors <- dynamicColors[geneTree$order]
  dynamicColors
})

output[["wgcna_dendro_module"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    METree <- get_eigengenes()
    color <- gsub('^.{2}', '', METree$labels)
    color <- color[METree$order]
    plotly_dendrogram(METree, NULL, color)
  }, error = function(err) {
    return(NULL)
  })
})

## Module dendrogram get eigengenes
get_eigengenes <- reactive({
  dynamicColors <- get_modules()
  
  if (is.null(input$wgcna_number_genes)) {
    amount <- 1000
  } else {
    amount <- input$wgcna_number_genes
  }
  
  lcpm <- inUse_normDge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing=TRUE))[1:amount]
  high_var_cpm <- as.data.frame(lcpm[select_var,])
  
  MEList <- moduleEigengenes(t(high_var_cpm), colors = dynamicColors)
  MEs <- MEList$eigengenes
  MEDiss <- 1-cor(MEs)
  METree <- hclust(as.dist(MEDiss), method = "average")
  METree
})

output[["wgcna_module_trait"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    MEs <- get_relation()
    trait <- data_samples()[, unlist(lapply(data_samples(), is.numeric)), drop = FALSE]
    
    moduleTraitCor <- cor(MEs, trait, use = "p")
    moduleTraitPvalue <- corPvalueStudent(moduleTraitCor, ncol(inUse_normDge$counts))
    
    plot_module_trait_relation_heat(moduleTraitCor, moduleTraitPvalue)
  }, error = function(err) {
    return(NULL)
  })
})

## Module dendrogram get eigengenes
get_relation <- reactive({
  dynamicColors <- get_modules()
  
  if (is.null(input$wgcna_number_genes)) {
    amount <- 1000
  } else {
    amount <- input$wgcna_number_genes
  }
  
  lcpm <- inUse_normDge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing=TRUE))[1:amount]
  high_var_cpm <- as.data.frame(lcpm[select_var,])
  
  MEs0 <- moduleEigengenes(t(high_var_cpm), dynamicColors)$eigengenes
  MEs <- orderMEs(MEs0)
  MEs
})

output[["wgcna_network_heat"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    data_TOM <- get_dendro_network_heatmap()
    plotly_dendro_heat(data_TOM)
  }, error = function(err) {
    return(NULL)
  })
})

## Get dat for module/Dendro network heatmap
get_dendro_network_heatmap <- reactive({
  dissTOM <- get_TOM_manual()
  geneTree <- hclust(as.dist(dissTOM), method = "average")
  
  data_TOM <- dissTOM^7
  diag(data_TOM) <- NA
  data_TOM <- data_TOM[, geneTree$order]
  data_TOM <- data_TOM[rev(geneTree$order),]
  data_TOM
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

output[["wgcna_module_trait_info"]] <- renderUI({
  infoText <- "Module and trait relation"
  informationBox(infoText)
})

output[["wgcna_network_heat_info"]] <- renderUI({
  infoText <- "Dendrogram heatmap network"
  informationBox(infoText)
})

