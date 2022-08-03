
## PCA
output[["pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    tdge <- t(inUse_normDge$counts)
    tdge[!is.finite(tdge)] <- 0
    pca <- prcomp(tdge, center = TRUE)
    percent <- data.frame(summary(pca)$importance[2, ])
    colnames(percent) <- "percent"
    
    pca <- data.frame(scale(tdge, center = T, scale = F) %*% pca$rotation)
    pca$sample <- rownames(pca)
    pca$group <- inUse_normDge$samples[, input$group_pca]
    
    scatter_plot(
      df = pca,
      size = 4,
      source = "pca",
      key = "sample",
      x = input$set_pca_pc1,
      y = input$set_pca_pc2,
      group = "group",
      title = "PCA",
      xlab = paste0(input$set_pca_pc1, " (", round(percent[input$set_pca_pc1,] * 100, 2), "%)"),
      ylab = paste0(input$set_pca_pc2, " (", round(percent[input$set_pca_pc2,] * 100, 2), "%)")
    )
  }, error = function(err) {
    print(err)
    return(NULL)
  })
})

## Set color of PCA
output[["group_pca"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "group_pca",
      label = "Color by:",
      choices = colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set PCs for PCA
output[["setpc_pca"]] <- renderUI({
  tryCatch({
    all_pc <- sprintf("PC%s", seq(1:ncol(inUse_normDge$counts)))
    tagList(
      selectInput(
        inputId = "set_pca_pc1",
        label = "Select X-axis PC:",
        choices = all_pc
      ),
      selectInput(
        inputId = "set_pca_pc2",
        label = "Select Y-axis PC:",
        choices = all_pc,
        selected = "PC2"
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points samples_pca
output[["selected_pca"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "pca")
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Variance PCA
output[["variance_pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    tdge <- t(inUse_normDge$counts)
    tdge[!is.finite(tdge)] <- 0
    pca <- prcomp(tdge, center = TRUE)
    percent <- data.frame(
      pc = names(summary(pca)$importance[2, ]),
      percent = summary(pca)$importance[2, ] * 100,
      color = "color"
    )
    
    bar_plot(
      df = percent,
      x = "pc",
      y = "percent",
      fill = "color",
      title = "PCA Scree plot",
      xlab = "Principal component",
      ylab = "Percentage"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## tSNE
output[["dim_tsne"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    set.seed(1234)
    
    perplexity <- 30
    while (perplexity > 0) {
      try({
        tsne_model <- Rtsne(
          t(inUse_normDge$counts),
          perplexity = perplexity,
          check_duplicates = FALSE,
          normalize = FALSE
        )
        break
      }, silent = TRUE)
      perplexity <- perplexity - 1
    }
    
    tsne_data <- as.data.frame(tsne_model$Y)
    rownames(tsne_data) <- colnames(inUse_normDge$counts)
    tsne_data$sample <- rownames(tsne_data)
    tsne_data$group <- inUse_normDge$samples[[input$group_dim_tsne]]
    
    scatter_plot(
      df = tsne_data,
      size = 4,
      source = "tsne",
      key = "sample",
      x = "V1",
      y = "V2",
      group = "group",
      title = "tSNE",
      xlab = "tSNE 1",
      ylab = "tSNE 2"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of tSNE
output[["group_dim_tsne"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "group_dim_tsne",
      label = "Color by:",
      choices = colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points tSNE
output[["selected_dim_tsne"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "tsne")
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Normalized mds
output[["norm_un_cluster"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    logFC <- plotMDS(inUse_normDge$counts, ndim = ncol(inUse_normDge) - 1)
    for_plots <- data.frame(logFC[c("x", "y")])
    for_plots$sample <- rownames(logFC$distance.matrix.squared)
    for_plots$group <- inUse_normDge$samples[, input$group_norm_mds]
    
    scatter_plot(
      df = for_plots,
      size = 4,
      source = "norm_mds",
      key = "sample",
      x = "x",
      y = "y",
      group = "group",
      title = "MDS Plot",
      xlab = "MDS 1",
      ylab = "MDS 2"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of mds
output[["group_norm_mds"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "group_norm_mds",
      label = "Color by:",
      choices = colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points norm_un_cluster
output[["selected_norm_mds"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "norm_mds")
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Sample dendrogram
output[["dim_dendro"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    sampleTree <- hclust(dist(t(inUse_normDge$counts)), method = "average")
    dendro_data <- get_dendrogram_data(sampleTree)
    dendro_data$group <- NA
    dendro_data$group[dendro_data$label != ""] <- as.character(inUse_normDge$samples[[input$color_dendro]])
    print(dendro_data)
    
    dendro_plot(
      df = dendro_data,
      group = "group",
      title = "Dendrogram",
      xlab = "",
      ylab = "Height"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of sample dendrogram
output[["color_dendro"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "color_dendro",
      label = "Color by:",
      choices = colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## PC values per gene (table)
pc_gene_table <- reactive({
  tryCatch({
    tdge <- t(inUse_normDge$counts)
    tdge[!is.finite(tdge)] <- 0
    pca <- prcomp(tdge, center = TRUE)
    pca <- pca$rotation
    pca
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["variance_pca_info"]] <- renderUI({
  infoText <-
    "This chart shows the variances of the PCA (Principal Components Analysis) components.
        A scree plot shows the 'eigenvalues' from the PCA and can be used to determine how many components
        can be kept for the PCA analysis."
  informationBox(infoText)
})

output[["pca_info"]] <- renderUI({
  infoText <-
    "The 2D PCA plot shows the samples based on the two components. A PCA plot shows important
        information from a multivariate data table and displays the result as new variables (main components)."
  informationBox(infoText)
})

output[["norm_un_cluster_info"]] <- renderUI({
  infoText <-
    "This MDS plot (multidimensional scaling plot) can be viewed as a 2D plot with
        calculations of two dimensions. With the MDS plotting distances between samples, samples
        shown, based on similarities and differences."
  informationBox(infoText)
})

output[["dim_tsne_info"]] <- renderUI({
  infoText <-
    "t-distributed stochastic neighbor embedding (t-SNE) is a statistical method
        for visualizing high-dimensional data by giving each datapoint a location in a
        multi-dimensional map."
  informationBox(infoText)
})

output[["dendro_info"]] <- renderUI({
  infoText <- "Hierarchical clustering of samples using normalized data. Log2CPM
        values are used to create the dendrogram. "
  informationBox(infoText)
})
