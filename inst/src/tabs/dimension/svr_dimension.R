
## Variance PCA
output[["variance_pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    variancePcaPlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## PCA
output[["pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    pcaPlot(inUse_normDge,
            input$group_pca,
            input$set_pca_pc1,
            input$set_pca_pc2)
  }, error = function(err) {
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

## tSNE
output[["dim_tsne"]] <- renderPlotly({
  tryCatch({
    checkReload()
    tsnePlot(inUse_normDge, input$group_dim_tsne)
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
    multidimensionalScalingPlot(inUse_normDge, input$group_norm_mds, "norm_mds")
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
    plotly_dendrogram(sampleTree, inUse_normDge$samples[[input$color_dendro]], NA)
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
