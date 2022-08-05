
## PCA
output[["pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$group_pca)) {
      break
    }
    
    ## Get input data
    plot_data <- pca_data(inUse_normDge)
    
    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        size = 5,
        key = "sample",
        x = input$set_pca_pc1,
        y = input$set_pca_pc2,
        group = input$group_pca,
        title = "PCA",
        xlab = paste0(input$set_pca_pc1, " (", plot_data$percent[as.numeric(gsub("PC", "", input$set_pca_pc1))], "%)"),
        ylab = paste0(input$set_pca_pc2, " (", plot_data$percent[as.numeric(gsub("PC", "", input$set_pca_pc2))], "%)")
      ),
      source = "pca"
    )
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
      choices = c("Samples" = "sample", colnames(data_samples()))
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
    
    ## Get input data
    plot_data <- pca_data(inUse_normDge)
    
    ## Create plot
    bar_plot(
      df = plot_data,
      x = "pc",
      y = "percent",
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
    
    ## Only plot if UI is loaded
    if(is.null(input$group_dim_tsne)) {
      break
    }
    
    ## Get input data
    plot_data <- tsne_data(inUse_normDge)
    
    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        size = 5,
        key = "sample",
        x = "V1",
        y = "V2",
        group = input$group_dim_tsne,
        title = "tSNE",
        xlab = "tSNE 1",
        ylab = "tSNE 2"
      ),
      source = "tsne"
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
      choices = c("Samples" = "sample", colnames(data_samples()))
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
    
    ## Only plot if UI is loaded
    if(is.null(input$group_norm_mds)) {
      break
    }
    
    ## Get input data
    plot_data <- mds_clust(inUse_normDge)
    
    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        size = 5,
        key = "sample",
        x = "x",
        y = "y",
        group = input$group_norm_mds,
        title = "MDS Plot",
        xlab = "MDS 1",
        ylab = "MDS 2"
      ),
      source = "norm_mds"
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
      choices = c("Samples" = "sample", colnames(data_samples()))
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
    
    ## Only plot if UI is loaded
    if(is.null(input$color_dendro)) {
      break
    }
    
    ## Get input data
    plot_data <- dendro_data(inUse_normDge)
    
    ## Create plot
    dendro_plot(
      df = plot_data,
      group = input$color_dendro,
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
      choices = c("Samples" = "sample", colnames(data_samples()))
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
