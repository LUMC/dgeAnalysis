
## PCA
color_mapping_global = NULL
last_group_pca = reactiveVal(NULL)
output[["pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$group_pca)) {
      break
    }
    
    ## Get input data
    plot_data <- pca_data(inUse_normDge)
    text <- 'paste("Sample:", sample)'

    ## Create data point colors
    selected_col <- input$group_pca
    if(selected_col == "sample") {
      unique_values <- unique(rownames(data_samples()))
    } else {
      unique_values <- unique(data_samples()[[selected_col]])
    }
    
    ## Update amount of groups when group_pca is changed
    if (!identical(unique_values, last_group_pca())) {
      default_colors <- scales::hue_pal()(length(unique_values))
      new_color_mapping <- setNames(default_colors, unique_values)
      color_mapping_global <<- new_color_mapping
      last_group_pca(unique_values)
      updateColourInput(session, inputId = "selected_color", value = "white")
      } else {
        new_color_mapping <- color_mapping_global
      }
    
    if (!is.null(input$selected_color)) {
      new_color_mapping[input$color_groups] <- input$selected_color
    }

    color_mapping_global <<- new_color_mapping

    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        x = input$set_pca_pc1,
        y = input$set_pca_pc2,
        text = text,
        group = input$group_pca,
        color_mapping = new_color_mapping,
        size = 5,
        key = "sample",
        title = "PCA",
        xlab = paste0(input$set_pca_pc1, " (", plot_data$percent[as.numeric(gsub("PC", "", input$set_pca_pc1))], "%)"),
        ylab = paste0(input$set_pca_pc2, " (", plot_data$percent[as.numeric(gsub("PC", "", input$set_pca_pc2))], "%)")
      ),
      source = "pca",
      tooltip = "text"
    ) %>% layout(dragmode = "select", clickmode = "event+select")
  }, error = function(err) {
    print(err)
    return(NULL)
  })
})

## Select groups for custom colorization
observe({
  tryCatch({
    checkReload()
    selected_col <- input$group_pca
    if (selected_col == "sample") {
      unique_values <- unique(rownames(data_samples()))
    } else {
      unique_values <- unique(data_samples()[[selected_col]])
    }
    updateSelectizeInput(
      inputId = "color_groups",
      choices = unique_values,
      server = TRUE,
      selected = 1
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
  }, error = function(err)  {
    return(NULL)
  })
})

output[["color_picker"]] <- renderUI ({
  tryCatch({
    if (!is.null(input$color_groups) && length(input$color_groups) > 0) {
      colourInput(
        inputId = "selected_color",
        label = "Select Color:",
        value = input$selected_color,
        allowTransparent = TRUE,
        palette = "square",
      )
    }
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
    if (is.null(s)) {
      throw()
    }
    
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
    text <- 'paste("PC:", pc,
                  "\nPercentage:", percent)'
    
    ## Create plot
    ggplotly(
      bar_plot(
        df = plot_data,
        x = "pc",
        y = "percent",
        text = text,
        title = "PCA Scree plot",
        xlab = "Principal component",
        ylab = "Percentage"
      ),
      tooltip = "text"
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
    text <- 'paste("Sample:", sample)'
    
    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        x = "V1",
        y = "V2",
        text = text,
        group = input$group_dim_tsne,
        size = 5,
        key = "sample",
        title = "tSNE",
        xlab = "tSNE 1",
        ylab = "tSNE 2"
      ),
      source = "tsne",
      tooltip = "text"
    ) %>% layout(dragmode = "select", clickmode = "event+select")
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
    if (is.null(s)) {
      throw()
    }
    
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
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
    text <- 'paste("Sample:", sample)'
    
    ## Create plot
    ggplotly(
      scatter_plot(
        df = plot_data,
        x = "x",
        y = "y",
        text = text,
        group = input$group_norm_mds,
        size = 5,
        key = "sample",
        title = "MDS Plot",
        xlab = "MDS 1",
        ylab = "MDS 2"
      ),
      source = "norm_mds",
      tooltip = "text"
    ) %>% layout(dragmode = "select", clickmode = "event+select")
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
    if (is.null(s)) {
      throw()
    }
    
    DT::datatable(data_samples()[unlist(s$key), , drop = FALSE], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
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
    text <- 'paste("Sample:", sample)'
    
    ## Create plot
    ggplotly(
      dendro_plot(
        df = plot_data,
        text = text,
        group = input$color_dendro,
        title = "Dendrogram",
        xlab = "",
        ylab = "Height"
      ),
      tooltip = "text"
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
