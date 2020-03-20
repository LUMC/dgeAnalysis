
## Variance PCA
output[["variance_pca"]] <- renderPlotly({
  tryCatch({
    checkReload()
    variancePcaPlot(inUse_normDge)
  }, error = function(err) {
    return(NULL)
  })
})

## samples PCA 2D
output[["samples_pca_2d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    samplePca2dPlot(inUse_normDge, input$group_pca2d, input$set_pca2d_pc1, input$set_pca2d_pc2)
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of PCA 2d
output[["group_pca2d"]] <- renderUI({
  tryCatch({
    selectInput("group_pca2d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of PCA 2d
output[["group_pca2d"]] <- renderUI({
  tryCatch({
    selectInput("group_pca2d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set PCs for PCA 2d
output[["setpc_pca2d"]] <- renderUI({
  tryCatch({
    all_pc <- sprintf("PC%s",seq(1:ncol(inUse_normDge$counts)))
    tagList(
      selectInput("set_pca2d_pc1", "Select X-axis PC:",
                  all_pc
      ),
      selectInput("set_pca2d_pc2", "Select Y-axis PC:",
                  all_pc,
                  selected = "PC2"
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Selected data points samples_pca_2d
output[["selected_pca"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_selected", source = "pca_pca2d")
    if(is.null(s)){s <- ""}
    DT::datatable(data_samples()[s$key,], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## samples PCA 3D
output[["samples_pca_3d"]] <- renderPlotly({
  tryCatch({
    checkReload()
    samplePca3dPlot(inUse_normDge, input$group_pca3d, input$set_pca3d_pc1, input$set_pca3d_pc2, input$set_pca3d_pc3)
  }, error = function(err) {
    return(NULL)
  })
})

## Set color of PCA 3d
output[["group_pca3d"]] <- renderUI({
  tryCatch({
    selectInput("group_pca3d", "Color by:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Set PCs for PCA 3d
output[["setpc_pca3d"]] <- renderUI({
  tryCatch({
    all_pc <- sprintf("PC%s",seq(1:ncol(inUse_normDge$counts)))
    tagList(
      selectInput("set_pca3d_pc1", "Select X-axis PC:",
                  all_pc
      ),
      selectInput("set_pca3d_pc2", "Select Y-axis PC:",
                  all_pc,
                  selected = "PC2"
      ),
      selectInput("set_pca3d_pc3", "Select Z-axis PC:",
                  all_pc,
                  selected = "PC3"
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["variance_pca_info"]] <- renderUI({
  infoText <- "This plot shows the variances of the PCA (Principal Components Analysis) components.
        A scree plot shows the 'eigenvalues' from the PCA and can be used to decide how many components
        can be kept for the PCA analysis."
  informationBox(infoText)
})

output[["samples_pca_2d_info"]] <- renderUI({
  infoText <- "The 2D PCA plot shows the samples based on the two components. A PCA plot shows important
        information from a multivariate data table and shows the result as new variables (Principal Components)."
  informationBox(infoText)
})

output[["samples_pca_3d_info"]] <- renderUI({
  infoText <- "The 3D PCA plot shows the samples based on the three components. A PCA plot shows important
        information from a multivariate data table and shows the result as new variables (Principal Components)."
  informationBox(infoText)
})
