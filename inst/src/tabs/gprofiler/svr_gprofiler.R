
## Enrichment table from all term data
output[["enrich_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    colnames(enrich) <-
      c(
        "source",
        "enrichedTerm",
        "P.Value",
        "termSize",
        "querySize",
        "geneCount",
        "significant",
        "genes"
      )
    DT::datatable(enrich, options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Show the default gprofiler plot
output[["enrich_gprofiler"]] <- renderPlotly({
  tryCatch({
    checkReload()
    gostplot(inUse_enrich, capped = FALSE, interactive = TRUE)
  }, error = function(err) {
    return(NULL)
  })
})

## Get a dataframe to use in plots from enrich object
clean_enrich <- reactive({
  tryCatch({
    checkReload()
    enrich_processed <- inUse_enrich$result
    enrich_processed <- enrich_processed[c("source", "term_name", "p_value", "term_size", "query_size", "intersection_size", "significant", "intersection")]
    rownames(enrich_processed) <- inUse_enrich$result$term_id
    enrich_processed <- enrich_processed[order(enrich_processed$p_value),]
    enrich_processed
  }, error = function(err) {
    return(NULL)
  })
})

## create barplot with gprofilers enrichment terms
output[["enrich_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    enrich <- clean_enrich()
    plot_data <- enrich_bar(enrich, input$terms_slider)
    text <- 'paste("Source:", source,
                  "\nTerm:", term_name,
                  "\nGenes:", intersection_size,
                  "\nP-Value:", round(p_value, 5))'
    
    ## Create plot
    ggplotly(
      bar_plot(
        df = plot_data,
        x = "intersection_size",
        y = "term_name",
        text = text,
        fill = "p_value",
        title = "Enrichment barplot",
        xlab = "Number of genes",
        ylab = ""
      ),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## create barplot with the number of DE genes in term
output[["enrich_DEbarplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    enrich <- clean_enrich()
    plot_data <- enrich_barDE(enrich, input$DEterms_slider, inUse_deTab)
    text <- 'paste("Term:", name,
                  "\nGenes:", abs(values),
                  "\nRegulation:", ind)'
    
    ## Create plot
    ggplotly(
      bar_plot(
        df = plot_data,
        x = "values",
        y = "name",
        text = text,
        fill = "ind",
        title = "DE genes in terms",
        xlab = "Number of genes",
        ylab = ""
      ),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with gprofiler enrichment terms
output[["cnet_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$cnet_slider,
                                 input$select_pathway)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    plot_data <- cnet_data(inUse_deTab, graphData, (input$cnet_slider + length(input$select_pathway)))
    text <- 'paste("Node:", genes,
                  "\nLog2FC:", round(fc, 2))'
    
    ## Create plot
    ggplotly(
      network_plot(
        df = plot_data,
        text = text,
        title = "Gene-Concept Network",
        label1 = input$cnet_annoP,
        label2 = input$cnet_annoG
      ),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to cnet plot
output[["cnet_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- clean_enrich()
    selectInput(
      inputId = "select_pathway",
      label = "Add specific pathway:",
      multiple = TRUE,
      choices = c("Click to add pathway" = "", enrich$term_name)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$cnet_slider,
                                 input$select_pathway)
    
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## create heatmap with gprofiler input
output[["heat_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Get input data
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$heat_slider,
                                 input$select_heat)
    plot_data <- heat_terms(geneSets, inUse_deTab)
    text <- 'paste("Term:", stringr::str_wrap(categoryID, 50),
                  "\nGene:", Gene,
                  "\nAverage Log2FC:", round(avgLog2FC, 2))'
    
    ## Create plot
    ggplotly(
      heatmap_plot(
        df = plot_data,
        x = "categoryID",
        y = "Gene",
        text = text,
        group = "none",
        fill = "avgLog2FC",
        title = "Genes in pathway",
        xlab = "",
        ylab = ""
      ),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Select a specific pathway to add to heatmap
output[["heat_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- clean_enrich()
    selectInput(
      inputId = "select_heat",
      label = "Add specific pathway:",
      multiple = TRUE,
      choices = c("Click to add pathway" = "", enrich$term_name)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Table with genes in corresponding heatmap plot
output[["heat_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$heat_slider,
                                 input$select_heat)
    
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## INFORMATION BOXES

output[["gProfiler2_plot_info"]] <- renderUI({
  infoText <-
    "The gProfiler 2 manhattan plot shows the terms colored by source (database).
        Each term is represented by a circle. The size of a circle represents the term
        size (number of genes). The y-axis shows the adjusted p-value in a (negative)
        log10 scale."
  informationBox(infoText)
})

output[["enrich_barplot_info"]] <- renderUI({
  infoText <-
    "The bar graph shows a sorted list of the most enriched terms. The bar chart is sorted based on
        p-value. The colors of the bars are also generated based on p-value. on the X-axis,
        the amount of genes linked to the pathway is displayed."
  informationBox(infoText)
})

output[["enrich_DEbarplot_info"]] <- renderUI({
  infoText <-
    "The bar graph shows a sorted list of the most enriched terms. The bar chart is sorted based on
        p-value. The plot shows the number of DE genes (up or down) regulated in an enrichment term."
  informationBox(infoText)
})

output[["enrich_cnet_info"]] <- renderUI({
  infoText <-
    "The concept network can show which genes are involved in the most significant terms. The 
        most enriched terms along with all associated genes are collected and displayed in a
        network plot. Some pathways may contain some matching genes. These genes will also
        connected. The color given to genes is based on the log-fold change determined after the
        expression analysis. Ultimately, this plot shows the connection of genes between the most
        important road."
  informationBox(infoText)
})

output[["enrich_heat_info"]] <- renderUI({
  infoText <-
    "The heatmap visualizes pathways and the associated genes. The genes are sorted based on:
        Log2FC. The paths are sorted by the number of gene maths among other paths, with paths listed with
        most gene matches on the left. This allows genes present in pathways to be compared on sight."
  informationBox(infoText)
})
