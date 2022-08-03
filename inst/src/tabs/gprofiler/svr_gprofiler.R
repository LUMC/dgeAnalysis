
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
    
    enrich <- clean_enrich()
    enrich <- na.omit(enrich[0:input$terms_slider, ])
    enrich$term_name <- factor(enrich$term_name,
                               levels = unique(enrich$term_name)[order(enrich$p_value,
                                                                       enrich$term_name,
                                                                       decreasing = TRUE)])
    
    bar_plot(
      df = enrich,
      x = "intersection_size",
      y = "term_name",
      colorbar = TRUE,
      fill = "p_value",
      title = "Enrichment barplot",
      xlab = "Number of genes",
      ylab = ""
    )
  }, error = function(err) {
    return(NULL)
  })
})

## create barplot with the number of DE genes in term
output[["enrich_DEbarplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    
    enrich <- na.omit(enrich[0:input$DEterms_slider,])
    enrich$term_name <- factor(enrich$term_name,
                               levels = unique(enrich$term_name)[order(enrich$p_value,
                                                                       enrich$term_name,
                                                                       decreasing = TRUE)])
    enrich[c("down", "not_sign", "up")] <- NA
    for (row in 1:nrow(enrich)) {
      genes <- unlist(strsplit(enrich[row, ]$intersection, split = ","))
      de <- table(deTab$DE[rownames(deTab) %in% genes])
      names(de) <- c("down", "not_sign", "up")[match(names(de), c(-1, 0, 1))]
      enrich[row, ][names(de)] <- de
    }
    
    plot_data <- enrich[c("term_name", "down", "up")]
    plot_data$down <- plot_data$down * -1
    plot_data <- stack(plot_data)
    plot_data$name <- enrich$term_name
    
    bar_plot(
      df = plot_data,
      x = "values",
      y = "name",
      fill = "ind",
      title = "DE genes in terms",
      xlab = "Number of genes",
      ylab = ""
    )
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with gprofiler enrichment terms
output[["cnet_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 5, #input$cnet_slider,
                                 NULL) #input$select_pathway)
    
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    set.seed(1234)
    layout <- as.data.frame(layout.kamada.kawai(graphData))
    
    ## Fix layout
    layout <- as.data.frame(layout)
    layout$genes <- names(V(graphData))
    
    ## Expand graph
    layout[1:2] <- layout[1:2] * 10

    conns <- get.data.frame(graphData)
    conns$from.x <- layout$V1[match(conns$from, layout$genes)]
    conns$from.y <- layout$V2[match(conns$from, layout$genes)]
    conns$to.x <- layout$V1[match(conns$to, layout$genes)]
    conns$to.y <- layout$V2[match(conns$to, layout$genes)]
    
    term_layout <- layout[1:5, ]
    gene_layout <- layout[6:nrow(layout), ]
    
    gene_layout$fc <- inUse_deTab$avgLog2FC[match(gene_layout$genes, rownames(inUse_deTab))]
    
    plotlyGraph(
      graphData,
      "Gene-Concept Network",
      "Log2FC",
      length(geneSets),
      input$cnet_annoP,
      input$cnet_annoG
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
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$heat_slider,
                                 input$select_heat)
    
    genelist <- list2df(geneSets)
    genelist <- merge(
      genelist,
      inUse_deTab[c("avgLog2FC"), drop = FALSE],
      by.x = "Gene",
      by.y = 0,
      all.x = TRUE
    )
    genelist <-
      merge(genelist, rev(sort(table(genelist$Gene))), by.x = "Gene", by.y = "Var1")
    if (length(rev(sort(table(
      genelist$categoryID
    )))) == 1) {
      genelist <- merge(genelist, rev(sort(table(
        genelist$categoryID
      ))), by.x = "categoryID", by.y = "row.names")
      colnames(genelist)[colnames(genelist) %in% c("Freq", "y")] <-
        c("Freq.x", "Freq.y")
    } else {
      genelist <- merge(genelist, rev(sort(table(
        genelist$categoryID
      ))), by.x = "categoryID", by.y = "Var1")
    }
    
    for (pathway in unique(genelist$categoryID)) {
      entrezID <- genelist$Gene[genelist$categoryID == pathway]
      entrez_matches <-
        count(genelist$Gene[genelist$categoryID != pathway] %in% entrezID)
      genelist$match[genelist$categoryID == pathway] <-
        entrez_matches
    }
    
    genelist <- genelist[order(-genelist$match, -genelist$Freq.y, genelist$avgLog2FC), ]
    genelist$Gene <- factor(genelist$Gene, levels = unique(genelist$Gene))
    genelist$categoryID <- factor(genelist$categoryID, levels = unique(genelist$categoryID))
    
    heatmap_plot(
      df = genelist,
      x = "categoryID",
      y = "Gene",
      group = "none",
      fill = "avgLog2FC",
      title = "Genes in pathway",
      xlab = "",
      ylab = ""
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
