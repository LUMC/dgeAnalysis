
## get and create kegg enrichment
get_kegg <- reactive({
  tryCatch({
    checkReload()
    organism <- get_organismID(inUse_deTab)
    org <- list(ENS="hsa",
                ENSMUS="mmu")
    organism <- org[[organism]]
    if (isTRUE(input$choose_kegg)) {
      enrich <- clusterProfiler::enrichKEGG(inUse_deTab$entrez[inUse_deTab$DE!=0], organism=organism, pvalueCutoff=0.05)
    } else {
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      enrich <- clusterProfiler::gseKEGG(geneList, organism=organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
    }
    enrich
  }, error = function(err) {
    return(NULL)
  })
})

## create kegg enrichment table
output[["kegg_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
    DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## create barplot with kegg enrichment terms
output[["kegg_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    enrichBarplot(enrich, input$bar_kegg_slider, input$bar_kegg_value)
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with kegg input
output[["cnet_kegg_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    
    geneSets <- extract_geneSets(enrich, input$cnet_kegg_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_kegg_slider)
    plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_kegg_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    
    geneSets <- extract_geneSets(enrich, input$cnet_kegg_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_kegg_slider)
    DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## create kegg plot with all pathways
output[["gsea_kegg_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    graphData <- emap_plotly(enrich)
    plotlyGraph(graphData, "KEGG", "P-Value", 0)
  }, error = function(err) {
    return(NULL)
  })
})

## create kegg plot of specific pathway by selection
output[["kegg_pathway"]] <- renderPlotly({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_click", source = "KEGG")
    
    graphData <- viewPathwayPlot(inUse_deTab, 'kegg', s$key)
    plotlyGraph(graphData, s$key, "Log2FC", 0)
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of specific pathway by selection
output[["kegg_pathway_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_click", source = "KEGG")
    
    graphData <- viewPathwayPlot(inUse_deTab, 'kegg', s$key)
    DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Show dropdown of all found kegg pathways
output[["select_kegg_pathway"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    selectInput("kegg_select", "Select a pathway:",
                enrich$Description
    )
  }, error = function(err) {
    return(NULL)
  })
})

## show selected pathway from kegg
output[["pathway_from_kegg"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    getpathway <- rownames(enrich)[enrich$Description %in% input$kegg_select]
    getFromKegg <- includeHTML(paste("https://www.genome.jp/kegg-bin/show_pathway?", getpathway, sep=""))
    getFromKegg <- gsub('src="', 'src="https://www.genome.jp', getFromKegg)
    getFromKegg <- gsub('href="', 'href="https://www.genome.jp', getFromKegg)
    getFromKegg <- gsub("<head.*</head>", "", getFromKegg)
    getFromKegg <- gsub("<table.*</table>", "", getFromKegg)
    getFromKegg
  }, error = function(err) {
    return(NULL)
  })
})
