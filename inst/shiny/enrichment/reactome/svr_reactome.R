
## get and create reactome enrichment
get_reactome <- reactive({
  tryCatch({
    checkReload()
    organism <- get_organismID(inUse_deTab)
    org <- list(ENS="human",
                ENSMUS="mouse")
    organism <- org[[organism]]
    if (isTRUE(input$choose_reactome)) {
      enrich <- ReactomePA::enrichPathway(inUse_deTab$entrez[inUse_deTab$DE!=0], organism=organism, pvalueCutoff=0.05)
    } else {
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      enrich <- ReactomePA::gsePathway(geneList, organism=organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
    }
    enrich
  }, error = function(err) {
    return(NULL)
  })
})

## create reactome enrichment table
output[["reactome_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
    DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## create barplot with reactome enrichment terms
output[["reactome_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    enrichBarplot(enrich, input$bar_reactome_slider, input$bar_reactome_value)
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with reactome input
output[["cnet_reactome_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    
    geneSets <- extract_geneSets(enrich, input$cnet_reactome_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_reactome_slider)
    plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_reactome_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    
    geneSets <- extract_geneSets(enrich, input$cnet_reactome_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_reactome_slider)
    DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## create reactome plot with all pathways
output[["gsea_reactome_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    graphData <- emap_plotly(enrich)
    plotlyGraph(graphData, "Reactome", "P-Value", 0)
  }, error = function(err) {
    return(NULL)
  })
})

## create reactome plot of specific pathway by selection
output[["reactome_pathway"]] <- renderPlotly({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_click", source = "Reactome")
    
    graphData <- viewPathwayPlot(inUse_deTab, 'reactome', s$key)
    plotlyGraph(graphData, s$key, "Log2FC", 0)
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of specific pathway by selection
output[["reactome_pathway_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    s <- event_data(event = "plotly_click", source = "Reactome")
    
    graphData <- viewPathwayPlot(inUse_deTab, 'reactome', s$key)
    DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Show dropdown of all found reactome pathways
output[["select_reactome_pathway"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    selectInput("reactome_select", "Select a pathway:",
                enrich$Description
    )
  }, error = function(err) {
    return(NULL)
  })
})

## show selected pathway from kegg
output[["pathway_from_reactome"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    getpathway <- rownames(enrich)[enrich$Description %in% input$reactome_select]
    getFromReactome <- includeHTML(paste("https://reactome.org/ContentService/exporter/diagram/", getpathway, ".svg?title=false", sep=""))
    getFromReactome <- HTML(paste('<a href="https://reactome.org/PathwayBrowser/#/', getpathway, '" target="_blank">\n', getFromReactome, '\n</a>', sep=""))
    getFromReactome
  }, error = function(err) {
    return(NULL)
  })
})
